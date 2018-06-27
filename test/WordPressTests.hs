{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}

module WordPressTests where

import           Control.Lens                  (filtered, to, (%~), (&), (^.),
                                                (^..))
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.Aeson                    (encode)
import           Data.Bool                     (bool)
import           Data.ByteString               (ByteString)
import           Data.Dependent.Map            (DMap)
import qualified Data.Dependent.Map            as DM
import           Data.Dependent.Sum            (DSum (..), (==>))
import           Data.Functor.Classes          (Eq1)
import           Data.Functor.Identity         (Identity (..))
import qualified Data.Text                     as T
import           Data.Time                     (LocalTime (LocalTime),
                                                diffUTCTime, fromGregorian,
                                                getCurrentTime, localTimeToUTC,
                                                nominalDay, secondsToDiffTime,
                                                timeToTimeOfDay, utc,
                                                utcToLocalTime)
import           Servant.API                   (BasicAuthData (BasicAuthData))
import           Servant.Client                (runClientM)

import           Hedgehog                      (Callback (..),
                                                Command (Command),
                                                Concrete (Concrete),
                                                HTraversable (htraverse),
                                                MonadGen, MonadTest, Property,
                                                Symbolic, Var (Var), annotate,
                                                annotateShow, assert, concrete,
                                                eval, evalEither, evalIO,
                                                executeParallel,
                                                executeSequential, failure,
                                                forAll, property, success, test,
                                                withRetries, withShrinks,
                                                withTests, (===))
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.Hedgehog           (testProperty)

import           Data.GADT.Aeson               (EqViaKey (..))
import           Web.WordPress.API             (createPost, getPost, listPosts)
import           Web.WordPress.Types.ListPosts (ListPostsKey (..))
import           Web.WordPress.Types.Post      (PostKey (..), PostMap,
                                                RESTContext (Create),
                                                Renderable, Status (..),
                                                mkCreatePR, mkCreateR)

import           Types                         (Env (..), HasIdentityPosts (..),
                                                HasPosts (..),
                                                HasStatePosts (..),
                                                StatePost (..), StatePosts (..),
                                                WPState (WPState),
                                                hasKeyMatchingPredicate,
                                                smooshStatePost)

wordpressTests
  :: Env
  -> TestTree
wordpressTests env =
  testGroup "wordpress" [
    testProperty "sequential" $ propWordpress env
  , testProperty "parallel" $ propWordpressParallel env
  ]

propWordpress
  :: Env
  -> Property
propWordpress env@Env{..} =
  property $ do
  now <- liftIO (utcToLocalTime utc <$> getCurrentTime)
  let
    commands = ($ env) <$> [cListPosts, cCreatePost now, cGetPost]
    initialState = WPState . StatePosts $ []
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialState commands

  evalIO reset
  executeSequential initialState actions

propWordpressParallel
  :: Env
  -> Property
propWordpressParallel env@Env{..} =
  withRetries 10 . property $ do
  now <- liftIO (utcToLocalTime utc <$> getCurrentTime)
  let
    commands = ($ env) <$> [cListPosts, cCreatePost now, cGetPost]
    initialState = WPState . StatePosts $ []
  actions <- forAll $
    Gen.parallel (Range.linear 1 200) (Range.linear 1 50) initialState commands

  test $ do
    evalIO reset
    executeParallel initialState actions


--------------------------------------------------------------------------------
-- LIST
--------------------------------------------------------------------------------
data ListPosts (v :: * -> *) =
  -- Two maps -- one for Vars and one for stuff we know ahead of time.
  ListPosts (DMap ListPostsKey v) (DMap ListPostsKey Identity)
  deriving (Show)

htraverseDmap
  :: (Applicative f)
  => (forall a. g a -> f (h a))
  -> DMap k g
  -> f (DMap k h)
htraverseDmap f =
  DM.traverseWithKey (const f)

deriving instance Eq1 v => Eq (ListPosts v)

instance HTraversable ListPosts where
  htraverse f (ListPosts dmv dmi) =
    ListPosts <$> htraverseDmap f dmv <*> pure dmi

cListPosts
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     , HasIdentityPosts state
     , HasStatePosts state
     )
  => Env
  -> Command n m state
cListPosts Env{..} =
  let
    gen = Just . genList
    exe (ListPosts dmv dmi) =
      let
        dmi' = DM.map (\(Concrete a) -> pure a) dmv
        dm = DM.union dmi' dmi
      in
        evalEither =<< liftIO (runClientM (listPosts dm) servantClient)
  in
    Command gen exe [
      Ensure $ \so _sn (ListPosts _ lpi) ps -> do
        annotateShow $ so ^. statePosts
        let
          lup :: a -> ListPostsKey a -> a
          lup def key = maybe def runIdentity $ DM.lookup key lpi
          pws = lup Publish ListPostsStatus
          perPage = lup 10 ListPostsPerPage
          eLength = length . take perPage $ postsWithStatus so pws
        eLength === length ps
    ]

genList
  :: ( MonadGen n
     , HasIdentityPosts state
     )
  => state Symbolic
  -> n (ListPosts v)
genList s = do
  status <- Gen.enumBounded
  perPage <- Gen.int (Range.linear 1 100)
  let
    numPosts = length $ postsWithStatus s status
    numPages = min 1 . (\(d,m) -> d + min m 1) $ numPosts `divMod` perPage
  page <- Gen.int (Range.linear 1 numPages)
  pure . ListPosts DM.empty $ DM.fromList [
      --ListPostsStatus ==> status
     ListPostsPerPage ==> perPage
    , ListPostsPage ==> page
    ]

postsWhere
  :: HasIdentityPosts state
  => state v
  -> (PostMap -> Bool)
  -> [PostMap]
postsWhere s p =
  s ^.. identityPosts . traverse . filtered p

postsWithStatus
  :: HasIdentityPosts state
  => state v
  -> Status
  -> [PostMap]
postsWithStatus s status =
  postsWhere s ((== Identity status) . (DM.! PostStatus))


--------------------------------------------------------------------------------
-- GET
--------------------------------------------------------------------------------
newtype GetPost (v :: * -> *) =
  GetPost (StatePost v)
  deriving (Show)

instance HTraversable GetPost where
  htraverse f (GetPost v)=
    GetPost <$> htraverse f v

cGetPost
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     , HasPosts state
     )
  => Env
  -> Command n m state
cGetPost env@Env{..} =
  let
    gen s =
      if s ^. posts & (not . null)
      then Just $ GetPost <$> (s ^. posts & Gen.element)
      else Nothing
    exe (GetPost (StatePost _ dmv)) = do
      postId <- eval . runIdentity $ concrete dmv DM.! PostId
      evalEither =<< liftIO (runClientM (getPost (auth env) postId) servantClient)
  in
    Command gen exe [
      Require $ \s (GetPost sp) ->
        (s ^.. posts . traverse . filtered (== sp)) & not . null
    , Ensure $ \_so _sn (GetPost sp) p -> do
        let
          pState = smooshStatePost sp

          sp' = DM.union pState p
          p' = DM.union p sp'
        annotateShow sp
        annotateShow p
        sp' === p'
    ]


--------------------------------------------------------------------------------
-- CREATE
--------------------------------------------------------------------------------
newtype CreatePost (v :: * -> *) =
  CreatePost PostMap
  deriving (Show)

instance HTraversable CreatePost where
  htraverse _ (CreatePost dmi) =
    pure (CreatePost dmi)

cCreatePost
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     , HasPosts state
     , HasIdentityPosts state
     )
  => LocalTime
  -> Env
  -> Command n m (state :: (* -> *) -> *)
cCreatePost now env@Env{..} =
  let
    gen = Just . genCreate now
    exe (CreatePost pm) = do
      annotateShow pm
      annotateShow $ encode pm
      r <- evalEither =<< liftIO (runClientM (createPost (auth env) pm) servantClient)
      annotateShow $ createToStatePost now (CreatePost pm) (Var (Concrete r))
      pure r
  in
    Command gen exe [
      Update $ \s cp o ->
        posts %~ (createToStatePost now cp o :) $ s
    -- , Ensure $ \_so _sn (CreatePost _pmi) _pmo ->
    --     undefined
    ]

createToStatePost ::
  LocalTime
  -> CreatePost v
  -> Var PostMap v
  -> StatePost v
createToStatePost now (CreatePost pm) pmo = do
  let
    pmi =
      foldr ($) pm [
        fixCreateStatus now
      , fixSlug
      ]
  StatePost pmi pmo

fixCreateStatus ::
  LocalTime
  -> PostMap
  -> PostMap
fixCreateStatus now pm =
  case DM.lookup PostStatus pm of
    Just is ->
      let
        haveDateLocal = DM.member PostDate pm
        fieldBeforeNow f = hasKeyMatchingPredicate f (< Identity now) pm
        fieldAfterNow f = hasKeyMatchingPredicate f (> Identity now) pm
        dateBeforeNow = (not haveDateLocal && fieldBeforeNow PostDateGmt) || fieldBeforeNow PostDate
        dateAfterNow = (not haveDateLocal && fieldAfterNow PostDateGmt) || fieldAfterNow PostDate
        hasStatus = ($ pm) . hasKeyMatchingPredicate PostStatus . (==) . Identity
        status =
          if | hasStatus Future && dateBeforeNow -> Identity Publish
             | hasStatus Publish && dateAfterNow -> Identity Future
             | otherwise -> is
      in
        DM.insert PostStatus status pm
    Nothing -> pm

fixSlug ::
  PostMap
  -> PostMap
fixSlug pm =
  case DM.lookup PostSlug pm of
    -- TODO: make a slug newtype and smart constructor to take care of all this
    Just s  -> DM.insert PostSlug (T.take 200 . T.toLower <$> s) pm
    Nothing -> pm

genCreate ::
  ( MonadGen n
  , HasIdentityPosts state
  )
  => LocalTime
  -> state (v :: * -> *)
  -> n (CreatePost v)
genCreate now s = do
  content <- genAlphaNum 1 500
  excerpt' <- T.take <$> Gen.int (Range.linear 1 (T.length content - 1)) <*> pure content
  status <- Gen.enumBounded
  let
    -- If something is marked for publishing in the future then make sure our date is at least a
    -- day away so its status doesn't change during testing.
    genDate =
      if status == Future
      then Gen.filter (not . withinADay now) genLocalTime
      else genLocalTime
    excerpt = bool content excerpt' (T.null excerpt')
    gensI = [
        PostDateGmt :=> genDate
        -- We don't want empty slugs because then WordPress defaults them and we can't be
        -- certain about when things should be equal without implementing their defaulting logic.
      , PostSlug :=> Gen.filter (not . existsPostWithSlug s) (genAlphaNum 1 300)
      , PostStatus :=> Gen.enumBounded
     -- , PostPassword
      , PostTitle :=> mkCreateR <$> genAlphaNum 1 30
      , PostContent :=> pure (mkCreatePR content)
      -- TODO: author should come from state. Start state has user with ID = 1.
      , PostAuthor :=> pure 1
      , PostExcerpt :=> pure (mkCreatePR excerpt)
     -- , PostFeaturedMedia
     -- , PostCommentStatus
     -- , PostPingStatus
     -- , PostFormat
     -- , PostMeta
     -- , PostSticky
     -- , PostTemplate
     -- , PostCategories
     -- , PostTags
      ]
    -- gensV = [
    --   ]
    f = fmap DM.fromList . traverse (\(kv :=> fv) -> fmap ((kv :=>) . pure) fv)
  CreatePost <$> f gensI

withinADay ::
  LocalTime
  -> LocalTime
  -> Bool
withinADay a b =
  let
    a' = localTimeToUTC utc a
    b' = localTimeToUTC utc b
  in
    abs (diffUTCTime a' b') < nominalDay

existsPostWithSlug ::
  HasIdentityPosts state
  => state v
  -> T.Text
  -> Bool
existsPostWithSlug s t =
  let
    isMatchingSlug = any (hasKeyMatchingPredicate PostSlug (== Identity t)) $ s ^. identityPosts
  in
    (not . null $ Identity t) && isMatchingSlug

genLocalTime
  :: MonadGen n
  => n LocalTime
genLocalTime =
  let
    gYear = Gen.int (Range.linearFrom 1900 1970 2500)
    gMonth = Gen.int (Range.linear 1 12)
    -- fromGregorian automatically trims to valid dates, so 2001-02-31 becomes 2001-02-28
    gDay = Gen.int (Range.linear 1 31)
    hToS = (* 3600)
    gSeconds = Gen.int (Range.linearFrom (hToS 12) 0 86400)
    gUTCTimeDay = fromGregorian . fromIntegral <$> gYear <*> gMonth <*> gDay
    gDiffTime = timeToTimeOfDay . secondsToDiffTime . fromIntegral <$> gSeconds
  in
    LocalTime <$> gUTCTimeDay <*> gDiffTime

genAlphaNum
  :: MonadGen n
  => Int
  -> Int
  -> n T.Text
genAlphaNum min' max' =
  Gen.text (Range.linear min' max') Gen.alphaNum

milliToMicro :: Integer -> Integer
milliToMicro = (* 1000)

auth
  :: Env
  -> BasicAuthData
auth Env{..} =
  BasicAuthData wpUser wpPassword
