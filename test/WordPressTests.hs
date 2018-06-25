{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}

module WordPressTests where

import           Control.Lens             (filtered, (%~), (&), (^.), (^..), to)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Aeson               (encode)
import           Data.Bool                (bool)
import           Data.ByteString          (ByteString)
import           Data.Dependent.Map       (DMap)
import qualified Data.Dependent.Map       as DM
import           Data.Dependent.Sum       (DSum (..), (==>))
import           Data.Functor             (void)
import           Data.Functor.Classes     (Eq1)
import           Data.Functor.Const       (Const (..))
import           Data.Functor.Identity    (Identity (..))
import qualified Data.Text                as T
import           Data.Time                (LocalTime (LocalTime), fromGregorian,
                                           secondsToDiffTime, timeToTimeOfDay)
import           Servant.API              (BasicAuthData (BasicAuthData))
import           Servant.Client           (runClientM)

import           Hedgehog                 (Callback (..), Command (Command), eval,
                                           Concrete (Concrete),
                                           HTraversable (htraverse), MonadGen,
                                           MonadTest, Property, Symbolic,
                                           Var (Var), annotateShow, assert,
                                           concrete, evalEither, evalIO,
                                           executeParallel, executeSequential,
                                           failure, forAll, property, success,
                                           test, withShrinks, withTests, (===), withRetries)
import qualified Hedgehog.Gen             as Gen
import qualified Hedgehog.Range           as Range
import           Test.Tasty               (TestTree, testGroup)
import           Test.Tasty.Hedgehog      (testProperty)

import           Web.WordPress.API        (createPost, getPost, listPosts)
import           Web.WordPress.Types.Post (EqViaKey (..), ListPostsKey (..),
                                           PostKey (..), PostMap,
                                           RESTContext (Create), Renderable,
                                           Status (..), mkCreatePR, mkCreateR)

import           Types                    (Env (..), HasPosts (..), StatePost (..), HasIdentityPosts (..),
                                           WPState (WPState), smooshStatePost, StatePosts (..))

wordpressTests
  :: Env
  -> TestTree
wordpressTests env =
  testGroup "wordpress" [
    testProperty "sequential" . withTests 10 $ propWordpress env
  , testProperty "parallel" $ propWordpressParallel env
  ]

propWordpress
  :: Env
  -> Property
propWordpress env@Env{..} =
  property $ do
  let
    commands = ($ env) <$> [cListPosts, cCreatePost, cGetPost]
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
  let
    commands = ($ env) <$> [cListPosts, cCreatePost, cGetPost]
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
     , HasPosts state
     , HasIdentityPosts state
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
      Ensure $ \so _sn _i ps ->
        length (postsWithStatus so Publish) === length ps
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
      ListPostsStatus ==> status
    , ListPostsPerPage ==> perPage
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
      Ensure $ \_so _sn (GetPost sp) p -> do
        let
          pState = smooshStatePost sp

          eqVals :: PostKey a -> Identity a -> Bool
          eqVals kv fv = eqViaKey kv fv (pState DM.! kv)

        annotateShow pState
        annotateShow p
        void $ DM.traverseWithKey (\kv fv -> Const <$> assert (eqVals kv fv)) p
    ]


--------------------------------------------------------------------------------
-- CREATE
--------------------------------------------------------------------------------
data CreatePost (v :: * -> *) =
  CreatePost (DMap PostKey v) PostMap
  deriving (Show)

instance HTraversable CreatePost where
  htraverse f (CreatePost dmv dmi) =
    CreatePost <$> htraverseDmap f dmv <*> pure dmi

cCreatePost
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     , HasPosts state
     )
  => Env
  -> Command n m (state :: (* -> *) -> *)
cCreatePost env@Env{..} =
  let
    gen = Just . genCreate
    exe (CreatePost dmv dmi) = do
      let
        dmi' = DM.map (\(Concrete a) -> pure a) dmv
        dm = DM.union dmi' dmi
      annotateShow $ encode dm
      r <- evalEither =<< liftIO (runClientM (createPost (auth env) dm) servantClient)
      pure r
  in
    Command gen exe [
      Update $ \s (CreatePost dmv dmi) o ->
          posts %~ (StatePost dmi o :) $ s
    ]

genCreate
  :: MonadGen n
  => state (v :: * -> *)
  -> n (CreatePost v)
genCreate _s = do
  content <- genAlphaNum 1 500
  excerpt' <- T.take <$> Gen.int (Range.linear 1 (T.length content - 1)) <*> pure content
  let
    excerpt = bool content excerpt' (T.null excerpt')
    gensI = [
        PostDateGmt :=> genUTCTime
      , PostSlug :=> genAlphaNum 0 30
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
  CreatePost <$> pure DM.empty <*> f gensI

genUTCTime
  :: MonadGen n
  => n LocalTime
genUTCTime =
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
