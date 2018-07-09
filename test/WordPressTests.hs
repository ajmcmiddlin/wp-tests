{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}

module WordPressTests where

import           Control.Applicative           (liftA3)
import           Control.Lens                  (at, filtered, ix, sans, to,
                                                (%~), (&), (.~), (?~), (^.),
                                                (^..), (^?), _Just, _Wrapped)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.Aeson                    (encode)
import           Data.Bool                     (bool)
import           Data.Dependent.Map            (DMap)
import qualified Data.Dependent.Map            as DM
import           Data.Dependent.Map.Lens       (dmix)
import           Data.Dependent.Sum            (DSum (..), (==>))
import           Data.Functor.Classes          (Eq1, Ord1)
import           Data.Functor.Identity         (Identity (..))
import qualified Data.Map                      as M
import           Data.Maybe                    (fromMaybe, isJust)
import qualified Data.Text                     as T
import           Data.Time                     (LocalTime (LocalTime),
                                                diffUTCTime, fromGregorian,
                                                getCurrentTime, localTimeToUTC,
                                                nominalDay, secondsToDiffTime,
                                                timeToTimeOfDay, utc,
                                                utcToLocalTime)
import           Network.HTTP.Types.Status     (gone410, notFound404)
import           Servant.API                   (BasicAuthData (BasicAuthData))
import           Servant.Client                (ClientM, ServantError (..),
                                                runClientM)

import           Hedgehog                      (Callback (..),
                                                Command (Command),
                                                Concrete (Concrete), Gen,
                                                HTraversable (htraverse),
                                                MonadGen, MonadTest, Parallel,
                                                Property, PropertyT, Sequential,
                                                Symbolic, TestT, Var (Var),
                                                annotateShow, concrete, eval,
                                                evalEither, evalIO,
                                                executeParallel,
                                                executeSequential, failure,
                                                forAll, property, success, test,
                                                withRetries, (===))
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.Hedgehog           (testProperty)

import           Web.WordPress.API             (createPost, deletePost, getPost,
                                                listPosts, listPostsAuth)
import           Web.WordPress.Types.ListPosts (ListPostsKey (..), ListPostsMap)
import           Web.WordPress.Types.Post      (Author (Author), PostKey (..),
                                                PostMap, Slug, Status (..),
                                                mkCreatePR, mkCreateR, mkSlug,
                                                trashSlug)

import           Types                         (Env (..), HasPostMaps (..),
                                                HasPosts (..),
                                                HasStatePosts (..),
                                                StatePosts (..),
                                                WPState (WPState),
                                                hasKeyMatchingPredicate)

wordpressTests
  :: Env
  -> TestTree
wordpressTests env =
  testGroup "wordpress" [
    testProperty "sequential" $ propWordpress env
  , testProperty "parallel" $ propWordpressParallel env
  ]

type GenFn m state exeModel =
  MonadTest m
  => [Command Gen m state]
  -> (forall (v :: * -> *). state v)
  -> PropertyT IO (exeModel m state)

type ExeFn m state exeModel =
  (forall (v :: * -> *). state v)
  -> exeModel m state
  -> m ()

propWordpress
  :: Env
  -> Property
propWordpress env@Env{..} =
  let
    f :: GenFn m state Sequential
    f cs s = forAll $ Gen.sequential (Range.linear 1 100) s cs
  in
    property $ mkProp env f executeSequential

propWordpressParallel
  :: Env
  -> Property
propWordpressParallel env@Env{..} =
  let
    f :: GenFn m state Parallel
    f cs s = forAll $ Gen.parallel (Range.linear 1 100) (Range.linear 1 10) s cs
  in
    withRetries 10 . property $ mkProp env f executeParallel

mkProp ::
  Env
  -> GenFn (TestT IO) WPState exeModel
  -> ExeFn (TestT IO) WPState exeModel
  -> PropertyT IO ()
mkProp env@Env{..} gen exe = do
  now <- liftIO (utcToLocalTime utc <$> getCurrentTime)
  let
    commands = ($ env) <$> [cDeletePostParallel, cList, cListAuth, cCreatePost now, cGetPost]
    initialState = WPState . StatePosts $ M.empty
  actions <- gen commands initialState

  test $ do
    evalIO reset
    exe initialState actions


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

cList, cListAuth ::
  ( MonadGen n
  , MonadIO m
  , MonadTest m
  , HasPostMaps state
  , HasStatePosts state
  )
  => Env
  -> Command n m state
cList = mkCListPosts (Just . genList) (const listPosts)
cListAuth = mkCListPosts (Just . genListAuth) listPostsAuth

mkCListPosts
  :: ( MonadIO m
     , MonadTest m
     , HasPostMaps state
     , HasStatePosts state
     )
  => (state Symbolic -> Maybe (n (ListPosts Symbolic)))
  -> (BasicAuthData -> ListPostsMap -> ClientM [PostMap])
  -> Env
  -> Command n m state
mkCListPosts gen list env@Env{..} =
  let
    lup :: a -> ListPostsKey a -> ListPostsMap -> a
    lup def key = maybe def runIdentity . DM.lookup key

    lupPerPage = lup 10 ListPostsPerPage

    exe (ListPosts dmv dmi) =
      let
        dmi' = DM.map (\(Concrete a) -> pure a) dmv
        dm = DM.union dmi' dmi
        l = list (auth env) dm
      in
        evalEither =<< liftIO (runClientM l servantClient)
  in
    Command gen exe [
      Require $ \s (ListPosts _ lpi) ->
        let
          numPosts = length . postsWithStatus s $ lup Publish ListPostsStatus lpi
          numPages' = numPages numPosts $ lupPerPage lpi
        in
          lup 1 ListPostsPage lpi <= numPages'
    , Ensure $ \so _sn (ListPosts _ lpi) ps -> do
        annotateShow $ so ^. statePosts
        let
          pws = lup Publish ListPostsStatus lpi
          perPage = lupPerPage lpi
          eLength = length . take perPage $ postsWithStatus so pws
        eLength === length ps
    ]

genList
  :: ( MonadGen n
     , HasPostMaps state
     )
  => state Symbolic
  -> n (ListPosts v)
genList s = do
  status <- Gen.enumBounded
  perPage <- Gen.int (Range.linear 1 100)
  let
    numPosts = length $ postsWithStatus s status
    numPages' = numPages numPosts perPage
  page <- Gen.int (Range.linear 1 numPages')
  pure . ListPosts DM.empty $ DM.fromList [
      ListPostsPerPage ==> perPage
    , ListPostsPage ==> page
    ]

genListAuth
  :: ( MonadGen n
     , HasPostMaps state
     )
  => state Symbolic
  -> n (ListPosts v)
genListAuth s = do
  status <- Gen.enumBounded
  (ListPosts dmv dmi) <- genList s
  pure . ListPosts dmv $ DM.insert ListPostsStatus status dmi

postsWhere
  :: HasPostMaps state
  => state v
  -> (PostMap -> Bool)
  -> [PostMap]
postsWhere s p =
  s ^.. postMaps . traverse . filtered p

postsWithStatus
  :: HasPostMaps state
  => state v
  -> Status
  -> [PostMap]
postsWithStatus s status =
  postsWhere s ((== Identity status) . (DM.! PostStatus))

numPages ::
  Int
  -> Int
  -> Int
numPages numPosts perPage =
  max 1 . (\(d,m) -> d + min m 1) $ numPosts `divMod` perPage

--------------------------------------------------------------------------------
-- GET
--------------------------------------------------------------------------------
newtype GetPost (v :: * -> *) =
  GetPost (Var Int v)
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
      then Just $ GetPost <$> (s ^. posts . to M.keys & Gen.element)
      else Nothing
    exe (GetPost varId) = do
      let
        get = getPost (auth env) (concrete varId)
      evalEither =<< liftIO (runClientM get servantClient)
  in
    Command gen exe [
      Require $ \s (GetPost varId) ->
        s ^. posts . at varId & not . null
    , Ensure $ \_so sn (GetPost varId) p -> do
        stateMap <- eval $ (sn ^. posts) M.! varId
        let
          pState = DM.intersection p stateMap
        annotateShow stateMap
        annotateShow p
        stateMap === pState
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
     , HasPostMaps state
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
      let create = runIdentity . (DM.! PostId) <$> createPost (auth env) pm
      evalEither =<< liftIO (runClientM create servantClient)
  in
    Command gen exe [
      Update $ \s cp o ->
        posts . at o ?~ createToStatePost now cp $ s
    ]

createToStatePost ::
  LocalTime
  -> CreatePost v
  -> PostMap
createToStatePost now (CreatePost pm) =
  foldr ($) pm [
      fixCreateStatus now
    ]

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

genCreate ::
  ( MonadGen n
  , HasPostMaps state
  )
  => LocalTime
  -> state (v :: * -> *)
  -> n (CreatePost v)
genCreate now s = do
  content <- genAlpha 1 500
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
    genSlug = Gen.filter (not . existsPostWithSlug s) . fmap mkSlug $ genAlpha 1 300
    gensI = [
        PostDateGmt :=> genDate
        -- We don't want empty slugs because then WordPress defaults them and we can't be
        -- certain about when things should be equal without implementing their defaulting logic.
      , PostSlug :=> genSlug
      , PostStatus :=> Gen.filter (/= Trash) Gen.enumBounded
     -- , PostPassword
      , PostTitle :=> mkCreateR <$> genAlpha 1 30
      , PostContent :=> pure (mkCreatePR content)
      -- TODO: author should come from state. Start state has user with ID = 1.
      , PostAuthor :=> pure (Author 1)
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
  HasPostMaps state
  => state v
  -> Slug
  -> Bool
existsPostWithSlug s slug =
  let
    isMatchingSlug = any (hasKeyMatchingPredicate PostSlug (== Identity slug)) $ s ^. postMaps
  in
    (not . null $ Identity slug) && isMatchingSlug


--------------------------------------------------------------------------------
-- DELETE
--------------------------------------------------------------------------------
data DeletePost (v :: * -> *) =
  DeletePost (Var Int v) (Maybe Bool)
  deriving (Show)

instance HTraversable DeletePost where
  htraverse f (DeletePost (Var postId) force) =
    ($ force) . DeletePost . Var <$> f postId

cDeletePost
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     , HasPosts state
     )
  => Env
  -> Command n m (state :: (* -> *) -> *)
cDeletePost env@Env{..} =
  let
    exe (DeletePost varId force) = do
      let
        delete = deletePost (auth env) (concrete varId) force
      evalEither =<< liftIO (runClientM delete servantClient)
  in
    Command genDelete exe [
      Require $ \s (DeletePost varId _force) ->
        let
          postExists = s ^. posts . at varId & isJust
          postNotTrash = postFieldAt s PostStatus varId /= Just Trash
        in
          postExists && postNotTrash
    , Update $ \s (DeletePost varId forced) _o ->
        case forced of
          Just True -> posts %~ sans varId $ s
          _ ->
            -- TODO extract the optic for the field
            (posts . at varId . _Just . dmix PostSlug . _Wrapped %~ trashSlug )
            . (posts . at varId . _Just . dmix PostStatus . _Wrapped .~ Trash) $ s
    ]

cDeletePostParallel ::
  forall n m state.
  ( MonadGen n
  , MonadIO m
  , HasPosts state
  )
  => Env
  -> Command n m (state :: (* -> *) -> *)
cDeletePostParallel env@Env{..} =
  let
    exe (DeletePost varId force) =
      liftIO $ runClientM (deletePost (auth env) (concrete varId) force) servantClient

    -- Help GHC unify some type variables by pulling update out and giving it a signature
    update :: forall v. Ord1 v
      => state v -> DeletePost v -> Var (Either ServantError PostMap) v -> state v
    update s (DeletePost varId forced) _o =
      let
        pOld = s ^. posts . at varId
        pOldStatus = postFieldAt s PostStatus varId
        forced' = fromMaybe False forced

        updateField :: forall a. PostKey a -> (a -> a) -> state v -> state v
        updateField key f = posts . at varId . _Just . dmix key . _Wrapped %~ f
      in
        -- TODO overlapping patterns
        case (pOld, pOldStatus, forced') of
          (Nothing, _, _) ->
            s
          (Just _, Just Trash, False) ->
            s
          (Just _, _, False) ->
            updateField PostSlug trashSlug . updateField PostStatus (const Trash) $ s
          (Just _, _, True) ->
            posts %~ sans varId $ s
  in
    Command genDelete exe [
      Require $ \s (DeletePost varId _force) ->
        let
          postExists = s ^. posts . at varId & isJust
          postNotTrash = postFieldAt s PostStatus varId /= Just Trash
        in
          postExists && postNotTrash
    , Update update
    , Ensure $ \sOld sNew (DeletePost varId forced) o ->
        let
          pOld = sOld ^. posts . at varId
          pOldStatus = postFieldAt sOld PostStatus varId
          forced' = fromMaybe False forced
        in do
          annotateShow $ sOld ^. posts
          annotateShow $ sNew ^. posts
          case (pOld, pOldStatus, forced', o) of
            -- TODO: overlapping patterns
            (Nothing, _, _, Left FailureResponse{..}) ->
              responseStatus === notFound404
            (Nothing, _, _, Left e) -> do
              annotateShow e
              failure
            (Nothing, _, _, Right p) -> do
              annotateShow p
              failure
            (Just _, Just Trash, False, Left FailureResponse{..}) ->
              responseStatus === gone410
            (Just _, _, _, Right _) ->
              success
            (Just _, _, _, Left _) ->
              failure
    ]

genDelete ::
  ( MonadGen n
  , HasPosts state
  )
  => state Symbolic
  -> Maybe (n (DeletePost Symbolic))
genDelete s =
  let
    genId = Gen.element (s ^. posts . to M.keys)
    genIdNotTrash = Gen.filter ((/= Just Trash) . postFieldAt s PostStatus) genId
  in
    if s ^. posts & not . null
      then Just $ DeletePost <$> genIdNotTrash <*> Gen.maybe Gen.bool
      else Nothing

postFieldAt ::
  ( HasPosts state
  , Ord1 v
  )
  => state v
  -> PostKey a
  -> Var Int v
  -> Maybe a
postFieldAt s key varId  =
  s ^? posts . ix varId . dmix key . _Wrapped

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

genAlpha
  :: MonadGen n
  => Int
  -> Int
  -> n T.Text
genAlpha min' max' =
  Gen.text (Range.linear min' max') Gen.alpha

milliToMicro :: Integer -> Integer
milliToMicro = (* 1000)

auth
  :: Env
  -> BasicAuthData
auth Env{..} =
  BasicAuthData wpUser wpPassword
