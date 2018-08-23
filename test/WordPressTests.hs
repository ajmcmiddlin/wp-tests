{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}

module WordPressTests where

import           Control.Applicative           ((<|>))
import           Control.Lens                  (at, filtered, ix, sans, to,
                                                (%~), (&), (?~), (^.), (^..),
                                                (^?), _Just, _Wrapped)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.Aeson                    (encode)
import           Data.Bool                     (bool)
import           Data.Dependent.Map            (DMap)
import qualified Data.Dependent.Map            as DM
import           Data.Dependent.Map.Lens       (dmix)
import           Data.Dependent.Sum            (DSum (..), (==>), EqTag (eqTagged))
import           Data.Foldable                 (traverse_)
import           Data.Functor                  ((<$), void)
import           Data.Functor.Classes          (Eq1, Ord1)
import           Data.Functor.Const            (Const (Const))
import           Data.Functor.Identity         (Identity (..))
import           Data.List.NonEmpty            (NonEmpty ((:|)))
import qualified Data.Map                      as M
import           Data.Maybe                    (fromJust, fromMaybe)
import qualified Data.Text                     as T
import           Data.Time                     (LocalTime (LocalTime),
                                                fromGregorian, getCurrentTime,
                                                secondsToDiffTime,
                                                timeToTimeOfDay, utc,
                                                utcToLocalTime)
import           Data.Time.Extras              (nominalDiff, nominalSecond)
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
                                                annotateShow, assert, concrete,
                                                eval, evalEither, evalIO,
                                                executeParallel,
                                                executeSequential, failure,
                                                forAll, property, success, test,
                                                withRetries, (===))
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.Hedgehog           (testProperty)

import           Web.WordPress.API             (createPost, deletePost,
                                                deletePostForce, getPost,
                                                listPosts, listPostsAuth,
                                                updatePost)
import           Web.WordPress.Types.ListPosts (ListPostsKey (..), ListPostsMap)
import           Web.WordPress.Types.Post      (Author (Author),
                                                DeletedPost (..),
                                                ForceDelete (..),
                                                NoForceDelete (..),
                                                PostKey (..), PostMap, Slug,
                                                Status (..), mkCreatePR,
                                                mkCreateR, mkSlug, trashSlug)

import           Types                         (Env (..), HasPosts (posts),
                                                StatePost (StatePost),
                                                WPState (WPState), getStatePost,
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
    commands = ($ env) <$> [cDeletePostParallel, cList, cListAuth, cCreatePost now, cGetPost now, cUpdatePost now]
    initialState = WPState M.empty
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
  , HasPosts state
  )
  => Env
  -> Command n m state
cList = mkCListPosts (Just . genList) (const listPosts)
cListAuth = mkCListPosts (Just . genListAuth) listPostsAuth

-- TODO: subclass `HasPost*` to avoid so many constraints?
mkCListPosts
  :: ( MonadIO m
     , MonadTest m
     , HasPosts state
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
        annotateShow $ so ^. posts
        annotateShow ps
        let
          pws = lup Publish ListPostsStatus lpi
          perPage = lupPerPage lpi
          eLength = length . take perPage $ postsWithStatus so pws
        eLength === length ps
        traverse_ (lookupAndCheck so) ps
    ]

lookupAndCheck ::
  ( HasPosts state
  , MonadTest m
  )
  => state Concrete
  -> PostMap
  -> m ()
lookupAndCheck s p = do
  let
    pId = runIdentity $ p DM.! PostId
  annotateShow p
  case s ^. posts . at (Var (Concrete pId)) of
    Just (StatePost sp) -> sp === DM.intersection p sp
    Nothing             -> failure

genList
  :: ( MonadGen n
     , HasPosts state
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
     , HasPosts state
     )
  => state Symbolic
  -> n (ListPosts v)
genListAuth s = do
  status <- Gen.enumBounded
  (ListPosts dmv dmi) <- genList s
  pure . ListPosts dmv $ DM.insert ListPostsStatus status dmi

postsWhere
  :: HasPosts state
  => state v
  -> (PostMap -> Bool)
  -> [PostMap]
postsWhere s p =
  s ^.. posts . traverse . _Wrapped . filtered p

postsWithStatus
  :: HasPosts state
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
  => LocalTime
  -> Env
  -> Command n m state
cGetPost now env@Env{..} =
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
        annotateShow stateMap
        annotateShow p
        -- TODO clean this up
        void . DM.traverseWithKey (\ka -> const $ Const () <$ fieldsEq now ka stateMap p) . getStatePost $ stateMap
    ]

fieldsEq ::
  ( EqTag PostKey Identity
  , MonadTest m
  )
  => LocalTime
  -> PostKey a
  -> StatePost
  -> PostMap
  -> m ()
fieldsEq now k s m' =
  let
    matches = eqTagged k k $ m' DM.! k
  in
    assert . any matches $ expected now k s

expected ::
  LocalTime
  -> PostKey a
  -> StatePost
  -> NonEmpty (Identity a)
expected now k (StatePost m) =
  case k of
    PostStatus ->
      let
        s =  runIdentity $ m DM.! PostStatus
        dt = nominalDiff (postDate m) now
      in
        if | s == Future && abs dt < (nominalSecond * 10) -> pure <$> Future :| [Publish]
           | s == Future && dt > 0 -> pure <$> Publish :| []
           | otherwise -> pure s :| []
    -- TODO: be better
    _ -> pure $ m DM.! k

postDate ::
  PostMap
  -> LocalTime
postDate m =
  -- Non-GMT date gets preference apparently. This was determined by creating a post with two dates
  -- that didn't agree and seeing which one came back. Also YOLO `fromJust`. Some men just want to
  -- watch the world burn.
  runIdentity . fromJust $
    DM.lookup PostDate m <|> DM.lookup PostDateGmt m

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
     )
  => LocalTime
  -> Env
  -> Command n m (state :: (* -> *) -> *)
cCreatePost now env@Env{..} =
  let
    gen = Just . fmap CreatePost . genPost
    exe (CreatePost pm) = do
      annotateShow pm
      annotateShow $ encode pm
      let create = runIdentity . (DM.! PostId) <$> createPost (auth env) pm
      evalEither =<< liftIO (runClientM create servantClient)
  in
    Command gen exe [
      Update $ \s (CreatePost p) o ->
        posts . at o ?~ genToStatePost now p $ s
    ]

--------------------------------------------------------------------------------
-- UPDATE
--------------------------------------------------------------------------------
data UpdatePost (v :: * -> *) =
  UpdatePost (Var Int v) PostMap
  deriving (Show)

instance HTraversable UpdatePost where
  htraverse f (UpdatePost pId dmi) =
    fmap (flip UpdatePost dmi) (htraverse f pId)

cUpdatePost
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     , HasPosts state
     )
  => LocalTime
  -> Env
  -> Command n m (state :: (* -> *) -> *)
cUpdatePost now env@Env{..} =
  let
    genId s = s ^. posts . to M.keys & Gen.element
    gen s =
      if s ^. posts & (not . null)
      then Just $ UpdatePost <$> genId s <*> genPost s
      else Nothing
    exe (UpdatePost pId pm) = do
      annotateShow pm
      annotateShow $ encode pm
      let update = runIdentity . (DM.! PostId) <$> updatePost (auth env) (concrete pId) pm
      evalEither =<< liftIO (runClientM update servantClient)
  in
    Command gen exe [
      Require $ \s (UpdatePost varId _) ->
        s ^. posts . at varId & not . null
    , Update $ \s (UpdatePost pId p) _ ->
        posts . at pId ?~ genToStatePost now p $ s
    ]

genToStatePost ::
  LocalTime
  -> PostMap
  -> StatePost
genToStatePost now pm =
  StatePost $ foldr ($) pm [
      fixGenStatus now
    ]

fixGenStatus ::
  LocalTime
  -> PostMap
  -> PostMap
fixGenStatus now pm =
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

genPost ::
  ( MonadGen n
  , HasPosts state
  )
  => state (v :: * -> *)
  -> n PostMap
genPost s = do
  content <- genAlpha 1 500
  excerpt' <- T.take <$> Gen.int (Range.linear 1 (T.length content - 1)) <*> pure content
  status <- Gen.filter (/= Trash) Gen.enumBounded
  let
    -- If something is marked for publishing in the future then make sure our date is at least a
    -- day away so its status doesn't change during testing.
    excerpt = bool content excerpt' (T.null excerpt')
    genSlug = Gen.filter (not . existsPostWithSlug s) . fmap mkSlug $ genAlpha 1 300
    gensI = [
        PostDateGmt :=> genLocalTime
        -- We don't want empty slugs because then WordPress defaults them and we can't be
        -- certain about when things should be equal without implementing their defaulting logic.
      , PostSlug :=> genSlug
      , PostStatus :=> pure status
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
  DM.traverseWithKey (const (fmap pure)) $ DM.fromList gensI

existsPostWithSlug ::
  HasPosts state
  => state v
  -> Slug
  -> Bool
existsPostWithSlug s slug =
  let
    pms = s ^.. posts . traverse . _Wrapped
    isMatchingSlug = any (hasKeyMatchingPredicate PostSlug (== Identity slug))
  in
    (not . null $ Identity slug) && isMatchingSlug pms


--------------------------------------------------------------------------------
-- DELETE
--------------------------------------------------------------------------------
data DeletePost (v :: * -> *) =
  DeletePost (Var Int v) (Maybe Bool)
  deriving (Show)

instance HTraversable DeletePost where
  htraverse f (DeletePost (Var postId) force) =
    ($ force) . DeletePost . Var <$> f postId

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
      liftIO $ runClientM (getDelete (auth env) (concrete varId) force) servantClient

    -- Help GHC unify some type variables by pulling update out and giving it a signature
    update :: forall v. Ord1 v
      => state v -> DeletePost v -> Var (Either ServantError DeletedPost) v -> state v
    update s (DeletePost varId forced) _o =
      let
        pOld = s ^. posts . at varId
        pOldStatus = postFieldAt s PostStatus varId
        forced' = fromMaybe False forced

        updateField :: forall a. PostKey a -> (a -> a) -> state v -> state v
        updateField key f = posts . at varId . _Just . _Wrapped . dmix key . _Wrapped %~ f
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
      Update update
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

getDelete ::
  BasicAuthData
  -> Int
  -> Maybe Bool
  -> ClientM DeletedPost
getDelete a postId = \case
  Nothing -> deletePost a postId Nothing
  Just False -> deletePost a postId (Just NoForceDelete)
  Just True -> deletePostForce a postId ForceDelete

genDelete ::
  ( MonadGen n
  , HasPosts state
  )
  => state Symbolic
  -> Maybe (n (DeletePost Symbolic))
genDelete s =
  let
    genId = Gen.element (s ^. posts . to M.keys)
  in
    if s ^. posts & not . null
      then Just $ DeletePost <$> genId <*> Gen.maybe Gen.bool
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
  s ^? posts . ix varId . _Wrapped . dmix key . _Wrapped

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
