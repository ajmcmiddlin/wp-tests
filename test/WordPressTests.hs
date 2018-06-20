{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}

module WordPressTests where

import           Control.Lens             (makeFields, (&), (^.))
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Dependent.Map       (DMap)
import qualified Data.Dependent.Map       as DM
import           Data.Dependent.Sum       (EqTag, DSum (..))
import           Data.Functor.Classes     (Eq1)
import           Data.Functor.Identity    (Identity (..))
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Time                (UTCTime (UTCTime), fromGregorian,
                                           secondsToDiffTime, utc,
                                           utcToLocalTime)
import           Servant.Client           (runClientM)

import           Hedgehog                 (Callback (..), Command (Command),
                                           Concrete (Concrete),
                                           HTraversable (htraverse), MonadGen,
                                           MonadTest, Property, Symbolic,
                                           Var (Var), annotateShow, assert,
                                           concrete, evalEither, evalIO,
                                           executeSequential, failure, forAll,
                                           property, success, (===))
import qualified Hedgehog.Gen             as Gen
import qualified Hedgehog.Range           as Range
import           Test.Tasty               (TestTree, testGroup)
import           Test.Tasty.Hedgehog      (testProperty)

import           Web.WordPress.API        (listPosts, createPost)
import           Web.WordPress.Types.Post (ListPostsKey, PostMap, PostKey (..), Rendered (..))

import           Types                    (Env (..), HasPosts (..),
                                           WPState (WPState))

wordpressTests
  :: Env
  -> TestTree
wordpressTests env =
  testGroup "wordpress" [
    propWordpress env
  ]

propWordpress
  :: Env
  -> TestTree
propWordpress env@Env{..} =
  testProperty "sequential" . property $ do
  let
    commands = ($ env) <$> [cListPosts] --, cAddPost]
    initialState = WPState M.empty
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialState commands

  evalIO reset
  executeSequential initialState actions


--------------------------------------------------------------------------------
-- LIST
--------------------------------------------------------------------------------
data ListPosts (v :: * -> *) =
  -- Two maps -- one for Vars and one for stuff we know ahead of time.
  ListPosts (DMap ListPostsKey v) (DMap ListPostsKey Identity)
  deriving (Show)

instance HTraversable (DMap k) where
  htraverse f =
    DM.traverseWithKey (const f)

deriving instance Eq1 v => Eq (ListPosts v)

instance HTraversable ListPosts where
  htraverse f (ListPosts dmv dmi) = ListPosts <$> htraverse f dmv <*> pure dmi

cListPosts
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     , HasPosts state
     )
  => Env
  -> Command n m state
cListPosts Env{..} =
  let
    gen _ =
      Just . pure $ ListPosts DM.empty DM.empty
    exe (ListPosts dmv dmi) =
      let
        dmi' = DM.map (\(Concrete a) -> Identity a) dmv
        dm = DM.union dmi' dmi
      in
        evalEither =<< liftIO (runClientM (listPosts dm) servantClient)
  in
    Command gen exe [
      Ensure $ \so _sn _i ps ->
        (so ^. posts & length) === length ps
    ]

--------------------------------------------------------------------------------
-- ADD
--------------------------------------------------------------------------------
data CreatePost (v :: * -> *) =
  CreatePost (DMap PostKey v) (DMap PostKey Identity)
  deriving (Show)

instance HTraversable CreatePost where
  htraverse f (CreatePost dmv dmi) =
    CreatePost <$> htraverse f dmv <*> pure dmi

cCreatePost
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     , HasPosts state
     )
  => Env
  -> Command n m state
cCreatePost Env{..} =
  let
    gen s = Just genCreate
    exe (CreatePost dmv dmi) =
      let
        dmi' = DM.map (\(Concrete a) -> Identity a) dmv
        dm = DM.union dmi' dmi
      in
        evalEither =<< liftIO (runClientM (createPost dm) servantClient)
  in
    Command gen exe [
    ]

genRP
  :: MonadGen n
  => Int
  -> Int
  -> n (CreatePost Identity)
genRP min max =
  RP <$> genUni min max <*> Gen.bool

genCreate
  :: MonadGen n
  => state (v :: * -> *)
  -> n (CreatePost Identity)
genCreate s =
  let
    gensI = [
        PostDateGmt :=> genUTCTime
      , PostSlug :=> genUni 0 30
      , PostStatus :=> Gen.enumBounded
     -- , PostPassword
      , PostTitle :=> Rendered <$> genUni 0 30
      , PostContent :=> genRP 0 500
      -- TODO: author should come from state. Start state has user with ID = 1.
      , PostAuthor :=> pure 1
     -- , PostExcerpt
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
    gensV = [
      ]
    f = fmap DM.fromList . traverse (\(kv :=> fv) -> fmap ((kv :=>) . pure) fv)
  in
    CreatePost <$> f gensV <*> f gensI

genUTCTime
  :: MonadGen n
  => n UTCTime
genUTCTime =
  let
    gYear = Gen.int (Range.linearFrom 1900 1970 2500)
    gMonth = Gen.int (Range.linear 1 12)
    -- fromGregorian automatically trims to valid dates, so 2001-02-31 becomes 2001-02-28
    gDay = Gen.int (Range.linear 1 31)
    hToS = (* 3600)
    gSeconds = Gen.int (Range.linearFrom (hToS 12) 0 86400)
    gUTCTimeDay = fromGregorian . fromIntegral <$> gYear <*> gMonth <*> gDay
    gDiffTime = secondsToDiffTime . fromIntegral <$> gSeconds
  in
    UTCTime <$> gUTCTimeDay <*> gDiffTime

genUni
  :: MonadGen n
  => Int
  -> Int
  -> n Text
genUni min max =
  Gen.text (Range.linear min max) Gen.unicode
