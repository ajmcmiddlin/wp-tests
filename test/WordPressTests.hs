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

import           Control.Lens             (filtered, (%~), (&), (^.), (^..))
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Aeson               (encode)
import           Data.Bool                (bool)
import           Data.ByteString          (ByteString)
import           Data.Dependent.Map       (DMap)
import qualified Data.Dependent.Map       as DM
import           Data.Dependent.Sum       (DSum (..))
import           Data.Functor             (void)
import           Data.Functor.Classes     (Eq1)
import           Data.Functor.Const       (Const (..))
import           Data.Functor.Identity    (Identity (..))
import qualified Data.Text                as T
import           Data.Time                (LocalTime (LocalTime), fromGregorian,
                                           secondsToDiffTime, timeToTimeOfDay)
import           Prelude                  hiding (max, min)
import           Servant.API              (BasicAuthData (BasicAuthData))
import           Servant.Client           (runClientM)

import           Hedgehog                 (Callback (..), Command (Command),
                                           Concrete (Concrete),
                                           HTraversable (htraverse), MonadGen,
                                           MonadTest, Property, Symbolic,
                                           Var (Var), annotateShow, assert,
                                           concrete, evalEither, evalIO,
                                           executeSequential, failure, forAll,
                                           property, success, withShrinks,
                                           withTests, (===))
import qualified Hedgehog.Gen             as Gen
import qualified Hedgehog.Range           as Range
import           Test.Tasty               (TestTree, testGroup)
import           Test.Tasty.Hedgehog      (testProperty)

import           Web.WordPress.API        (createPost, getPost, listPosts)
import           Web.WordPress.Types.Post (ListPostsKey, PostKey (..), PostMap,
                                           RESTContext (Create), Renderable,
                                           Status (..), mkCreatePR, mkCreateR, EqViaKey (..))

import           Types                    (Env (..), HasPosts (..),
                                           WPState (WPState))

wordpressTests
  :: Env
  -> TestTree
wordpressTests env =
  testGroup "wordpress" [
    testProperty "sequential" $ propWordpress env
  ]

propWordpress
  :: Env
  -> Property
propWordpress env@Env{..} =
  property $ do
  let
    commands = ($ env) <$> [cListPosts, cCreatePost, cGetPost]
    initialState = WPState []
  actions <- forAll $
    Gen.sequential (Range.linear 1 20) initialState commands

  evalIO reset
  executeSequential initialState actions


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
     )
  => Env
  -> Command n m state
cListPosts Env{..} =
  let
    gen _ =
      Just . pure $ ListPosts DM.empty DM.empty
    exe (ListPosts dmv dmi) =
      let
        dmi' = DM.map (\(Concrete a) -> pure a) dmv
        dm = DM.union dmi' dmi
      in
        evalEither =<< liftIO (runClientM (listPosts dm) servantClient)
  in
    Command gen exe [
      Ensure $ \so _sn _i ps ->
        let f p = concrete p DM.! PostStatus == Identity Publish
        in (so ^.. posts . traverse . filtered f & length) === length ps
    ]


--------------------------------------------------------------------------------
-- GET
--------------------------------------------------------------------------------
newtype GetPost (v :: * -> *) =
  GetPost (Var PostMap v)
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
    exe (GetPost v) =
      let postId = runIdentity $ concrete v DM.! PostId
       in evalEither =<< liftIO (runClientM (getPost (auth env) postId) servantClient)
  in
    Command gen exe [
      Ensure $ \_so _sn (GetPost v) p -> do
        let
          ps = concrete v
        annotateShow ps
        annotateShow p
        void $ DM.traverseWithKey (\kv fv -> Const <$> assert (eqViaKey kv fv (ps DM.! kv))) p
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
      Update $ \s _i o -> posts %~ (o :) $ s
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
genAlphaNum min max =
  Gen.text (Range.linear min max) Gen.alphaNum

milliToMicro :: Integer -> Integer
milliToMicro = (* 1000)

auth
  :: Env
  -> BasicAuthData
auth Env{..} =
  BasicAuthData wpUser wpPassword
