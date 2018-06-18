{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}

module WordPressTests where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Dependent.Map       (DMap)
import qualified Data.Dependent.Map       as DM
import           Data.Dependent.Sum       (EqTag)
import Data.Functor.Classes (Eq1)
import           Data.Map                 (Map)
import qualified Data.Map                 as M


import           Hedgehog                 (Callback (..), Command (Command),
                                           HTraversable (htraverse), MonadGen,
                                           MonadTest, Property, Var (Var),
                                           annotateShow, assert, concrete,
                                           evalEither, evalIO,
                                           executeSequential, failure, forAll,
                                           property, success, (===), Symbolic)
import qualified Hedgehog.Gen             as Gen
import qualified Hedgehog.Range           as Range
import           Test.Tasty               (TestTree, testGroup)
import           Test.Tasty.Hedgehog      (testProperty)

import           Web.WordPress.Types.Post (ListPostsKey, PostMap)

import           Types                    (Env (..))

data WPState (v :: * -> *) =
  WPState
  { posts :: Map (Var Int v) (Var PostMap v)
  }

wordpressTests
  :: Env
  -> TestTree
wordpressTests env =
  testGroup "wordpress" [
    propWordpress env
  ]

--------------------------------------------------------------------------------
-- LIST
--------------------------------------------------------------------------------
data MaybzVar v where
  --forall a.
  I :: a -> MaybzVar v
  V :: Var a v -> MaybzVar v

instance HTraversable MaybzVar where
  htraverse f = \case
    I a -> pure (I a)
    V v -> V <$> htraverse f v

data ListPosts (v :: * -> *) =
  ListPosts (DMap ListPostsKey v)
  deriving (Show)

deriving instance Eq1 v => Eq (ListPosts v)

instance HTraversable ListPosts where
  htraverse f (ListPosts dm) = undefined

cListPosts
  :: ( MonadGen n
     , MonadIO m
     --, HasPosts state
     )
  => Env
  -> Command n m state
cListPosts Env{..} =
  let
    gen s =
      Just . pure . ListPosts $ DM.empty
    exe i =
      liftIO . putStrLn $ "foo"
  in
    Command gen exe [
    ]

--------------------------------------------------------------------------------
-- LIST
--------------------------------------------------------------------------------
cAddPost
  :: ( MonadGen n
     , MonadIO m
     --, HasPosts state
     )
  => Env
  -> Command n m state
cAddPost Env{..} =
  undefined
  -- let
  --   gen s =
  --     undefined
  --   exe =
  --     undefined
  -- in
  --   Command gen exe [
  --   ]

propWordpress
  :: Env
  -> TestTree
propWordpress env@Env{..} =
  testProperty "sequential" . property $ do
  let
    commands = ($ env) <$> [cListPosts, cAddPost]
    initialState = WPState M.empty
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialState commands

  evalIO (reset dbConn)
  executeSequential initialState actions

