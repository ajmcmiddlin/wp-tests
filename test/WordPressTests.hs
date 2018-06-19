{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}

module WordPressTests where

import           Control.Lens             (makeFields, (^.), (&))
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Dependent.Map       (DMap)
import qualified Data.Dependent.Map       as DM
import           Data.Dependent.Sum       (EqTag)
import           Data.Functor.Classes     (Eq1)
import           Data.Functor.Identity    (Identity (..))
import           Data.Map                 (Map)
import qualified Data.Map                 as M
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

import           Web.WordPress.API        (listPosts)
import           Web.WordPress.Types.Post (ListPostsKey, PostMap)

import           Types                    (Env (..), HasPosts (..), WPState (WPState))

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
    commands = ($ env) <$> [cListPosts] --, cAddPost]
    initialState = WPState M.empty
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialState commands

  evalIO reset
  executeSequential initialState actions
