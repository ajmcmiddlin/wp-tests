{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where

import           Control.Lens             (Getter, Lens', lens, to)
import           Data.ByteString          (ByteString)
import           Data.Dependent.Map       (DMap)
import qualified Data.Dependent.Map       as DM
import           Database.MySQL.Base      (ConnectInfo)
import           Servant.Client           (ClientEnv)
import           Web.WordPress.Types.Post (PostMap)

import           Hedgehog                 (Concrete (..), HTraversable (..),
                                           Var, concrete)

data Env =
  Env
  { dbConnInfo    :: ConnectInfo
  , servantClient :: ClientEnv
  , wpUser        :: ByteString
  , wpPassword    :: ByteString
  , reset         :: IO ()
  }

class HasStatePosts s where
  statePosts :: Lens' (s v) (StatePosts v)

class HasPosts s where
  posts :: Lens' (s v) [StatePost v]

class HasIdentityPosts (s :: (* -> *) -> *) where
  identityPosts :: Getter (s v) [PostMap]

class HasVarPosts (s :: (* -> *) -> *) where
  varPosts :: Getter (s v) [Var PostMap v]

class HasIdentityPost (s :: (* -> *) -> *) where
  identityPost :: Lens' (s v) PostMap


newtype WPState (v :: * -> *) =
  WPState
  { _posts :: StatePosts v
  }
  deriving (Eq, Show)

instance HasStatePosts WPState where
  statePosts = lens _posts (const WPState)

instance HasPosts WPState where
  posts = statePosts . posts

instance HasIdentityPosts WPState where
  identityPosts = statePosts . identityPosts

instance HasVarPosts WPState where
  varPosts = statePosts . varPosts


newtype StatePosts v =
  StatePosts {getStatePosts :: [StatePost v]}
  deriving (Eq, Show)

instance HasPosts StatePosts where
  posts = lens getStatePosts (const StatePosts)

instance HasIdentityPosts StatePosts where
  identityPosts = to $ \(StatePosts sps) ->
    fmap (\(StatePost dmi _) -> dmi) sps

instance HasVarPosts StatePosts where
  varPosts = posts . to (fmap (\(StatePost _ dmv) -> dmv))


data StatePost v =
  StatePost
  { idPost :: PostMap
  , varPost :: Var PostMap v
  }
  deriving (Eq, Show)

instance HTraversable StatePost where
  htraverse f (StatePost dmi dmv) =
    StatePost dmi <$> htraverse f dmv

instance HasIdentityPost StatePost where
  identityPost =
    lens idPost (\s ip -> s {idPost = ip})

smooshStatePost
  :: StatePost Concrete
  -> PostMap
smooshStatePost (StatePost dmi dmc) =
  DM.union dmi (concrete dmc)

hasKeyMatchingPredicate ::
  DM.GCompare k
  => k a
  -> (f a -> Bool)
  -> DMap k f
  -> Bool
hasKeyMatchingPredicate ka p dm =
  case DM.lookup ka dm of
    Just fa -> p fa
    Nothing  -> False

-- This looked like it was hanging, but might have been shrinking:
-- NOTE: upped the size from 54, as it failed on 55th test
-- recheck (Size 100) (Seed 2581475982454513726 (-5481735485570210791)) propWordpressParallel
-- recheck (Size 55) (Seed 3363758708465963961 (-434715610133022345)) <property>
