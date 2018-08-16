{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where

import           Control.Lens             (Getter, Lens', lens, to)
import           Data.ByteString          (ByteString)
import           Data.Dependent.Map       (DMap)
import qualified Data.Dependent.Map       as DM
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Database.MySQL.Base      (ConnectInfo)
import           Servant.Client           (ClientEnv)
import           Web.WordPress.Types.Post (PostMap)

import           Hedgehog                 (Var)

data Env =
  Env
  { dbConnInfo    :: ConnectInfo
  , servantClient :: ClientEnv
  , wpUser        :: ByteString
  , wpPassword    :: ByteString
  , reset         :: IO ()
  }

type Posts v = Map (Var Int v) PostMap

class HasStatePosts s where
  statePosts :: Lens' (s v) (StatePosts v)

class HasPosts s where
  posts :: Lens' (s v) (Posts v)

class HasPostMaps (s :: (* -> *) -> *) where
  postMaps :: Getter (s v) [PostMap]

class HasPostMap (s :: (* -> *) -> *) where
  postMap :: Lens' (s v) PostMap


newtype WPState (v :: * -> *) =
  WPState
  { _posts :: StatePosts v
  }
  deriving (Eq, Show)

instance HasStatePosts WPState where
  statePosts = lens _posts (const WPState)

instance HasPosts WPState where
  posts = statePosts . posts

instance HasPostMaps WPState where
  postMaps = statePosts . postMaps


newtype StatePosts v =
  StatePosts {getStatePosts :: Posts v}
  deriving (Eq, Show)

instance HasPosts StatePosts where
  posts = lens getStatePosts (const StatePosts)

instance HasPostMaps StatePosts where
  postMaps = to $ \(StatePosts ps) -> M.elems ps


-- data StatePost v =
--   StatePost
--   { idPost  :: PostMap
--   , varPost :: Var PostMap v
--   }
--   deriving (Eq, Show)

-- instance HTraversable StatePost where
--   htraverse f (StatePost dmi dmv) =
--     StatePost dmi <$> htraverse f dmv

-- instance HasPostMap StatePost where
--   postMap =
--     lens idPost (\s ip -> s {idPost = ip})

-- smooshStatePost
--   :: StatePost Concrete
--   -> PostMap
-- smooshStatePost (StatePost dmi dmc) =
--   DM.union dmi (concrete dmc)

hasKeyMatchingPredicate ::
  DM.GCompare k
  => k a
  -> (f a -> Bool)
  -> DMap k f
  -> Bool
hasKeyMatchingPredicate ka p dm =
  case DM.lookup ka dm of
    Just fa -> p fa
    Nothing -> False

-- This looked like it was hanging, but might have been shrinking:
-- NOTE: upped the size from 54, as it failed on 55th test
-- recheck (Size 100) (Seed 2581475982454513726 (-5481735485570210791)) propWordpressParallel
-- recheck (Size 55) (Seed 3363758708465963961 (-434715610133022345)) <property>
