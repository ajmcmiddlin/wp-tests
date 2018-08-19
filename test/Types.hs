{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where

import           Control.Lens             (Getter, Lens', to)
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

class HasPosts s where
  posts :: Lens' (s v) (Posts v)

class HasPostMaps (s :: (* -> *) -> *) where
  postMaps :: Getter (s v) [PostMap]

class HasPostMap (s :: (* -> *) -> *) where
  postMap :: Lens' (s v) PostMap


newtype WPState (v :: * -> *) =
  WPState
  { _posts :: Posts v
  }
  deriving (Eq, Show)

instance HasPosts WPState where
  posts = posts

instance HasPostMaps WPState where
  postMaps = posts . to M.elems

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

