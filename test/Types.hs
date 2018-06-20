{-# LANGUAGE KindSignatures #-}

module Types where

import           Control.Lens             (Getter, to)
import           Data.ByteString          (ByteString)
import           Data.Dependent.Map       (DMap)
import           Data.Map                 (Map)
import           Database.MySQL.Base      (Connection)
import           Servant.Client           (ClientEnv)
import           Web.WordPress.Types.Post (PostMap)

import           Hedgehog                 (Var)

data Env =
  Env
  { dbConn        :: Connection
  , servantClient :: ClientEnv
  , wpUser        :: ByteString
  , wpPassword    :: ByteString
  , reset         :: IO ()
  }

type StatePosts v = Map (Var Int v) (Var PostMap v)

newtype WPState (v :: * -> *) =
  WPState
  { _posts :: StatePosts v
  }

class HasPosts (s :: (* -> *) -> *) where
  posts :: Getter (s v) (StatePosts v)

instance HasPosts WPState where
  posts = to _posts

