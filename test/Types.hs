{-# LANGUAGE KindSignatures #-}

module Types where

import           Control.Lens             (Lens', lens)
import           Data.ByteString          (ByteString)
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

type StatePosts v = [Var PostMap v]

newtype WPState (v :: * -> *) =
  WPState
  { _posts :: StatePosts v
  }

class HasPosts (s :: (* -> *) -> *) where
  posts :: Lens' (s v) (StatePosts v)

instance HasPosts WPState where
  posts = lens _posts (const WPState)

