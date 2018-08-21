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

-- We want to differentiate between post maps we keep in state and those we get back.
newtype StatePost =
  StatePost PostMap
  deriving (Eq, Show)

type Posts v = Map (Var Int v) StatePost

class HasPosts s where
  posts :: Lens' (s v) (Posts v)

class HasPostsList (s :: (* -> *) -> *) where
  postsList :: Getter (s v) [StatePost]

-- class HasPostMap (s :: (* -> *) -> *) where
--   postMap :: Lens' (s v) Posts

newtype WPState (v :: * -> *) =
  WPState
  { _posts :: Posts v
  }
  deriving (Eq, Show)

instance HasPosts WPState where
  posts = posts

instance HasPostsList WPState where
  postsList = posts . to M.elems


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
