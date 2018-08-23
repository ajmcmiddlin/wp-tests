{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Types where

import           Control.Lens             (Lens', Rewrapped, Wrapped, lens)
import           Data.ByteString          (ByteString)
import           Data.Dependent.Map       (DMap)
import qualified Data.Dependent.Map       as DM
import           Data.Map                 (Map)
import           Database.MySQL.Base      (ConnectInfo)
import           Servant.Client           (ClientEnv)
import           Web.WordPress.Types.Post (PostMap)

import           GHC.Generics             (Generic)

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
  StatePost { getStatePost :: PostMap}
  deriving (Eq, Show, Generic)

instance Wrapped StatePost
instance Rewrapped StatePost StatePost

type Posts v = Map (Var Int v) StatePost

class HasPosts s where
  posts :: Lens' (s v) (Posts v)


newtype WPState (v :: * -> *) =
  WPState
  { _posts :: Posts v
  }
  deriving (Eq, Show)

instance HasPosts WPState where
  posts = posts


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
