{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.QueryParamMap where

import           Data.Dependent.Map    (DMap)
import qualified Data.Dependent.Map    as DM
import           Data.Functor.Identity (Identity (Identity))
import           Data.List.NonEmpty    (NonEmpty (..))
import           Data.Proxy            (Proxy (Proxy))
import           Data.Text             (Text)
import           Servant.API           ((:>))
import           Servant.Client        (HasClient (..))
import           Servant.Common.Req    (appendToQueryString)
import           Web.HttpApiData       (ToHttpApiData, toQueryParam)

data QueryParamMap (key :: * -> *) (f :: * -> *)

instance (ToQueryParamKeyValues key f, HasClient api) => HasClient (QueryParamMap key f :> api) where
  type Client (QueryParamMap key f :> api) = DMap key f -> Client api

  clientWithRoute Proxy req dm =
    let
      addPair (k, v) = appendToQueryString k (Just v)
      f ka fa req' = foldr addPair req' $ toQueryParamKeyValues ka fa
    in
      clientWithRoute (Proxy :: Proxy api) $ DM.foldrWithKey f req dm

class ToHttpApiData1 f where
  liftToQueryParam :: (a -> Text) -> f a -> NonEmpty Text

toQueryParam1 ::
  ( ToHttpApiData a
  , ToHttpApiData1 f
  )
  => f a
  -> NonEmpty Text
toQueryParam1 =
  liftToQueryParam toQueryParam

instance ToHttpApiData1 Identity where
  liftToQueryParam f (Identity a) = f a :| []

instance ToHttpApiData1 NonEmpty where
  liftToQueryParam = fmap

class ToHttpApiDataViaKey (key :: * -> *) (f :: * -> *) where
  toQueryParamViaKey :: key a -> f a -> Text

class ToQueryParamKey (key :: * -> *) where
  toQueryParamKey :: key a -> Text

-- | Class for converting a something like a DMap to its query parameter (key,value) pairs.
class ToQueryParamKeyValues (k :: * -> *) f where
  toQueryParamKeyValues :: k a -> f a -> NonEmpty (Text, Text)

  default toQueryParamKeyValues ::
      ( ToQueryParamKey k
      , ToHttpApiData1 f
      , ToHttpApiData a
      )
    => k a
    -> f a
    -> NonEmpty (Text, Text)
  toQueryParamKeyValues =
      defaultToQueryParamKeyValues

defaultToQueryParamKeyValues ::
      ( ToQueryParamKey k
      , ToHttpApiData1 f
      , ToHttpApiData a
      )
    => k a
    -> f a
    -> NonEmpty (Text, Text)
defaultToQueryParamKeyValues ka =
    fmap (toQueryParamKey ka, ) . toQueryParam1

nonEmptyQueryParamKeyValues ::
  ( ToHttpApiData a
  , ToQueryParamKey k
  )
  => k (NonEmpty a)
  -> Identity (NonEmpty a)
  -> NonEmpty (Text, Text)
nonEmptyQueryParamKeyValues ka (Identity nea) =
    (toQueryParamKey ka, ) <$> toQueryParam1 nea
