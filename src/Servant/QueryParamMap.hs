{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Servant.QueryParamMap where

import Data.Aeson.Text (encodeToLazyText)
import           Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DM
import Data.Proxy (Proxy (Proxy))
import Data.Text.Lazy (toStrict)
import           Servant.API        ((:>))
import           Servant.Client     (HasClient (..))
import Servant.Common.Req (appendToQueryString)

import Data.GADT.Aeson (ToJSONViaKey (..), GKey (..))

data QueryParamMap (key :: * -> *) (f :: * -> *)

-- TODO: replace GKey with QueryParamName class
-- TODO: replace JSON encoding with something specific to QueryParamMap
instance (ToJSONViaKey key f, GKey key, HasClient api) => HasClient (QueryParamMap key f :> api) where
  type Client (QueryParamMap key f :> api) = DMap key f -> Client api

  clientWithRoute Proxy req dm =
    let
      f ka fa = appendToQueryString (toFieldName ka) (Just . toStrict . encodeToLazyText . toJSONViaKey ka $ fa)
      req' = DM.foldrWithKey f req dm
    in
      clientWithRoute (Proxy :: Proxy api) req'
