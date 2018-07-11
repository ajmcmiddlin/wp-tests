{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.RequiredQueryParam where

import           Data.Proxy            (Proxy (Proxy))
import           GHC.TypeLits          (KnownSymbol, Symbol)
import           Servant.API           ((:>), HasLink (..), QueryParam)
import           Servant.Client        (HasClient (..))
import           Web.HttpApiData       (ToHttpApiData)


-- Taken from: https://github.com/haskell-servant/servant/issues/241#issuecomment-285466745
-- TODO: replace with Servant.API.Modifiers when we hit servant >= 0.14
type role QueryParam' phantom phantom
data QueryParam' (sym :: Symbol) (a :: k)

instance (KnownSymbol sym, ToHttpApiData v, HasLink sub) =>
         HasLink (QueryParam' sym v :> sub) where
  type MkLink (QueryParam' sym v :> sub) = v -> MkLink sub
  toLink _ l v = toLink (Proxy :: Proxy (QueryParam sym v :> sub)) l (Just v)

instance (KnownSymbol sym, ToHttpApiData a, HasClient api) =>
         HasClient (QueryParam' sym a :> api) where
  type Client (QueryParam' sym a :> api) = a -> Client api
  clientWithRoute Proxy req param =
    clientWithRoute (Proxy :: Proxy (QueryParam sym a :> api)) req (Just param)
