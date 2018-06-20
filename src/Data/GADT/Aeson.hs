{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Data.GADT.Aeson where

import           Data.Aeson            (FromJSON (..), ToJSON (..), Value,
                                        object, withObject, (.:?))
import           Data.Aeson.Types      (FromJSON1, Parser, ToJSON1, toJSON1)
import           Data.Bool             (bool)
import           Data.Dependent.Map    (DMap, DSum (..), GCompare (..), empty,
                                        foldrWithKey, fromList, insert, lookup,
                                        toList)
import           Data.Functor.Identity (Identity (Identity))
import           Data.GADT.Compare     ((:~:) (Refl), GCompare (..), GEq (..),
                                        GOrdering (..))
import           Data.Monoid           (First (First, getFirst))
import           Data.Some             (Some (This))
import           Data.Text             (Text, pack)
import           GHC.Prim              (Proxy#, proxy#)
import           GHC.TypeLits          (KnownSymbol, Symbol, symbolVal')

class FromJSON1 f => FromJSONViaKey k f where
  parseJSONViaKey ::  k a -> Value -> Parser (f a)

class ToJSONViaKey k f where
  toJSONViaKey :: k a -> f a -> Value

class GKey k where
  toFieldName :: k a -> Text
  fromFieldName :: Text -> Maybe (Some k)
  keys :: [Some k]
  {-# MINIMAL toFieldName, keys #-}

  -- | This is not optimal as it does a linear search through 'keys' to find the first matching field.
  fromFieldName = defaultFromFieldName

defaultFromFieldName t = getFirst $ foldMap (\(This k) -> First $ bool Nothing (Just (This k)) (toFieldName k == t)) keys

data FooKey (s :: Symbol) a where
  I :: FooKey "i" Int

instance KnownSymbol s => GKey (FooKey s) where
  toFieldName (ka :: FooKey s a) = pack (symName @s)
  keys = undefined

  fromFieldName :: KnownSymbol s => Text -> Maybe (Some (FooKey s))
  fromFieldName = defaultFromFieldName

instance GEq (FooKey s) where
  geq I I = Just Refl

instance GCompare (FooKey s) where
  gcompare I I = GEQ

instance ToJSON1 f => ToJSONViaKey (FooKey s) f where
  toJSONViaKey I = toJSON1

data L (key :: Symbol -> * -> *) a where
  L :: KnownSymbol s => k s a -> L k a

instance GKey (L FooKey) where
  toFieldName (L ksa) = toFieldName ksa
  keys = [This (L I)]

instance GEq (L FooKey) where
  geq (L I) (L I) = Just Refl

instance GCompare (L FooKey) where
  gcompare (L I) (L I) = GEQ

data BarKey a where
  S :: BarKey Int

instance (GKey k, ToJSONViaKey k f) => ToJSON (DMap k f) where
  toJSON dm =
    let
      toPair (k :=> v) = (toFieldName k, toJSONViaKey k v)
    in
      object . fmap toPair . toList $ dm

-- | 'parseJSON' implementation for a 'DMap'. The reason we don't provide the instance itself is so
-- a name for the object can be provided, which gives better error messages.
mkParseJSON
  :: forall k f.
    (GKey k, GCompare k, Applicative f, FromJSONViaKey k f)
  => String
  -> Value
  -> Parser (DMap k f)
mkParseJSON t = withObject t $ \o ->
  let
    add :: Some k -> Parser (DMap k f) -> Parser (DMap k f)
    add (This k) pDm = do
      mv <- o .:? toFieldName k
      ma <- maybe (pure Nothing) (fmap Just . parseJSONViaKey k) mv
      maybe id (insert k) ma <$> pDm
  in
    foldr add (pure empty) keys

symName
  :: forall name. KnownSymbol name => String
symName =
  symbolVal' (proxy# :: (Proxy# name))
