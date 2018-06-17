{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Data.GADT.Aeson where

import           Data.Aeson            (FromJSON (..), ToJSON (..), Value, object, withObject, (.:?))
import           Data.Aeson.Types      (Parser)
import Data.Bool (bool)
import           Data.Dependent.Map    (DMap, DSum (..), GCompare (..), empty,
                                        foldrWithKey, fromList, insert, lookup,
                                        toList)
import           Data.Functor.Identity (Identity (Identity))
import           Data.GADT.Compare     ((:~:) (Refl), GCompare (..), GEq (..),
                                        GOrdering (..))
import Data.Monoid (First (First, getFirst))
import           Data.Some             (Some (This))
import           Data.Text             (Text, pack)
import           GHC.Prim              (Proxy#, proxy#)
import           GHC.TypeLits          (KnownSymbol, Symbol, symbolVal')

class FromJSONKey k f where
  parseJSONKey :: forall a. FromJSON (f a) => k a -> Value -> Parser (f a)

class ToJSONKey k f where
  toJSONKey :: k a -> f a -> Value

class GKey k where
  toFieldName :: k a -> Text
  fromFieldName :: Text -> Maybe (Some k)
  keys :: [Some k]

  {-# MINIMAL toFieldName, keys #-}

  fromFieldName t = getFirst $ foldMap (\(This k) -> First $ bool Nothing (Just (This k)) (toFieldName k == t)) keys

data FooKey (s :: Symbol) a where
  I :: FooKey "i" Int

instance KnownSymbol s => GKey (FooKey s) where
  toFieldName (ka :: FooKey s a) = pack (symName @s)
  fromFieldName = undefined

instance GEq (FooKey s) where
  geq I I = Just Refl

instance GCompare (FooKey s) where
  gcompare I I = GEQ

instance FromJSONKey (FooKey s) f where
  -- parseJSONKey :: forall k a. FromJSON a => Value -> Parser a
  parseJSONKey _ = parseJSON

instance ToJSONKey (FooKey s) Identity where
  toJSONKey I = toJSON

data L (key :: Symbol -> * -> *) a where
  L :: KnownSymbol s => k s a -> L k a

instance GKey (L FooKey) where
  toFieldName (L ksa) = toFieldName ksa
  -- fromFieldName t = fmap (\case (This (fk :: KnownSymbol s => FooKey s a)) -> This (L fk)) $ toKey t

instance GEq (L FooKey) where
  geq (L I) (L I) = Just Refl

instance GCompare (L FooKey) where
  gcompare (L I) (L I) = GEQ

data BarKey a where
  S :: BarKey Int

instance (GKey k, ToJSONKey k f) => ToJSON (DMap k f) where
  toJSON dm =
    let
      toPair (k :=> v) = (toFieldName k, toJSONKey k v)
    in
      object . fmap toPair . toList $ dm

mkParseJSON
  :: (GKey k, GCompare k, Applicative f, FromJSONKey k f)
  => String
  -> Value
  -> Parser (DMap k f)
mkParseJSON t = withObject t $ \o ->
  let
    add :: Some k -> Parser (DMap k f) -> Parser (DMap k f)
    add (This k) pDm =
      maybe id (insert k . pure) <$> parseJSONKey (o .:? toFieldName k) <*> pDm
  in
    foldr add (pure empty) keys


symName
  :: forall name. KnownSymbol name => String
symName =
  symbolVal' (proxy# :: (Proxy# name))
