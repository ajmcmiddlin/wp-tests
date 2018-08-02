{-# LANGUAGE TemplateHaskell #-}

module Data.GADT.Aeson.TH where

import           Data.Aeson          (FromJSON (..), ToJSON (..),
                                      Value (Bool, String), object, withObject,
                                      withText, (.:))
import           Data.Aeson.Types    (FromJSON1, Result (..), ToJSON1 (..),
                                      parse, parseJSON1, toJSON1)
import           Data.Dependent.Map  (DMap)
import           Data.GADT.Aeson     (EqViaKey (..), FromJSONViaKey (..),
                                      GKey (..), ToJSONViaKey (..), mkParseJSON,
                                      symName, toJSONDMap)

import           Language.Haskell.TH --(Q, reify, Name, DecQ, Info (DataConI), instanceD, Type (AppT, ForallT))

-- instance FromJSON1 f => FromJSONViaKey PostKey f where

deriveFromJSONViaKey ::
  Name
  -> DecQ
deriveFromJSONViaKey n = do
  keyType <- reify n
  let
    mkDecl (GadtC [conName] _bangTypes _ty) =
      funD (mkName "parseJSONViaKey")
    decls = case keyType of
      TyConI (DataD _ctx _n _tyvars _kind cons _deriving) ->
        fmap _makeDecl cons
  f <- varT <$> newName "f"
  let c = cxt [appT (conT ''FromJSON1) f]
  instanceD c (varT n) decls
