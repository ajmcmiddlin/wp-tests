{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.GADT.Aeson.TH where

import           Data.Aeson           (FromJSON (..), ToJSON (..),
                                       Value (Bool, String), object, withObject,
                                       withText, (.:))
import           Data.Aeson.Types     (FromJSON1, Result (..), ToJSON1 (..),
                                       parse, parseJSON1, toJSON1)
import           Data.Dependent.Map   (DMap)
import           Data.Dependent.Sum   (EqTag (..), ShowTag (..))
import           Data.Functor.Classes (Eq1, Show1, eq1, showsPrec1)
import           Data.GADT.Aeson      (EqViaKey (..), FromJSONViaKey (..),
                                       GKey (..), ToJSONViaKey (..),
                                       mkParseJSON, symName, toJSONDMap)

import           Language.Haskell.TH

-- instance FromJSON1 f => FromJSONViaKey {NAME} f where
--   parseJSONViaKey {CONSTRUCTOR} = parseJSON1
deriveFromJSONViaKey ::
  Name
  -> DecsQ
deriveFromJSONViaKey n =
  deriveClassForGADT ''FromJSONViaKey ''FromJSON1 n 'parseJSONViaKey 'parseJSON1

deriveToJSONViaKey ::
  Name
  -> DecsQ
deriveToJSONViaKey n =
  deriveClassForGADT ''ToJSONViaKey ''ToJSON1 n 'toJSONViaKey 'toJSON

deriveShowTag ::
  Name
  -> DecsQ
deriveShowTag n =
  deriveClassForGADT ''ShowTag ''Show1 n 'showTaggedPrec 'showsPrec1

deriveClassForGADT ::
  Name
  -> Name
  -> Name
  -> Name
  -> Name
  -> DecsQ
deriveClassForGADT klass ctx ty method f = do
  keyType <- reify ty
  let
    mkEq conName = clause [conP conName []] (normalB (varE f)) []
    mkDecl = \case
      (GadtC [conName] _bangTypes _ty) -> mkEq conName
      _ -> fail "Can only deriveFromJSONViaKey with GADT constructors"
    decl = case keyType of
      TyConI (DataD _ctx _n _tyvars _kind cons _deriving) ->
        funD method $ fmap mkDecl cons
      _ -> fail "Can only deriveFromJSONViaKey with a type constructor"
  f' <- varT <$> newName "f"
  let c = cxt [appT (conT ctx) f']
  pure <$> instanceD c (foldl appT (conT klass) [conT ty, f']) [decl]
