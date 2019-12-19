module Juvix.INetIR.JSON where

import qualified Data.Aeson as A
import Data.Char
import Data.JSON.Schema.Generator hiding (generate)
import Juvix.INetIR.Types
import Juvix.Library

instance JSONSchemaGen Port

instance (Typeable dataTy, JSONSchemaGen dataTy) ⇒ JSONSchemaGen (Kind dataTy)

instance (Typeable dataTy, JSONSchemaGen dataTy) ⇒ JSONSchemaGen (Node dataTy)

instance (Typeable opTy, JSONSchemaGen opTy) ⇒ JSONSchemaGen (BespokeFunction opTy)

instance (Typeable dataTy, JSONSchemaGen dataTy, Typeable opTy, JSONSchemaGen opTy) ⇒ JSONSchemaGen (Net dataTy opTy)

generate ∷ JSONSchemaGen a ⇒ Proxy a → A.Value
generate = convert options . toSchema defaultOptions

options ∷ A.Options
options =
  A.defaultOptions
    { A.fieldLabelModifier = (\(h : t) → toLower h : t) . dropWhile isLower,
      A.omitNothingFields = True,
      A.sumEncoding = A.ObjectWithSingleField
    }
