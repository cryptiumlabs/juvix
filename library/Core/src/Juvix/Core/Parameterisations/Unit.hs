{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.Parameterisations.Unit where

import qualified Juvix.Core.IR.Evaluator as E
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding ((<|>))
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

-- k: primitive type: unit
data Ty
  = Ty
  deriving (Show, Eq)

-- c: primitive constant and f: functions
data Val
  = Val
  deriving (Show, Eq)

hasType :: Val -> P.PrimType Ty -> Bool
hasType Val (Ty :| []) = True
hasType _ _ = False

instance P.CanApply Ty where
  arity _ = 0
  apply f xs = Left $ P.ExtraArguments f xs

instance P.CanApply Val where
  arity _ = 0
  apply f xs = Left $ P.ExtraArguments f xs

instance P.CanApply (P.TypedPrim Ty Val) where
  arity _ = 0
  apply f xs = Left $ P.ExtraArguments f xs

instance E.HasWeak Ty where weakBy' _ _ ty = ty

instance Monoid (IR.XVPrimTy ext Ty val) => E.HasSubstValue ext Ty val Ty where
  substValueWith _ _ _ ty = pure $ IR.VPrimTy' ty mempty

instance Monoid (IR.XPrimTy ext Ty val) => E.HasPatSubstTerm ext Ty val Ty where
  patSubstTerm' _ _ ty = pure $ IR.PrimTy' ty mempty

instance E.HasWeak Val where weakBy' _ _ val = val

instance Monoid (IR.XVPrim ext ty Val) => E.HasSubstValue ext ty Val Val where
  substValueWith _ _ _ val = pure $ IR.VPrim' val mempty

instance Monoid (IR.XPrim ext Ty Val) => E.HasPatSubstTerm ext Ty Val Val where
  patSubstTerm' _ _ val = pure $ IR.Prim' val mempty

builtinTypes :: P.Builtins Ty
builtinTypes = [(["Unit"], Ty)]

builtinValues :: P.Builtins Val
builtinValues = [(["tt"], Val)]

t :: P.Parameterisation Ty Val
t =
  P.Parameterisation
    { hasType,
      builtinTypes,
      builtinValues,
      stringVal = const Nothing,
      intVal = const Nothing,
      floatVal = const Nothing
    }
