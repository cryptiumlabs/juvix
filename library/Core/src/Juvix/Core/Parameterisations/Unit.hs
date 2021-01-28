{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.Parameterisations.Unit where

import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.IR.Evaluator as E
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

instance
  ( E.HasWeak val,
    Monoid (IR.XAnn ext Ty val),
    Monoid (IR.XPrimTy ext Ty val),
    Monoid (IR.XStar ext Ty val)
  ) =>
  E.HasPatSubstElim ext Ty val Ty
  where
  patSubstElim' _ _ ty =
    pure $ IR.Ann' mempty (IR.PrimTy' ty mempty) (IR.Star' 0 mempty) 1 mempty

instance E.HasWeak Val where weakBy' _ _ val = val

instance Monoid (IR.XVPrim ext ty Val) => E.HasSubstValue ext ty Val Val where
  substValueWith _ _ _ val = pure $ IR.VPrim' val mempty

instance
  ( Monoid (IR.XAnn ext Ty Val),
    Monoid (IR.XPrim ext Ty Val),
    Monoid (IR.XPrimTy ext Ty Val)
  ) =>
  E.HasPatSubstElim ext Ty Val Val
  where
  patSubstElim' _ _ val =
    let ty = IR.PrimTy' Ty mempty in
    pure $ IR.Ann' Usage.Omega (IR.Prim' val mempty) ty 0 mempty

parseTy :: Token.GenTokenParser String () Identity -> Parser Ty
parseTy lexer = do
  Token.reserved lexer "Unit"
  pure Ty

parseVal :: Token.GenTokenParser String () Identity -> Parser Val
parseVal lexer = do
  Token.reserved lexer "tt"
  pure Val

reservedNames :: [String]
reservedNames = ["Unit", "tt"]

reservedOpNames :: [String]
reservedOpNames = []

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
      parseTy,
      parseVal,
      reservedNames,
      reservedOpNames,
      stringTy = \_ _ -> False,
      stringVal = const Nothing,
      intTy = \_ _ -> False,
      intVal = const Nothing,
      floatTy = \_ _ -> False,
      floatVal = const Nothing
    }
