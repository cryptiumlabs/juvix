{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Parameterization and application of the LLVM backend primitives.
module Juvix.Backends.LLVM.Parameterization
  ( llvm,
  )
where

import Juvix.Backends.LLVM.Primitive
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.IR.Evaluator as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import qualified LLVM.AST.Type as LLVM

instance Param.CanApply PrimTy where
  arity = arityTy

instance Param.CanApply (PrimVal ext) where
  arity val = case val of
    App.Cont {} -> App.numLeft val
    App.Return {} -> arityRaw $ App.retTerm val

-- | Parameters for the LLVM backend.
llvm :: Param.Parameterisation PrimTy RawPrimVal
llvm =
  Param.Parameterisation
    { Param.hasType = hasType,
      Param.builtinTypes = builtinTypes,
      Param.builtinValues = builtinValues,
      Param.stringVal = const Nothing,
      Param.intVal = integerToRawPrimVal,
      Param.floatVal = const Nothing
    }
  where
    -- Typechecking of primitive values.
    hasType :: RawPrimVal -> Param.PrimType PrimTy -> Bool
    hasType t (Param.PrimType ty) = case t of
      Add -> Param.check3Equal ty
      Sub -> Param.check3Equal ty
      LitInt _ -> length ty == 1

    -- The primitive LLVM types available to Juvix users.
    builtinTypes :: Param.Builtins PrimTy
    builtinTypes =
      [("LLVM.int8", PrimTy LLVM.i8)]

    -- The primitive LLVM values available to Juvix users.
    builtinValues :: Param.Builtins RawPrimVal
    builtinValues =
      [ ("LLVM.add", Add),
        ("LLVM.sub", Sub),
        ("LLVM.litint", LitInt 0) -- TODO: what to do with the 0?
      ]

    -- Translate an integer into a RawPrimVal.
    -- TODO: should we take type information into account? As of now, there is
    -- no way to do achieve this due to a lack of information available to the
    -- function.
    integerToRawPrimVal :: Integer -> Maybe RawPrimVal
    integerToRawPrimVal = Just . LitInt

-- | TODO: for now these are just copied over from the Michelson backend.
instance IR.HasWeak PrimTy where weakBy' _ _ t = t

instance IR.HasWeak RawPrimVal where weakBy' _ _ t = t

instance
  Monoid (IR.XVPrimTy ext PrimTy primVal) =>
  IR.HasSubstValue ext PrimTy primVal PrimTy
  where
  substValueWith _ _ _ t = pure $ IR.VPrimTy' t mempty

instance
  Monoid (IR.XPrimTy ext PrimTy primVal) =>
  IR.HasPatSubstTerm ext PrimTy primVal PrimTy
  where
  patSubstTerm' _ _ t = pure $ IR.PrimTy' t mempty

instance
  Monoid (IR.XPrim ext primTy RawPrimVal) =>
  IR.HasPatSubstTerm ext primTy RawPrimVal RawPrimVal
  where
  patSubstTerm' _ _ t = pure $ IR.Prim' t mempty
