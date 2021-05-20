{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parameterization and application of the LLVM backend primitives.
module Juvix.Backends.LLVM.Parameterization
  ( llvm,
  )
where

import Juvix.Backends.LLVM.Primitive
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import qualified LLVM.AST.Type as LLVM

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
      Param.stringVal = undefined,
      Param.intVal = undefined,
      Param.floatVal = undefined
    }
  where
    -- Typechecking of primitive values.
    hasType :: RawPrimVal -> Param.PrimType PrimTy -> Bool
    hasType t ty = case t of
      Add -> Param.check3Equal ty
      Sub -> Param.check3Equal ty

    -- The primitive LLVM types available to Juvix users.
    builtinTypes :: Param.Builtins PrimTy
    builtinTypes =
      [("LLVM.int8", LLVM.i8)]

    -- The primitive LLVM values available to Juvix users.
    builtinValues :: Param.Builtins RawPrimVal
    builtinValues =
      [ ("LLVM.add", Add),
        ("LLVM.sub", Sub)
      ]
