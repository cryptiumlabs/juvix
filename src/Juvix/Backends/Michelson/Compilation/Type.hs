-- |
-- - Functions for representation of types in the Michelson backend.
module Juvix.Backends.Michelson.Compilation.Type where

import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Backends.Michelson.Parameterisation
import qualified Juvix.Core.ErasedAnn.Types as J
import Juvix.Library hiding (Type)
import qualified Michelson.Untyped as M

{-
 - Convert parameterised core types to their equivalent in Michelson.
 -}
typeToType ∷
  ∀ m.
  (HasThrow "compilationError" CompilationError m) ⇒
  Type →
  m M.Type
typeToType ty =
  case ty of
    J.SymT _ → throw @"compilationError" InvalidInputType
    J.Star _ → throw @"compilationError" InvalidInputType
    J.PrimTy (PrimTy mTy) → pure mTy
    -- TODO ∷ Integrate usage information into this
    J.Pi _usages argTy retTy → do
      argTy ← typeToType argTy
      retTy ← typeToType retTy
      pure (M.Type (M.TLambda argTy retTy) "")

{-
 - Closure packing:
 - No free variables - ()
 - Free variables: nested pair of free variables in order, finally ().
 -}
closureType ∷ [(Symbol, M.Type)] → M.Type
closureType [] = M.Type M.TUnit ""
closureType ((_, x) : xs) = M.Type (M.TPair "" "" x (closureType xs)) ""

{-
 - Lambda types: (closure type, argument type) -> (return type)
 -}

lamType ∷ [(Symbol, M.Type)] → [(Symbol, M.Type)] → M.Type → M.Type
lamType argsPlusClosures extraArgs retTy =
  M.Type
    ( M.TLambda
        ( M.Type
            ( M.TPair
                ""
                ""
                (closureType argsPlusClosures)
                (closureType extraArgs)
            )
            ""
        )
        retTy
    )
    ""
{- TODO: Figure out how to add nice annotations without breaking equality comparisons. -}
