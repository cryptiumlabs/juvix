module Juvix.Backends.Michelson.Compilation.Type where

import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Backends.Michelson.Parameterisation
import qualified Juvix.Core.ErasedAnn.Types as J
import Juvix.Library hiding (Type)
import qualified Michelson.Untyped.Type as M

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
    J.Pi _ argTy retTy → do
      argTy ← typeToType argTy
      retTy ← typeToType retTy
      pure (M.Type (M.TLambda argTy retTy) "")

typeToTypeLam ∷
  ∀ m.
  (HasThrow "compilationError" CompilationError m) ⇒
  Type →
  [[M.Type]] →
  m M.Type
typeToTypeLam ty closureTypes =
  case ty of
    J.SymT _ → throw @"compilationError" InvalidInputType
    J.Star _ → throw @"compilationError" InvalidInputType
    J.PrimTy (PrimTy mTy) → pure mTy
    J.Pi _ argTy retTy → do
      case closureTypes of
        [] → throw @"compilationError" InvalidInputType
        ty : tys → do
          -- TODO: We also need to deal with closures in the argument. Maybe we need a new `Type` data structure.
          argTy ← typeToTypeLam argTy []
          retTy ← typeToTypeLam retTy tys
          let closureTy = closureType ty
          pure (M.Type (M.TPair "" "" closureTy (M.Type (M.TLambda (M.Type (M.TPair "" "" argTy closureTy) "") retTy) "")) "")

-- No free variables - ()
-- Free variables: nested pair of free variables in order, finally ().
closureType ∷ [M.Type] → M.Type
closureType [] = M.Type M.TUnit ""
closureType (x : xs) = M.Type (M.TPair "" "" x (closureType xs)) ""
