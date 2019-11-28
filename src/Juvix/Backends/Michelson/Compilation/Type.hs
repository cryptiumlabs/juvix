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
  m M.Type
typeToTypeLam ty =
  case ty of
    J.SymT _ → throw @"compilationError" InvalidInputType
    J.Star _ → throw @"compilationError" InvalidInputType
    J.PrimTy (PrimTy mTy) → pure mTy
    J.Pi _ argTy retTy → do
      argTy ← typeToTypeLam argTy
      retTy ← typeToTypeLam retTy
      let closureTy = M.Type M.TUnit ""
      pure (M.Type (M.TPair "" "" closureTy (M.Type (M.TLambda (M.Type (M.TPair "" "" closureTy argTy) "") retTy) "")) "")
