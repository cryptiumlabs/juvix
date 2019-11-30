module Juvix.Backends.Michelson.Compilation.Type where

import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Backends.Michelson.Parameterisation
import qualified Juvix.Core.ErasedAnn.Types as J
import Juvix.Library hiding (Type)
import qualified Michelson.Untyped as M
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

-- No free variables - ()
-- Free variables: nested pair of free variables in order, finally ().
closureType ∷ [(Symbol, M.Type)] → M.Type
closureType [] = M.Type M.TUnit "closure terminator"
closureType ((n, x) : xs) = M.Type (M.TPair (M.ann $ show n) "" x (closureType xs)) ""

lamTy ∷ [(Symbol, M.Type)] → M.Type → M.Type → M.Type
lamTy env argTy retTy = M.Type (M.TLambda (M.Type (M.TPair "arg" "env" argTy (closureType env)) "arg with closure") retTy) "lambda"

lamRetTy ∷ [(Symbol, M.Type)] → M.Type → M.Type → (M.Type, M.Type)
lamRetTy env argTy retTy =
  let lTy = lamTy env argTy retTy
   in (lTy, M.Type (M.TPair "closure" "lambda" (closureType env) lTy) "lambda with closure")
