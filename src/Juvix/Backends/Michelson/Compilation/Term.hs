module Juvix.Backends.Michelson.Compilation.Term where

import Juvix.Backends.Michelson.Parameterisation
import qualified Juvix.Core.Erased.Types as J
import Juvix.Library
import qualified Michelson.Untyped as M

termToInstr ∷ ∀ m. (Monad m) ⇒ Term → m Op
termToInstr term =
  case term of
    J.Var _ → undefined
    J.Prim prim →
      case prim of
        PrimVal () → pure (M.PrimEx (M.PUSH "" (M.Type M.TUnit "") (M.ValueUnit)))
    J.Lam _ _ → undefined
    J.App _ _ → undefined
