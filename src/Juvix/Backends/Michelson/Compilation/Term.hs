module Juvix.Backends.Michelson.Compilation.Term where

import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Backends.Michelson.Parameterisation
import qualified Juvix.Core.Erased.Types as J
import Juvix.Library
import qualified Michelson.Untyped as M

termToInstr ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m,
    HasWriter "compilationLog" [CompilationLog] m
  ) ⇒
  Term →
  m Op
termToInstr term = do
  let tellReturn ∷ Op → m Op
      tellReturn ret = tell @"compilationLog" [TermToInstr term ret] >> pure ret
      notYetImplemented ∷ m Op
      notYetImplemented = throw @"compilationError" (NotYetImplemented ("termToInstr: " <> show term))
  case term of
    J.Var _ → undefined
    J.Prim prim →
      case prim of
        PrimVal () → pure (M.PrimEx (M.PUSH "" (M.Type M.TUnit "") (M.ValueUnit)))
    J.Lam _ _ → undefined
    J.App _ _ → undefined
