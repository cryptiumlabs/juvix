module Juvix.Backends.Michelson.Compilation where

import qualified Data.Map as Map
import qualified Data.Text.Lazy as L
import Juvix.Backends.Michelson.Compilation.Term
import Juvix.Backends.Michelson.Compilation.Type
import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Backends.Michelson.Compilation.Util
import Juvix.Backends.Michelson.Optimisation
import Juvix.Backends.Michelson.Parameterisation
import Juvix.Library hiding (Type)
import qualified Michelson.Printer as M
import qualified Michelson.TypeCheck as M
import qualified Michelson.Untyped as M

-- TODO: We might want to do this in a different way to preserve annotations.
contractToSource ∷ M.SomeContract → Text
contractToSource (M.SomeContract instr _ _) = L.toStrict (M.printTypedContract instr)

compileToMichelson ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m,
    HasWriter "compilationLog" [CompilationLog] m
  ) ⇒
  Term →
  Type →
  m (M.SomeContract)
compileToMichelson term ty = do
  michelsonOp' ← termToInstr term
  let michelsonOp = leftSeq michelsonOp'
  michelsonTy ← typeToType ty
  case michelsonTy of
    M.Type (M.TLambda argTy retTy) _ → do
      let contract = M.Contract argTy retTy [michelsonOp]
      case M.typeCheckContract Map.empty contract of
        Right (M.SomeContract instr start end) → do
          optimised ← optimise instr
          pure (M.SomeContract optimised start end)
        Left err → throw @"compilationError" (DidNotTypecheck err)
    _ → throw @"compilationError" InvalidInputType
