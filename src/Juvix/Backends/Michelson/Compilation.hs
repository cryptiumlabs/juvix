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

typedContractToSource ∷ M.SomeContract → Text
typedContractToSource (M.SomeContract instr _ _) = L.toStrict (M.printTypedContract instr)

untypedContractToSource ∷ M.Contract' M.ExpandedOp → Text
untypedContractToSource c = L.toStrict (M.printUntypedContract c)

compile ∷ Term → Type → (Either CompilationError (M.Contract' M.ExpandedOp, M.SomeContract), [CompilationLog])
compile term ty =
  let (ret, env) = execWithStack [] (compileToMichelson term ty)
   in (ret, compilationLog env)

compileToMichelson ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m,
    HasWriter "compilationLog" [CompilationLog] m
  ) ⇒
  Term →
  Type →
  m (M.Contract' M.ExpandedOp, M.SomeContract)
compileToMichelson term ty = do
  michelsonTy ← typeToType ty
  case michelsonTy of
    M.Type (M.TLambda argTy@(M.Type (M.TPair _ _ paramTy storageTy) _) _) _ → do
      michelsonOp' ← termToMichelson term argTy
      let michelsonOp = leftSeq michelsonOp'
      let contract = M.Contract paramTy storageTy [michelsonOp]
      case M.typeCheckContract Map.empty contract of
        Right _ → do
          optimised ← optimise michelsonOp
          let optimisedContract = M.Contract paramTy storageTy [optimised]
          case M.typeCheckContract Map.empty optimisedContract of
            Right c → pure (optimisedContract, c)
            Left err → throw @"compilationError" (DidNotTypecheckAfterOptimisation err)
        Left err → throw @"compilationError" (DidNotTypecheck err)
    _ → throw @"compilationError" InvalidInputType
