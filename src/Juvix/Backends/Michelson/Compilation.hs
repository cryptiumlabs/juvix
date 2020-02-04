-- |
-- - Entrypoints into compilation from core terms to Michelson terms & contracts.
module Juvix.Backends.Michelson.Compilation where

import qualified Data.Map as Map
import qualified Data.Text.Lazy as L
import Juvix.Backends.Michelson.Compilation.Term
import Juvix.Backends.Michelson.Compilation.Lambda
import Juvix.Backends.Michelson.Compilation.Type
import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Backends.Michelson.Compilation.Util
import qualified Juvix.Backends.Michelson.Compilation.VirtualStack as VStack
import Juvix.Backends.Michelson.Optimisation
import Juvix.Backends.Michelson.Parameterisation
import Juvix.Library hiding (Type)
import qualified Michelson.Printer as M
import qualified Michelson.TypeCheck as M
import qualified Michelson.Typed as MT
import qualified Michelson.Untyped as M

typedContractToSource ∷ M.SomeContract → Text
typedContractToSource (M.SomeContract (MT.FullContract instr _ _)) =
  L.toStrict (M.printTypedContract False instr)

untypedContractToSource ∷ M.Contract' M.ExpandedOp → Text
untypedContractToSource c = L.toStrict (M.printUntypedContract False c)

compileContract ∷
  Term →
  Type →
  (Either CompilationError (M.Contract' M.ExpandedOp, M.SomeContract), [CompilationLog])
compileContract term ty =
  let (ret, env) = execWithStack VStack.nil (compileToMichelsonContract term ty)
   in (ret, compilationLog env)

compileExpr ∷ Term → Type → (Either CompilationError SomeInstr, [CompilationLog])
compileExpr term ty =
  let (ret, env) = execWithStack VStack.nil (compileToMichelsonExpr term ty)
   in (ret, compilationLog env)

compileToMichelsonContract ∷
  ∀ m.
  ( HasState "stack" VStack.T m,
    HasThrow "compilationError" CompilationError m,
    HasWriter "compilationLog" [CompilationLog] m
  ) ⇒
  Term →
  Type →
  m (M.Contract' M.ExpandedOp, M.SomeContract)
compileToMichelsonContract term ty = do
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

-- TODO: This shouldn't require being a function.
compileToMichelsonExpr ∷
  ∀ m.
  ( HasState "stack" VStack.T m,
    HasThrow "compilationError" CompilationError m,
    HasWriter "compilationLog" [CompilationLog] m
  ) ⇒
  Term →
  Type →
  m (SomeInstr)
compileToMichelsonExpr term ty = do
  michelsonTy ← typeToType ty
  case michelsonTy of
    M.Type (M.TLambda argTy@(M.Type (M.TPair _ _ paramTy storageTy) _) _) _ → do
      michelsonOp' ← termToMichelson term argTy
      let michelsonOp = leftSeq michelsonOp'
      MT.withSomeSingT (MT.fromUType argTy) $ \sty →
        case M.runTypeCheckIsolated (M.typeCheckList [michelsonOp] (sty M.-:& M.SNil)) of
          Right (_ M.:/ (s M.::: _)) → pure (SomeInstr s)
          -- TODO ∷ Figure out what this case should be
          Right (_ M.:/ (M.AnyOutInstr _)) → undefined
          Left err → throw @"compilationError" (DidNotTypecheck err)
