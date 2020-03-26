-- |
-- - Entrypoints into compilation from core terms to Michelson terms & contracts.
module Juvix.Backends.Michelson.Compilation where

import qualified Data.Map as Map
import qualified Data.Text.Lazy as L
import Juvix.Backends.Michelson.Compilation.Types
import qualified Juvix.Backends.Michelson.DSL.Environment as DSL
import qualified Juvix.Backends.Michelson.DSL.InstructionsEff as DSL
import qualified Juvix.Backends.Michelson.Optimisation as Optimisation
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
  (Either DSL.CompError (M.Contract' M.ExpandedOp, M.SomeContract), [CompilationLog])
compileContract term ty =
  let (ret, env) = DSL.execMichelson (compileToMichelsonContract term ty)
   in (ret, DSL.compilationLog env)

compileExpr ∷ Term → Type → (Either DSL.CompError SomeInstr, [CompilationLog])
compileExpr term ty =
  let (ret, env) = DSL.execMichelson (compileToMichelsonExpr term ty)
   in (ret, DSL.compilationLog env)

compileToMichelsonContract ∷
  DSL.Reduction m ⇒
  Term →
  Type →
  m (M.Contract' M.ExpandedOp, M.SomeContract)
compileToMichelsonContract term ty = do
  michelsonTy ← DSL.typeToPrimType ty
  case michelsonTy of
    M.Type (M.TLambda argTy@(M.Type (M.TPair _ _ paramTy storageTy) _) _) _ → do
      -- TODO: Figure out what happened to argTy.
      michelsonOp ← DSL.instOuter term
      let contract = M.Contract paramTy storageTy [michelsonOp]
      case M.typeCheckContract Map.empty contract of
        Right _ → do
          optimised ← Optimisation.optimise michelsonOp
          let optimisedContract = M.Contract paramTy storageTy [optimised]
          case M.typeCheckContract Map.empty optimisedContract of
            Right c → pure (optimisedContract, c)
            Left err → throw @"compilationError" (DidNotTypecheckAfterOptimisation err)
        Left err → throw @"compilationError" (DidNotTypecheck err)
    _ → throw @"compilationError" InvalidInputType

-- TODO: This shouldn't require being a function.
compileToMichelsonExpr ∷
  DSL.Reduction m ⇒
  Term →
  Type →
  m SomeInstr
compileToMichelsonExpr term ty = do
  michelsonTy ← DSL.typeToPrimType ty
  case michelsonTy of
    M.Type (M.TLambda argTy@(M.Type (M.TPair _ _ _paramTy _storageTy) _) _) _ → do
      -- TODO: Figure out what happened to argTy.
      michelsonOp ← DSL.instOuter term
      MT.withSomeSingT (MT.fromUType argTy) $ \sty →
        case M.runTypeCheckIsolated (M.typeCheckList [michelsonOp] (sty M.-:& M.SNil)) of
          Right (_ M.:/ (s M.::: _)) → pure (SomeInstr s)
          -- TODO ∷ Figure out what this case should be
          Right (_ M.:/ (M.AnyOutInstr _)) → undefined
          Left err → throw @"compilationError" (DidNotTypecheck err)
    M.Type a _ → do
      michelsonOp ← DSL.instOuter term
      undefined

runMichelsonExpr ∷ DSL.Reduction m ⇒ NewTerm → m M.ExpandedOp
runMichelsonExpr = DSL.instOuter
