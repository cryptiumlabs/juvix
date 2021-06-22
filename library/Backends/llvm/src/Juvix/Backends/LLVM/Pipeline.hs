-- | The basic connection between the backend and the Juvix pipeline.
module Juvix.Backends.LLVM.Pipeline
  ( BLLVM (..),
  )
where

import qualified Data.HashMap.Strict as HM
import Juvix.Backends.LLVM.Compilation
import Juvix.Backends.LLVM.Parameterization
import Juvix.Backends.LLVM.Primitive
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Pipeline as Core
import Juvix.Library
import Juvix.Library.Feedback
import qualified Juvix.Pipeline as Pipeline
import qualified Juvix.ToCore.FromFrontend as FF

-- | Identifier for the LLVM backend.
data BLLVM = BLLVM
  deriving (Show, Eq)

instance Pipeline.HasBackend BLLVM where
  type Ty BLLVM = PrimTy
  type Val BLLVM = RawPrimVal

  stdlibs _ = ["stdlib/LLVM.ju"]

  -- Copied over from the Michelson backend, and adapter where necessary.
  typecheck ctx = do
    let res = Pipeline.contextToCore ctx llvm
    case res of
      Right (FF.CoreDefs _order globals) -> do
        let globalDefs = HM.mapMaybe Pipeline.toCoreDef globals
        -- Find main function.
        case HM.elems $ HM.filter Pipeline.isMain globalDefs of
          [] -> fail "No main function found"
          -- Match a single function, there should only be one main.
          [IR.RawGFunction f]
            -- Get the RawFunction and RawFunClause from the global function.
            | IR.RawFunction _name usage ty (clause :| []) <- f,
              IR.RawFunClause _ [] term _ <- clause -> do
              let convGlobals = map (Pipeline.convGlobal Set) globalDefs
                  newGlobals = HM.map (Pipeline.unsafeEvalGlobal convGlobals) convGlobals
                  lookupGlobal = IR.rawLookupFun' globalDefs
                  inlinedTerm = IR.inlineAllGlobals term lookupGlobal
              (res, _) <- liftIO $ Pipeline.exec (Core.coreToAnn @PrimTy @RawPrimVal @CompilationError inlinedTerm (IR.globalToUsage usage) ty) llvm newGlobals
              case res of
                Right r -> do
                  pure r
                Left err -> do
                  print term
                  fail $ show err
          somethingElse -> do
            fail $ "Typechecker Failed: " ++ show somethingElse
      Left err -> do
        fail $ "failed at ctxToCore\n" ++ show err

  compile out term = do
    let raw = Core.toRaw term
    code <- compileProgram raw
    Pipeline.writeout out code
