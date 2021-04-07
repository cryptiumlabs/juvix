module Juvix.Pipeline.Backend.Michelson (BMichelson (..)) where

import qualified Data.HashMap.Strict as HM
import qualified Juvix.Backends.Michelson.Compilation as M
import qualified Juvix.Backends.Michelson.Parameterisation as Param
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Pipeline as CorePipeline
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import Juvix.Pipeline.Backend.Internal
import qualified Juvix.Pipeline.Compile as Compile
import qualified Juvix.Pipeline.Internal as Pipeline
import qualified Juvix.Pipeline.Types as Types
import Juvix.ToCore.FromFrontend as FF

data BMichelson = BMichelson
  deriving (Eq, Show)

instance HasBackend BMichelson where
  type Ty BMichelson = Param.PrimTy
  type Val BMichelson = Param.RawPrimVal
  typecheck ctx = do
    let res = Pipeline.contextToCore ctx Param.michelson
    case res of
      Right (FF.CoreDefs _order globals) -> do
        let globalDefs = HM.mapMaybe Compile.toCoreDef globals
        case HM.elems $ HM.filter Compile.isMain globalDefs of
          [] -> Feedback.fail "No main function found"
          [IR.RawGFunction f]
            | IR.RawFunction _name usage ty (clause :| []) <- f,
              IR.RawFunClause _ [] term _ <- clause -> do
              let convGlobals = map (Compile.convGlobal Param.Set) globalDefs
                  newGlobals = HM.map (Compile.unsafeEvalGlobal convGlobals) convGlobals
                  lookupGlobal = IR.rawLookupFun' globalDefs
                  inlinedTerm = IR.inlineAllGlobals term lookupGlobal
              (res, _) <- liftIO $ Types.exec (CorePipeline.coreToAnn @Param.PrimTy @Param.RawPrimVal @Param.CompilationError inlinedTerm (IR.globalToUsage usage) ty) Param.michelson newGlobals
              case res of
                Right r -> do
                  pure r
                Left err -> do
                  print term
                  Feedback.fail $ show err
          somethingElse -> do
            Feedback.fail $ show somethingElse
      Left err -> do
        Feedback.fail $ "failed at ctxToCore\n" ++ show err

  compile term = do
    let (res, _logs) = M.compileContract $ CorePipeline.toRaw term
    case res of
      Right c -> do
        return $ M.untypedContractToSource (fst c)
      Left err -> Feedback.fail $ show err
