module Juvix.Backends.Michelson.Pipeline (BMichelson (..)) where

import qualified Data.HashMap.Strict as HM
import qualified Juvix.Backends.Michelson.Compilation as M
import qualified Juvix.Backends.Michelson.Parameterisation as Param
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Pipeline as CorePipeline
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import Juvix.Pipeline as Pipeline
import Juvix.ToCore.FromFrontend as FF (CoreDefs (..))

data BMichelson = BMichelson
  deriving (Eq, Show)

instance HasBackend BMichelson where
  type Ty BMichelson = Param.PrimTy
  type Val BMichelson = Param.RawPrimVal
  stdlibs _ = ["stdlib/Michelson.ju", "stdlib/MichelsonAlias.ju"]

  typecheck ctx = do
    let res = Pipeline.contextToCore ctx Param.michelson
    case res of
      Right (FF.CoreDefs _order globals) -> do
        let globalDefs = HM.mapMaybe Pipeline.toCoreDef globals
        case HM.elems $ HM.filter Pipeline.isMain globalDefs of
          [] -> Feedback.fail "No main function found"
          [IR.RawGFunction f]
            | IR.RawFunction _name usage ty (clause :| []) <- f,
              IR.RawFunClause _ [] term _ <- clause -> do
              let convGlobals = map (Pipeline.convGlobal Param.Set) globalDefs
                  newGlobals = HM.map (Pipeline.unsafeEvalGlobal convGlobals) convGlobals
                  lookupGlobal = IR.rawLookupFun' globalDefs
                  inlinedTerm = IR.inlineAllGlobals term lookupGlobal
              (res, _) <- liftIO $ Pipeline.exec (CorePipeline.coreToAnn @Param.PrimTy @Param.RawPrimVal @Param.CompilationError inlinedTerm (IR.globalToUsage usage) ty) Param.michelson newGlobals
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

  compile out term = do
    let (res, _logs) = M.compileContract $ CorePipeline.toRaw term
    case res of
      Right c -> do
        -- TODO: Maybe avoid having writeout. 
        -- Need some other interpretation for tests
        -- Maybe use effects 
        writeout out $ M.untypedContractToSource (fst c)
      Left err -> Feedback.fail $ show err
