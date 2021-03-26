{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Juvix.Pipeline.Backend.Michelson where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import qualified Data.Text.IO as T
import qualified Juvix.Backends.Michelson.Compilation as M
import qualified Juvix.Backends.Michelson.Parameterisation as Param
import qualified Juvix.Core.Application as CoreApp
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import Juvix.ToCore.FromFrontend as FF
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.IR.Typechecker.Types as TypeChecker
import Juvix.Core.IR.Types.Base
import Juvix.Core.IR.Types.Globals
import Juvix.Core.Parameterisation
import qualified Juvix.Core.Pipeline as CorePipeline
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Pipeline.Internal as Pipeline
import Juvix.Pipeline.Types
import Juvix.Pipeline.Backend.Internal
import Juvix.Pipeline.Compile
import qualified System.IO.Temp as Temp
import qualified Prelude as P

data BMichelson = BMichelson
  deriving (Eq, Show)

instance HasBackend BMichelson where
  type Ty BMichelson = Param.PrimTy
  type Val BMichelson = Param.RawPrimVal
  typecheck ctx = do
    let res = Pipeline.contextToCore ctx Param.michelson
    case res of
      Right (FF.CoreDefs _order globals) -> do
        let globalDefs = HM.mapMaybe toCoreDef globals
        case HM.elems $ HM.filter isMain globalDefs of
          [] -> Feedback.fail "No main function found"
          [IR.RawGFunction f]
            | IR.RawFunction _name usage ty (clause :| []) <- f,
              IR.RawFunClause _ [] term _ <- clause -> do
              let convGlobals = map (convGlobal Param.Set) globalDefs
                  newGlobals = HM.map (unsafeEvalGlobal convGlobals) convGlobals
                  lookupGlobal = IR.rawLookupFun' globalDefs
                  inlinedTerm = IR.inlineAllGlobals term lookupGlobal
              (res, _) <- liftIO $ exec (CorePipeline.coreToAnn inlinedTerm (IR.globalToUsage usage) ty) Param.michelson newGlobals
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
  typecheck _ =
    Feedback.fail $ "Typecheck not implemented for Michelson backend."

  compile term = do
    let (res, _logs) = M.compileContract $ CorePipeline.toRaw term
    case res of
      Right c -> do
        return $ M.untypedContractToSource (fst c)
      Left err -> Feedback.fail $ show err

