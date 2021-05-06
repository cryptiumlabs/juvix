{-# LANGUAGE UndecidableInstances #-}

module Juvix.Backends.Plonk.Pipeline
  ( BPlonk (..),
  )
where

import qualified Data.Aeson as A
import Data.Field.Galois (GaloisField)
import qualified Data.HashMap.Strict as HM
import qualified Juvix.Backends.Plonk.Builder as Builder
import qualified Juvix.Backends.Plonk.Circuit as Circuit
import qualified Juvix.Backends.Plonk.Compiler as Compiler
import qualified Juvix.Backends.Plonk.Dot as Dot
import qualified Juvix.Backends.Plonk.Parameterization as Parameterization
import qualified Juvix.Backends.Plonk.Types as Types
import qualified Juvix.Core.Application as CoreApp
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.IR.Typechecker.Types as TypeChecker
import Juvix.Core.Parameterisation
  ( CanApply (ApplyErrorExtra, Arg),
    TypedPrim,
  )
import qualified Juvix.Core.Pipeline as CorePipeline
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import Juvix.Pipeline as Pipeline
import Juvix.ToCore.FromFrontend as FF (CoreDefs (..))
import qualified Text.PrettyPrint.Leijen.Text as Pretty

data BPlonk f = BPlonk
  deriving (Eq, Show)

instance
  ( GaloisField f,
    Eq f,
    Integral f,
    CanApply
      ( CoreApp.Return'
          IR.NoExt
          (NonEmpty (Types.PrimTy f))
          (Types.PrimVal f)
      ),
    CanApply (Types.PrimTy f),
    IR.HasPatSubstTerm
      (OnlyExts.T IR.NoExt)
      (Types.PrimTy f)
      ( CoreApp.Return'
          IR.NoExt
          (NonEmpty (Types.PrimTy f))
          (Types.PrimVal f)
      )
      (Types.PrimTy f),
    IR.HasWeak (Types.PrimVal f),
    IR.HasSubstValue
      IR.NoExt
      (Types.PrimTy f)
      ( CoreApp.Return'
          IR.NoExt
          (NonEmpty (Types.PrimTy f))
          (Types.PrimVal f)
      )
      (Types.PrimTy f),
    IR.HasPatSubstTerm
      (OnlyExts.T IR.NoExt)
      (Types.PrimTy f)
      (Types.PrimVal f)
      (Types.PrimTy f),
    IR.HasPatSubstTerm
      (OnlyExts.T IR.NoExt)
      (Types.PrimTy f)
      (Types.PrimVal f)
      (Types.PrimVal f),
    IR.HasPatSubstTerm
      (OnlyExts.T TypeChecker.T)
      (Types.PrimTy f)
      (TypedPrim (Types.PrimTy f) (Types.PrimVal f))
      (Types.PrimTy f),
    Show (Arg (Types.PrimTy f)),
    Show (ApplyErrorExtra (Types.PrimTy f)),
    Show
      (ApplyErrorExtra (TypedPrim (Types.PrimTy f) (Types.PrimVal f))),
    A.ToJSON (Circuit.ArithCircuit f)
  ) =>
  HasBackend (BPlonk f)
  where
  type Ty (BPlonk f) = Types.PrimTy f
  type Val (BPlonk f) = Types.PrimVal f
  stdlibs _ = ["stdlib/Circuit.ju"]
  typecheck ctx = do
    let res = Pipeline.contextToCore ctx (Parameterization.param @f)
    case res of
      Right (FF.CoreDefs _order globals) -> do
        let globalDefs = HM.mapMaybe Pipeline.toCoreDef globals
        case HM.elems $ HM.filter Pipeline.isMain globalDefs of
          [] -> Feedback.fail "No main function found"
          [IR.RawGFunction f]
            | IR.RawFunction _name usage ty (clause :| []) <- f,
              IR.RawFunClause _ [] term _ <- clause -> do
              let convGlobals = map (Pipeline.convGlobal Types.PField) globalDefs
                  newGlobals = HM.map (Pipeline.unsafeEvalGlobal convGlobals) convGlobals
                  lookupGlobal = IR.rawLookupFun' globalDefs
                  inlinedTerm = IR.inlineAllGlobals term lookupGlobal
              (res, _) <- liftIO $ Pipeline.exec (CorePipeline.coreToAnn @(Types.PrimTy f) @(Types.PrimVal f) @Types.CompilationError inlinedTerm (IR.globalToUsage usage) ty) (Parameterization.param @f) newGlobals
              case res of
                Right r -> do
                  pure r
                Left err -> do
                  print term
                  Feedback.fail $ show err
          somethingElse -> Feedback.fail $ show somethingElse
      Left err -> Feedback.fail $ "failed at ctxToCore\n" ++ show err

  compile out term = do
    let circuit = Builder.execCircuitBuilder . Compiler.compileTermWithWire $ CorePipeline.toRaw term
    let pretty = toS . Pretty.displayT . Pretty.renderPretty 1 120 . Pretty.pretty
    let json = show $ A.encode circuit
    liftIO $ Dot.dotWriteSVG out (Dot.arithCircuitToDot circuit)
    writeout (out <> ".pretty") $ pretty circuit
    writeout (out <> ".json") json
