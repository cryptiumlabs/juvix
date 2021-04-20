{-# LANGUAGE UndecidableInstances #-}

module Juvix.Pipeline.Backend.Plonk
  ( BPlonk (..),
  )
where

import Data.Field.Galois (GaloisField)
import qualified Data.HashMap.Strict as HM
import qualified Juvix.Backends.Plonk as Plonk
import qualified Juvix.Core.Application as CoreApp
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.IR.Typechecker.Types as TypeChecker
import qualified Data.Aeson as A
import Juvix.Core.Parameterisation
  ( CanApply (ApplyErrorExtra, Arg),
    TypedPrim,
  )
import qualified Juvix.Core.Pipeline as CorePipeline
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import Juvix.Pipeline.Backend.Internal (HasBackend (..), writeout)
import Juvix.Pipeline.Compile
  ( convGlobal,
    isMain,
    toCoreDef,
    unsafeEvalGlobal,
  )
import Juvix.Pipeline.Internal as Pipeline
import Juvix.Pipeline.Types (exec)
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
          (NonEmpty (Plonk.PrimTy f))
          (Plonk.PrimVal f)
      ),
    CanApply (Plonk.PrimTy f),
    IR.HasPatSubstTerm
      (OnlyExts.T IR.NoExt)
      (Plonk.PrimTy f)
      ( CoreApp.Return'
          IR.NoExt
          (NonEmpty (Plonk.PrimTy f))
          (Plonk.PrimVal f)
      )
      (Plonk.PrimTy f),
    IR.HasWeak (Plonk.PrimVal f),
    IR.HasSubstValue
      IR.NoExt
      (Plonk.PrimTy f)
      ( CoreApp.Return'
          IR.NoExt
          (NonEmpty (Plonk.PrimTy f))
          (Plonk.PrimVal f)
      )
      (Plonk.PrimTy f),
    IR.HasPatSubstTerm
      (OnlyExts.T IR.NoExt)
      (Plonk.PrimTy f)
      (Plonk.PrimVal f)
      (Plonk.PrimTy f),
    IR.HasPatSubstTerm
      (OnlyExts.T IR.NoExt)
      (Plonk.PrimTy f)
      (Plonk.PrimVal f)
      (Plonk.PrimVal f),
    IR.HasPatSubstTerm
      (OnlyExts.T TypeChecker.T)
      (Plonk.PrimTy f)
      (TypedPrim (Plonk.PrimTy f) (Plonk.PrimVal f))
      (Plonk.PrimTy f),
    Show (Arg (Plonk.PrimTy f)),
    Show
      (Arg (TypedPrim (Plonk.PrimTy f) (Plonk.PrimVal f))),
    Show (ApplyErrorExtra (Plonk.PrimTy f)),
    Show
      (ApplyErrorExtra (TypedPrim (Plonk.PrimTy f) (Plonk.PrimVal f))),
    A.ToJSON (Plonk.ArithCircuit f)
  ) =>
  HasBackend (BPlonk f)
  where
  type Ty (BPlonk f) = Plonk.PrimTy f
  type Val (BPlonk f) = Plonk.PrimVal f
  stdlibs _ = ["stdlib/Circuit.ju"]
  typecheck ctx = do
    let res = Pipeline.contextToCore ctx (Plonk.plonk @f)
    case res of
      Right (FF.CoreDefs _order globals) -> do
        let globalDefs = HM.mapMaybe toCoreDef globals
        case HM.elems $ HM.filter isMain globalDefs of
          [] -> Feedback.fail "No main function found"
          [IR.RawGFunction f]
            | IR.RawFunction _name usage ty (clause :| []) <- f,
              IR.RawFunClause _ [] term _ <- clause -> do
              let convGlobals = map (convGlobal Plonk.PField) globalDefs
                  newGlobals = HM.map (unsafeEvalGlobal convGlobals) convGlobals
                  lookupGlobal = IR.rawLookupFun' globalDefs
                  inlinedTerm = IR.inlineAllGlobals term lookupGlobal
              (res, _) <- liftIO $ exec (CorePipeline.coreToAnn @(Plonk.PrimTy f) @(Plonk.PrimVal f) @Plonk.CompilationError inlinedTerm (IR.globalToUsage usage) ty) (Plonk.plonk @f) newGlobals
              case res of
                Right r -> do
                  pure r
                Left err -> do
                  print term
                  Feedback.fail $ show err
          somethingElse -> Feedback.fail $ show somethingElse
      Left err -> Feedback.fail $ "failed at ctxToCore\n" ++ show err

  compile out term = do
    let circuit = Plonk.execCircuitBuilder . Plonk.compileTermWithWire $ CorePipeline.toRaw term
    let pretty = toS . Pretty.displayT . Pretty.renderPretty 1 120 . Pretty.pretty
    let json = show $ A.encode circuit
    liftIO $ Plonk.dotWriteSVG out (Plonk.arithCircuitToDot circuit)
    writeout (out <> ".pretty") $ pretty circuit
    writeout (out <> ".json") json
