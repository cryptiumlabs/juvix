{-# LANGUAGE UndecidableInstances #-}

module Juvix.Backends.Plonk.Pipeline
  ( BPlonk (..),
    compileCircuit,
  )
where

import qualified Data.Aeson as A
import Data.Field.Galois (GaloisField)
import qualified Juvix.Backends.Plonk.Builder as Builder
import qualified Juvix.Backends.Plonk.Circuit as Circuit
import qualified Juvix.Backends.Plonk.Compiler as Compiler
import qualified Juvix.Backends.Plonk.Dot as Dot
import qualified Juvix.Backends.Plonk.Parameterization as Parameterization
import qualified Juvix.Backends.Plonk.Types as Types
import qualified Juvix.Core.ErasedAnn.Types as CoreErased
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.IR.Typechecker.Types as TypeChecker
import Juvix.Core.Parameterisation
  ( CanApply (ApplyErrorExtra, Arg),
    TypedPrim,
  )
import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.Pipeline as CorePipeline
import Juvix.Library
import Juvix.Pipeline as Pipeline
import qualified Text.PrettyPrint.Leijen.Text as Pretty

data BPlonk f = BPlonk
  deriving (Eq, Show, Read)

instance
  ( GaloisField f,
    Eq f,
    Integral f,
    CanApply (Param.TypedPrim (Types.PrimTy f) (Types.PrimVal f)),
    CanApply (Types.PrimTy f),
    IR.HasPatSubstTerm
      (OnlyExts.T IR.T)
      (Types.PrimTy f)
      (Param.TypedPrim (Types.PrimTy f) (Types.PrimVal f))
      (Types.PrimTy f),
    IR.HasWeak (Types.PrimVal f),
    IR.HasSubstValue
      IR.T
      (Types.PrimTy f)
      (Param.TypedPrim (Types.PrimTy f) (Types.PrimVal f))
      (Types.PrimTy f),
    IR.HasPatSubstTerm
      (OnlyExts.T IR.T)
      (Types.PrimTy f)
      (Types.PrimVal f)
      (Types.PrimTy f),
    IR.HasPatSubstTerm
      (OnlyExts.T IR.T)
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
  type Err (BPlonk f) = Types.CompilationError f
  stdlibs _ = ["stdlib/Circuit.ju"]
  typecheck ctx = Pipeline.typecheck' ctx (Parameterization.param @f) Types.PField
  compile out term = do
    let circuit = compileCircuit term
    liftIO $ Dot.dotWriteSVG out (Dot.arithCircuitToDot circuit)
    writeout (out <> ".pretty") $
      let pretty = toS . Pretty.displayT . Pretty.renderPretty 1 120 . Pretty.pretty
       in pretty circuit
    writeout (out <> ".json") $
      let json = show $ A.encode circuit
       in json

compileCircuit ::
  (Integral f, Show f) =>
  CoreErased.AnnTerm
    (Types.PrimTy f)
    (CoreErased.TypedPrim (Types.PrimTy f) (Types.PrimVal f)) ->
  Circuit.ArithCircuit f
compileCircuit term = Builder.execCircuitBuilder . Compiler.compileTermWithWire $ CorePipeline.toRaw term
