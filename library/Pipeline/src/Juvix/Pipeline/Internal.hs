module Juvix.Pipeline.Internal
  ( Error (..),
    toCore,
    contextToCore,
    defName,
    -- we export these functions to be able to call them stepwise from
    -- a testing place
    addSig,
    addDef,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Juvix.Core as Core
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.Context.Traverse as Context
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Frontend as Frontend
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Parser (ParserError)
import qualified Juvix.Library.Sexp as Sexp
import qualified Juvix.ToCore.FromFrontend as FF

data Error
  = PipelineErr Core.Error
  | ParseErr ParserError
  deriving (Show)

toCore :: [FilePath] -> IO (Either Error (Context.T Sexp.T Sexp.T Sexp.T))
toCore paths = do
  x <- Frontend.ofPath paths
  case x of
    Left er -> pure $ Left (ParseErr er)
    Right x -> do
      from <- Core.ofFrontend x
      case from of
        Left errr -> pure $ Left (PipelineErr errr)
        Right con -> pure $ Right con

contextToCore ::
  (Show primTy, Show primVal) =>
  Context.T Sexp.T Sexp.T Sexp.T ->
  P.Parameterisation primTy primVal ->
  Either (FF.Error primTy primVal) (FF.CoreDefs primTy primVal)
contextToCore ctx param = do
  FF.execEnv ctx param do
    let ordered = Context.recGroups ctx
    for_ ordered \grp -> do
      traverse_ addSig grp
      traverse_ addDef grp
    defs <- get @"core"
    pure $ FF.CoreDefs {defs, order = fmap Context.name <$> ordered}

addSig ::
  ( Show primTy, Show primVal,
    HasThrow "fromFrontendError" (FF.Error primTy primVal) m,
    HasReader "param" (P.Parameterisation primTy primVal) m,
    HasState "coreSigs" (FF.CoreSigsHR primTy primVal) m,
    HasState "patVars" (HM.HashMap IR.GlobalName IR.PatternVar) m
  ) =>
  Context.Entry Sexp.T Sexp.T Sexp.T ->
  m ()
addSig (Context.Entry x feDef) = do
  msig <- FF.transformSig x feDef
  for_ msig $ modify @"coreSigs" . HM.insertWith FF.mergeSigs x

addDef ::
  ( Show primTy, Show primVal,
    HasThrow "fromFrontendError" (FF.Error primTy primVal) m,
    HasReader "param" (P.Parameterisation primTy primVal) m,
    HasState "core" (HM.HashMap NameSymbol.T (FF.CoreDef primTy primVal)) m,
    HasState "coreSigs" (FF.CoreSigsHR primTy primVal) m,
    HasState "nextPatVar" IR.PatternVar m,
    HasState "patVars" (HM.HashMap IR.GlobalName IR.PatternVar) m
  ) =>
  Context.Entry Sexp.T Sexp.T Sexp.T ->
  m ()
addDef (Context.Entry x feDef) = do
  defs <- FF.transformDef x feDef
  for_ defs \def ->
    modify @"core" $ HM.insert (defName def) def

defName :: FF.CoreDef primTy primVal -> NameSymbol.T
defName = \case
  FF.CoreDef (IR.RawGDatatype (IR.RawDatatype {rawDataName = x})) -> x
  FF.CoreDef (IR.RawGDataCon (IR.RawDataCon {rawConName = x})) -> x
  FF.CoreDef (IR.RawGFunction (IR.RawFunction {rawFunName = x})) -> x
  FF.CoreDef (IR.RawGAbstract (IR.RawAbstract {rawAbsName = x})) -> x
  FF.SpecialDef x _ -> x
