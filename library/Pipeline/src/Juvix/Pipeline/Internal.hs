{-# LANGUAGE ViewPatterns #-}

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
import qualified Juvix.Context as Context
import qualified Juvix.Core as Core
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.Common.Context.Traverse as Context
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Frontend as Frontend
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Parser (ParserError)
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Sexp as Sexp
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

extractTypeDeclar :: Context.Definition term ty a -> Maybe a
extractTypeDeclar (Context.TypeDeclar t) = Just t
extractTypeDeclar _ = Nothing

mkDef ::
  Sexp.T ->
  Sexp.T ->
  Context.SumT term1 ty1 ->
  Context.T term2 ty2 Sexp.T ->
  Maybe (Context.Def Sexp.T Sexp.T)
mkDef typeCons dataConstructor Context.Sum {sumTName} c = do
  t <- extractTypeDeclar . Context.extractValue =<< Context.lookup (NameSymbol.fromSymbol sumTName) c
  declaration <- Sexp.findKey Sexp.car dataConstructor t
  Just $
    Context.D
      { defUsage = Just Usage.Omega,
        defMTy = generateSumConsSexp typeCons declaration,
        defTerm = Sexp.list [Sexp.atom ":primitive", Sexp.atom "Builtin.Constructor"],
        defPrecedence = Context.default'
      }

generateSumConsSexp :: Sexp.T -> Sexp.T -> Maybe Sexp.T
generateSumConsSexp typeCons (Sexp.cdr -> declaration) = do
  pure . sanitizeRecord $ Sexp.foldr f typeCons declaration
  where
    sanitizeRecord (x Sexp.:> fields)
      | Sexp.isAtomNamed x ":record-d" = Sexp.list $ removeFieldNames fields
    sanitizeRecord xs = xs
    removeFieldNames fields
      | Just l <- Sexp.toList (Sexp.groupBy2 fields) = Sexp.cdr <$> l
    f n acc = Sexp.list [arrow, n, acc]
    arrow = Sexp.atom "TopLevel.Prelude.->"

contextToCore ::
  (Show primTy, Show primVal) =>
  Context.T Sexp.T Sexp.T Sexp.T ->
  P.Parameterisation primTy primVal ->
  Either (FF.Error primTy primVal) (FF.CoreDefs primTy primVal)
contextToCore ctx param =
  FF.execEnv ctx param do
    newCtx <- Context.mapSumWithName ctx attachConstructor

    let ordered = Context.recGroups newCtx

    for_ ordered \grp -> do
      traverse_ addSig grp

    for_ ordered \grp -> do
      traverse_ addDef grp

    defs <- get @"core"
    pure $ FF.CoreDefs {defs, order = fmap Context.name <$> ordered}
  where
    -- Attaches the sum constructor with a data constructor filling
    attachConstructor s@Context.Sum {sumTDef, sumTName} dataCons c =
      case sumTDef of
        Just __ -> s
        Nothing ->
          let dataConsSexp = Sexp.atom $ NameSymbol.fromSymbol dataCons
              typeConsSexp = Sexp.atom $ NameSymbol.fromSymbol sumTName
           in s {Context.sumTDef = mkDef typeConsSexp dataConsSexp s c}
        |> Context.SumCon
        |> pure

addSig ::
  ( Show primTy,
    Show primVal,
    HasThrow "fromFrontendError" (FF.Error primTy primVal) m,
    HasReader "param" (P.Parameterisation primTy primVal) m,
    HasState "coreSigs" (FF.CoreSigsHR primTy primVal) m,
    HasState "patVars" (HM.HashMap Core.GlobalName Core.PatternVar) m
  ) =>
  Context.Entry Sexp.T Sexp.T Sexp.T ->
  m ()
addSig (Context.Entry x feDef) = do
  sigs <- FF.transformSig x feDef
  for_ sigs $ modify @"coreSigs" . HM.insertWith FF.mergeSigs x

addDef ::
  ( Show primTy,
    Show primVal,
    HasThrow "fromFrontendError" (FF.Error primTy primVal) m,
    HasReader "param" (P.Parameterisation primTy primVal) m,
    HasState "core" (HM.HashMap NameSymbol.T (FF.CoreDef primTy primVal)) m,
    HasState "coreSigs" (FF.CoreSigsHR primTy primVal) m,
    HasState "nextPatVar" Core.PatternVar m,
    HasState "patVars" (HM.HashMap Core.GlobalName Core.PatternVar) m
  ) =>
  Context.Entry Sexp.T Sexp.T Sexp.T ->
  m ()
addDef (Context.Entry x feDef) = do
  defs <- FF.transformDef x feDef
  for_ defs \def -> do
    modify @"core" $ HM.insert (defName def) def

defName :: FF.CoreDef primTy primVal -> NameSymbol.T
defName = \case
  FF.CoreDef (Core.RawGDatatype Core.RawDatatype {rawDataName = x}) -> x
  FF.CoreDef (Core.RawGDataCon Core.RawDataCon {rawConName = x}) -> x
  FF.CoreDef (Core.RawGFunction Core.RawFunction {rawFunName = x}) -> x
  FF.CoreDef (Core.RawGAbstract Core.RawAbstract {rawAbsName = x}) -> x
  FF.SpecialDef x _ -> x
