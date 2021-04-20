{-# LANGUAGE ViewPatterns #-}
module Juvix.Pipeline.Internal
  ( Error (..),
    toCore,
    contextToCore,
    defName,
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
import Debug.Pretty.Simple (pTraceShowM, pTraceShow)
import qualified Juvix.Library.Usage as Usage

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

mkDef 
  :: Sexp.T
  -> Context.SumT term1 ty1
  -> Context.T term2 ty2 Sexp.T
  -> Maybe (Context.Def Sexp.T Sexp.T)
mkDef dataConstructor s@Context.Sum{sumTDef, sumTName} c = do 
    t <- extractTypeDeclar . Context.extractValue =<< Context.lookup (NameSymbol.fromSymbol sumTName) c
    declaration <- Sexp.findKey Sexp.car dataConstructor t 
    Just $
        Context.D
          { defUsage = Just Usage.Omega, 
            defMTy = generateSumConsType declaration,
            defTerm = Sexp.list [Sexp.atom ":primitive", Sexp.atom "Builtin.Constructor"],
            defPrecedence = Context.default'
          }

-- TODO: We're only handling the ADT case with no records or GADTs
generateSumConsType :: Sexp.T -> Maybe Sexp.T
generateSumConsType (Sexp.cdr -> declaration) = Sexp.foldr1 f declaration
  where
    f n acc = Sexp.list [Sexp.atom "TopLevel.Prelude.->", n, acc]

contextToCore ::
  (Show primTy, Show primVal) =>
  Context.T Sexp.T Sexp.T Sexp.T ->
  P.Parameterisation primTy primVal ->
  Either (FF.Error primTy primVal) (FF.CoreDefs primTy primVal)
contextToCore ctx param = do
  FF.execEnv ctx param do
    newCtx <- Context.mapWithContext' ctx updateCtx

    let ordered = Context.recGroups newCtx
    pTraceShowM ordered
    for_ ordered \grp -> do
      traverse_ addSig grp
      traverse_ addDef grp
    defs <- get @"core"
    pure $ FF.CoreDefs {defs, order = fmap Context.name <$> ordered}
  where
    updateCtx def dataCons s@Context.Sum{sumTDef} c =
      case sumTDef of
        Just v -> pure $ Just v
        Nothing -> 
          let dataConsSexp = Sexp.atom $ NameSymbol.fromSymbol dataCons
          in case mkDef dataConsSexp s c of
            Nothing -> pure $ notImplemented
            Just x -> pure $ Just x
      
    addSig (Context.Entry x feDef) = do
      msig <- FF.transformSig x feDef
      traceM "Add Sig!"
      pTraceShowM (x, feDef, msig)
      for_ msig $ modify @"coreSigs" . HM.insertWith FF.mergeSigs x
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
