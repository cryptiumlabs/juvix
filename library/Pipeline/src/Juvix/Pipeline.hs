{-# LANGUAGE TypeFamilyDependencies #-}

module Juvix.Pipeline
  ( module Juvix.Pipeline.Compile,
    module Juvix.Pipeline.Types,
    module Juvix.Pipeline,
  )
where

import Control.Arrow (left)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as PM
import qualified Data.Text as Text
import qualified Data.Text.IO as T
import Debug.Pretty.Simple (pTraceShowM)
import qualified Juvix.Context as Context
import qualified Juvix.Core.Application as CoreApp
import qualified Juvix.Core.Base as Core
import qualified Juvix.Core.Base.TransformExt as TransformExt
import qualified Juvix.Core.Base.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import qualified Juvix.Core.HR.Types as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Typechecker.Types as TypeChecker
import Juvix.Core.Parameterisation
  ( CanApply (ApplyErrorExtra, Arg),
    TypedPrim,
  )
import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.Translate as Translate
import qualified Juvix.Core.Types as Core
import qualified Juvix.Frontend as Frontend
import qualified Juvix.Frontend.Types as Types
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Parser (ParserError)
import Juvix.Pipeline.Compile
import qualified Juvix.Pipeline.Core as Core
import qualified Juvix.Pipeline.Frontend as Frontend
import Juvix.Pipeline.Types
import qualified Juvix.Sexp as Sexp
import qualified Juvix.ToCore.FromFrontend as FF
import qualified System.IO.Temp as Temp
import qualified Text.Megaparsec as P
import Text.Pretty.Simple (pShowNoColor)
import qualified Text.PrettyPrint.Leijen.Text as Pretty

-- TODO: Change error type to Error
type Pipeline = Feedback.FeedbackT [] [Char] IO

type IR b = (Core.PatternMap Core.GlobalName, FF.CoreDefs IR.T (Ty b) (Val b))

type Constraints b =
  ( Eq (Ty b),
    Eq (Val b),
    Show (Err b),
    Show (Val b),
    Show (Ty b),
    Show (ApplyErrorExtra (Ty b)),
    Show (ApplyErrorExtra (TypedPrim (Ty b) (Val b))),
    Show (Arg (Ty b)),
    Show (Arg (TypedPrim (Ty b) (Val b))),
    CanApply (Ty b),
    CanApply (TypedPrim (Ty b) (Val b)),
    IR.HasWeak (Val b),
    IR.HasSubstValue IR.T (Ty b) (TypedPrim (Ty b) (Val b)) (Ty b),
    IR.HasPatSubstTerm (OnlyExts.T IR.T) (Ty b) (Val b) (Ty b),
    IR.HasPatSubstTerm (OnlyExts.T IR.T) (Ty b) (Val b) (Val b),
    IR.HasPatSubstTerm (OnlyExts.T IR.T) (Ty b) (TypedPrim (Ty b) (Val b)) (Ty b),
    IR.HasPatSubstTerm (OnlyExts.T TypeChecker.T) (Ty b) (TypedPrim (Ty b) (Val b)) (Ty b)
  )

data Error
  = FrontendErr Frontend.Error
  | ParseErr ParserError
  -- TODO: CoreError
  deriving (Show)

createTmpPath :: Text -> IO FilePath
createTmpPath code = Temp.writeSystemTempFile "juvix-tmp.ju" (Text.unpack code)

prelude :: FilePath
prelude = "stdlib/Prelude.ju"

class HasBackend b where
  type Ty b = ty | ty -> b
  type Val b = val | val -> b
  type Err b = e | e -> b

  stdlibs :: b -> [FilePath]
  stdlibs _ = []

  -- | Parse juvix source code passing a set of libraries explicitly to have them in scope
  toML' :: [FilePath] -> b -> Text -> Pipeline [(NameSymbol.T, [Types.TopLevel])]
  toML' libs b code = liftIO $ do
    fp <- createTmpPath code
    e <- Frontend.parseFiles (libs ++ [fp])
    case e of
      Left err -> Feedback.fail . toS . pShowNoColor . P.errorBundlePretty $ err
      Right x -> pure x

  -- | Parse juvix source code using prelude and the default set of libraries of the backend
  toML :: b -> Text -> Pipeline [(NameSymbol.T, [Types.TopLevel])]
  toML b = toML' (prelude : stdlibs b) b

  toSexp :: b -> [(NameSymbol.T, [Types.TopLevel])] -> Pipeline (Context.T Sexp.T Sexp.T Sexp.T)
  toSexp _b x = liftIO $ do
    e <- Frontend.frontendToSexp x
    case e of
      Left err -> Feedback.fail . toS . pShowNoColor $ err
      Right x -> pure x

  toHR ::
    (Show (Ty b), Show (Val b)) =>
    Param.Parameterisation (Ty b) (Val b) ->
    Context.T Sexp.T Sexp.T Sexp.T ->
    Pipeline (FF.CoreDefs HR.T (Ty b) (Val b))
  toHR param sexp = pure $ FF.coreDefs (Core.contextToHR sexp param)

  toIR ::
    FF.CoreDefs HR.T (Ty b) (Val b) ->
    Pipeline (Core.PatternMap Core.GlobalName, FF.CoreDefs IR.T (Ty b) (Val b))
  toIR hr = pure $ FF.hrToIRDefs hr

  toErased ::
    Constraints b =>
    Param.Parameterisation (Ty b) (Val b) ->
    Ty b ->
    (Core.PatternMap Core.GlobalName, FF.CoreDefs IR.T (Ty b) (Val b)) ->
    Pipeline (ErasedAnn.AnnTermT (Ty b) (Val b))
  toErased param ty (patToSym, defs) = do
    (usage, term, ty) <- getMain >>= toLambda
    let inlinedTerm = IR.inlineAllGlobals term lookupGlobal patToSym
    let erasedAnn = ErasedAnn.irToErasedAnn @(Err b) inlinedTerm usage ty
    res <- liftIO $ fst <$> exec erasedAnn param evaluatedGlobals
    case res of
      Right r -> do
        pure r
      Left err -> do
        Feedback.fail $ "Error: " <> toS (pShowNoColor err) <> " on Term: " <> toS (pShowNoColor term)
    where
      -- Filter out Special Defs
      globalDefs = HM.mapMaybe toCoreDef defs
      lookupGlobal = IR.rawLookupFun' globalDefs
      -- Type primitive values, i.e.
      --      RawGlobal (Ty b) (Val b)
      -- into RawGlobal (Ty b) (TypedPrim (Ty b) (Val b))
      typedGlobals = map (typePrims ty) globalDefs
      evaluatedGlobals = HM.map (unsafeEvalGlobal typedGlobals) typedGlobals
      getMain = case HM.elems $ HM.filter isMain globalDefs of
        [] -> Feedback.fail $ "No main function found in " <> toS (pShowNoColor globalDefs)
        main : _ -> pure main
      toLambda main =
        case TransformExt.extForgetE <$> IR.toLambdaR @IR.T main of
          Just (IR.Ann usage term ty _) -> pure (usage, term, ty)
          _ -> Feedback.fail $ "Unable to convert main to lambda" <> toS (pShowNoColor main)

  -------------
  -- Parsing --
  -------------

  -- | Parse juvix source code passing a set of libraries explicitly to have them in scope
  parseWithLibs :: [FilePath] -> b -> Text -> Pipeline (Context.T Sexp.T Sexp.T Sexp.T)
  parseWithLibs libs b code = do
    fp <- liftIO $ createTmpPath code
    toML' (libs ++ [fp]) b code
      >>= toSexp b

  -- TODO: parse === toML?
  parse :: b -> Text -> Pipeline (Context.T Sexp.T Sexp.T Sexp.T)
  parse b = parseWithLibs libs b
    where
      libs = prelude : stdlibs b

  ------------------
  -- Typechecking --
  ------------------

  typecheck :: Context.T Sexp.T Sexp.T Sexp.T -> Pipeline (ErasedAnn.AnnTermT (Ty b) (Val b))

  typecheck' ::
    Constraints b =>
    Context.T Sexp.T Sexp.T Sexp.T ->
    Param.Parameterisation (Ty b) (Val b) ->
    Ty b ->
    Pipeline (ErasedAnn.AnnTermT (Ty b) (Val b))
  typecheck' ctx param ty =
    let ir = Core.contextToIR ctx param
        defs = FF.coreDefs ir
        patVars = FF.patVars ir
        patToSym = HM.toList patVars |> map swap |> PM.fromList
     in toErased param ty (patToSym, defs)

  compile ::
    FilePath ->
    ErasedAnn.AnnTermT (Ty b) (Val b) ->
    Pipeline ()

-- | Write the output code to a given file.
writeout :: FilePath -> Text -> Pipeline ()
writeout fout code = liftIO $ T.writeFile fout code
