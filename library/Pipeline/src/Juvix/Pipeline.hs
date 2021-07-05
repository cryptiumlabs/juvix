{-# LANGUAGE TypeFamilyDependencies #-}

module Juvix.Pipeline
  ( module Juvix.Pipeline.Compile,
    module Juvix.Pipeline.Types,
    module Juvix.Pipeline,
  )
where

import qualified Data.Aeson as A
import Control.Arrow (left)
import qualified Data.Text as Text
import qualified Data.Text.IO as T
import Debug.Pretty.Simple (pTraceShowM)
import Text.Pretty.Simple (pShowNoColor)
import qualified Juvix.Context as Context
import qualified Juvix.Core.Application as CoreApp
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Base as Core
import qualified Juvix.Core.Base.TransformExt as TransformExt
import qualified Juvix.Core.Base.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.IR.Typechecker.Types as TypeChecker
import Juvix.Core.Parameterisation
  ( CanApply (ApplyErrorExtra, Arg),
    TypedPrim,
  )
import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.Types as Core
import Juvix.Library
import Juvix.Library.Parser (ParserError)
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Sexp as Sexp
import Juvix.Pipeline.Compile
import qualified Juvix.Pipeline.Core as Core
import qualified Juvix.Pipeline.Frontend as Frontend
import Juvix.Pipeline.Types
import qualified Juvix.Frontend as Frontend
import qualified Juvix.ToCore.FromFrontend as FF
import qualified System.IO.Temp as Temp
import qualified Text.Megaparsec as P
import qualified Text.PrettyPrint.Leijen.Text as Pretty
import qualified Juvix.Core.HR.Types as HR
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as PM
import qualified Juvix.Frontend.Types as Types
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Core.Translate as Translate


type Pipeline = Feedback.FeedbackT [] [Char] IO
data Error
  = FrontendErr Frontend.Error
  | ParseErr ParserError
  -- TODO: CoreError
  deriving (Show)

createTmpPath :: Text -> IO FilePath 
createTmpPath code = Temp.writeSystemTempFile "juvix-tmp.ju" (Text.unpack code)

prelude :: FilePath 
prelude = "stdlib/Prelude.ju" 

-- toIR :: (Applicative f, Traversable t) =>
-- (t (HR.Pattern primTy primVal), FF.CoreDefs HR.T ty val)
-- -> f (t (IR.Pattern primTy primVal), FF.CoreDefs IR.T ty val)
-- toIR (patVars, defs) = pure (fst $ Translate.hrPatternsToIR patVars, FF.hrToIRDefs defs)
class HasBackend b where
  type Ty b = ty | ty -> b
  type Val b = val | val -> b
  type Err b = e | e -> b

  
  stdlibs :: b -> [FilePath]
  stdlibs _ = []

 
  -------------
  -- Parsing --
  -------------

  -- | Parse juvix source code passing a set of libraries explicitly to have them in scope
  toML' :: [FilePath] -> b -> Text -> Pipeline [(NameSymbol.T, [Types.TopLevel])]
  toML' libs b code = liftIO $ do
    fp <- createTmpPath code
    e <- Frontend.parseFiles (libs ++ [fp])
    case e of
      Left err -> Feedback.fail . toS . pShowNoColor . P.errorBundlePretty $ err
      Right x -> pure x

  toML :: b -> Text -> Pipeline [(NameSymbol.T, [Types.TopLevel])]
  toML b = toML' (prelude : stdlibs b) b

  toSexp :: b -> [(NameSymbol.T, [Types.TopLevel])] -> Pipeline (Context.T Sexp.T Sexp.T Sexp.T)
  toSexp _b x = liftIO $ do
    e <- Frontend.frontendToSexp x
    case e of
      Left err -> Feedback.fail . toS . pShowNoColor $ err
      Right x -> pure x

  -- | Parse juvix source code passing a set of libraries explicitly to have them in scope
  parseWithLibs :: [FilePath] -> b -> Text -> Pipeline (Context.T Sexp.T Sexp.T Sexp.T)
  parseWithLibs libs b code =  do
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
  toHR ::
    (Show (Ty b), Show (Val b)) =>
    Context.T Sexp.T Sexp.T Sexp.T ->
    Param.Parameterisation (Ty b) (Val b) ->
    Pipeline (HM.HashMap HR.GlobalName HR.PatternVar, FF.CoreDefs HR.T (Ty b) (Val b))
  toHR ctx param = 
    let hrState = Core.contextToHR ctx param 
    -- TODO: Filter coreDefs
    in pure (FF.patVars hrState, FF.coreDefs hrState)

  
  toIR :: 
    (HM.HashMap Core.GlobalName HR.PatternVar, FF.CoreDefs HR.T (Ty b) (Val b))
    -> Pipeline (HM.HashMap Core.GlobalName Core.PatternVar, FF.CoreDefs IR.T (Ty b) (Val b))
  toIR (patVars, defs) = pure (patVars, FF.hrToIRDefs defs)
  
  -- toErased :: (HM.HashMap Core.GlobalName Core.PatternVar, FF.CoreDefs IR.T (Ty b) (Val b))
  -- toErased (patVars, defs) = 

  typecheck :: Context.T Sexp.T Sexp.T Sexp.T -> Pipeline (ErasedAnn.AnnTermT (Ty b) (Val b))

  typecheck' ::
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
    ) =>
    Context.T Sexp.T Sexp.T Sexp.T ->
    Param.Parameterisation (Ty b) (Val b) ->
    Ty b ->
    Pipeline (ErasedAnn.AnnTermT (Ty b) (Val b))
  typecheck' ctx param ty = do
    -- Handle main function
    case HM.elems $ HM.filter isMain globalDefs of
      [] -> Feedback.fail $ "No main function found in " <> show globalDefs
      f : _ ->
        case TransformExt.extForgetE <$> IR.toLambdaR @IR.T f of
          Nothing -> Feedback.fail "Unable to convert main to lambda"
          Just (IR.Ann usage term ty _) -> do
            let inlinedTerm = IR.inlineAllGlobals term lookupGlobal patternMap
            (res, _) <- liftIO $ exec (ErasedAnn.irToErasedAnn @(Err b) inlinedTerm usage ty) param evaluatedGlobals
            case res of
              Right r -> do
                pure r
              Left err -> do
                Feedback.fail $ "Error: " <> show err <> "on Term: " <> show term
    where
      irState = Core.contextToIR ctx param
      -- Filter out Special Defs
      globalDefs = HM.mapMaybe toCoreDef (FF.coreDefs irState)
      patVars = FF.patVars irState
      patternMap = HM.toList patVars |> map swap |> PM.fromList
      lookupGlobal = IR.rawLookupFun' globalDefs
    -- Type primitive values, i.e.
    --      RawGlobal (Ty b) (Val b) 
    -- into RawGlobal (Ty b) (TypedPrim (Ty b) (Val b))
      typedGlobals = map (typePrims ty) globalDefs
      evaluatedGlobals = HM.map (unsafeEvalGlobal typedGlobals) typedGlobals

  compile ::
    FilePath ->
    ErasedAnn.AnnTermT (Ty b) (Val b) ->
    Pipeline ()

-- | Write the output code to a given file.
writeout :: FilePath -> Text -> Pipeline ()
writeout fout code = liftIO $ T.writeFile fout code
