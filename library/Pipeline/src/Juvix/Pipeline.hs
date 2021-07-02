{-# LANGUAGE TypeFamilyDependencies #-}

module Juvix.Pipeline
  ( module Juvix.Pipeline.Compile,
    module Juvix.Pipeline.Types,
    module Juvix.Pipeline,
  )
where

import qualified Data.Aeson as A
import qualified Data.Text as Text
import qualified Data.Text.IO as T
import Debug.Pretty.Simple (pTraceShowM)
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
import qualified Juvix.Pipeline.Core as CorePipeline
import qualified Juvix.Pipeline.Frontend as FrontendPipeline
import Juvix.Pipeline.Types
import qualified Juvix.Frontend as Frontend
import qualified Juvix.ToCore.FromFrontend as FF
import qualified System.IO.Temp as Temp
import qualified Text.Megaparsec as P
import qualified Text.PrettyPrint.Leijen.Text as Pretty
import qualified Juvix.Core.HR.Types as HR
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as PM

type Pipeline = Feedback.FeedbackT [] [Char] IO
data Error
  = FrontendErr FrontendPipeline.Error
  | ParseErr ParserError
  -- TODO: CoreError
  deriving (Show)


class HasBackend b where
  type Ty b = ty | ty -> b
  type Val b = val | val -> b
  type Err b = e | e -> b

  stdlibs :: b -> [FilePath]
  stdlibs _ = []

  parseWithLibs :: [FilePath] -> b -> Text -> Pipeline (Context.T Sexp.T Sexp.T Sexp.T)
  parseWithLibs libs b code = liftIO $ do
    fp <- Temp.writeSystemTempFile "juvix-toSexp.ju" (Text.unpack code)
    core <- toSexp (libs ++ [fp])
    handleCore core

  parse :: b -> Text -> Pipeline (Context.T Sexp.T Sexp.T Sexp.T)
  parse b code = do
    core <- liftIO $ toSexp_wrap code
    handleCore core
    where
      toSexp_wrap :: Text -> IO (Either Error (Context.T Sexp.T Sexp.T Sexp.T))
      toSexp_wrap code = do
        fp <- Temp.writeSystemTempFile "juvix-toSexp.ju" (Text.unpack code)
        toSexp
          ("stdlib/Prelude.ju" : stdlibs b ++ [fp])

  typecheck :: Context.T Sexp.T Sexp.T Sexp.T -> Pipeline (ErasedAnn.AnnTermT (Ty b) (Val b))

  toHR ::
    (Show (Ty b), Show (Val b)) =>
    Context.T Sexp.T Sexp.T Sexp.T ->
    Param.Parameterisation (Ty b) (Val b) ->
    Pipeline (FF.CoreDefs HR.T (Ty b) (Val b))
  toHR ctx param = notImplemented
    -- case CorePipeline.contextToHR ctx param of
    -- Left err -> Feedback.fail $ show err
    -- Right defs -> pure defs

  
  -- toIR: in terms of HR 
  -- = f toHR
  
  -- toErased
  --  = f toIR

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
    let res = notImplemented -- Pipeline.contextToCore ctx param

    -- Pipeline.
    case res of
      Right (FF.CoreDefs _order globals) -> do
        -- Filter out Special Defs
        let globalDefs = HM.mapMaybe toCoreDef globals
            lookupGlobal = IR.rawLookupFun' globalDefs
        -- Type primitive values, i.e.
        --      RawGlobal (Ty b) (Val b) 
        -- into RawGlobal (Ty b) (TypedPrim (Ty b) (Val b))
        let typedGlobals = map (typePrims ty) globalDefs
            evaluatedGlobals = HM.map (unsafeEvalGlobal typedGlobals) typedGlobals
        case HM.elems $ HM.filter isMain globalDefs of
          [] -> Feedback.fail $ "No main function found in " <> show globalDefs
          [f@(Core.RawGFunction _)] ->
            case TransformExt.extForgetE <$> IR.toLambdaR @IR.T f of
              Nothing -> do
                Feedback.fail "Unable to convert main to lambda"
              Just (IR.Ann usage term ty _) -> do
                let patternMap = HM.toList (FF.patVars state) |> map swap |> PM.fromList
                let inlinedTerm = IR.inlineAllGlobals term lookupGlobal patternMap
                (res, _) <- liftIO $ exec (ErasedAnn.irToErasedAnn @(Err b) inlinedTerm usage ty) param evaluatedGlobals
                case res of
                  Right r -> do
                    pure r
                  Left err -> do
                    print term
                    Feedback.fail $ show err
          somethingElse -> do
            pTraceShowM somethingElse
            Feedback.fail $ show somethingElse
      Left err -> Feedback.fail $ "failed at ctxToCore\n" ++ show err
  compile ::
    FilePath ->
    ErasedAnn.AnnTermT (Ty b) (Val b) ->
    Pipeline ()

-- | Write the output code to a given file.
writeout :: FilePath -> Text -> Pipeline ()
writeout fout code = liftIO $ T.writeFile fout code

parseExplicit :: b -> Text -> [FilePath] -> Pipeline (Context.T Sexp.T Sexp.T Sexp.T)
parseExplicit _b code libs = do
  core <- liftIO $ toSexp_wrap code
  case core of
    Right ctx -> return ctx
    Left (ParseErr err) -> Feedback.fail $ P.errorBundlePretty err
    Left err -> Feedback.fail $ show err
  where
    toSexp_wrap :: Text -> IO (Either Error (Context.T Sexp.T Sexp.T Sexp.T))
    toSexp_wrap code = do
      fp <- Temp.writeSystemTempFile "juvix-toSexp.ju" (Text.unpack code)
      toSexp (fp : libs)

handleCore :: MonadFail m => Either Error a -> m a
handleCore core = case core of
  Right ctx -> return ctx
  Left (ParseErr err) -> Feedback.fail $ P.errorBundlePretty err
  Left err -> Feedback.fail $ show err

toSexp :: [FilePath] -> IO (Either Error (Context.T Sexp.T Sexp.T Sexp.T))
toSexp paths = do
  x <- Frontend.parseFiles paths
  case x of
    Left er -> pure $ Left (ParseErr er)
    Right x -> do
      from <- FrontendPipeline.frontendToSexp x
      case from of
        Left errr -> pure $ Left (FrontendErr errr)
        Right con -> pure $ Right con

