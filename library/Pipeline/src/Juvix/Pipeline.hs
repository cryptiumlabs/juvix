{-# LANGUAGE TypeFamilyDependencies #-}

module Juvix.Pipeline
  ( module Juvix.Pipeline.Compile,
    module Juvix.Pipeline.Internal,
    module Juvix.Pipeline.Types,
    module Juvix.Pipeline,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.IO as T
import qualified Juvix.Core.Application as CoreApp
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Library.Sexp as Sexp
import Juvix.Pipeline.Compile
import Juvix.Pipeline.Internal
import qualified Juvix.Pipeline.Internal as Pipeline
import Juvix.Pipeline.Types
import qualified System.IO.Temp as Temp
import qualified Text.Megaparsec as P
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Juvix.Core.ErasedAnn.Types as CoreErased
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.TransformExt as TransformExt
import qualified Juvix.Core.IR.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.IR.Typechecker.Types as TypeChecker
import Debug.Pretty.Simple ( pTraceShowM ) 
import Juvix.Core.Parameterisation
  ( CanApply (ApplyErrorExtra, Arg),
    TypedPrim,
  )
import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.Pipeline as CorePipeline
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import Juvix.ToCore.FromFrontend as FF (CoreDefs (..))
import qualified Text.PrettyPrint.Leijen.Text as Pretty
import qualified Juvix.Core.Types as Core

class HasBackend b where
  type Ty b = ty | ty -> b
  type Val b = val | val -> b

  stdlibs :: b -> [FilePath]
  stdlibs _ = []

  parseWithLibs :: [FilePath] -> b -> Text -> Pipeline (Context.T Sexp.T Sexp.T Sexp.T)
  parseWithLibs libs b code = liftIO $ do
    fp <- Temp.writeSystemTempFile "juvix-toCore.ju" (Text.unpack code)
    core <- Pipeline.toCore (libs ++ [fp])
    handleCore core

  parse :: b -> Text -> Pipeline (Context.T Sexp.T Sexp.T Sexp.T)
  parse b code = do
    core <- liftIO $ toCore_wrap code
    handleCore core
    where
      toCore_wrap :: Text -> IO (Either Pipeline.Error (Context.T Sexp.T Sexp.T Sexp.T))
      toCore_wrap code = do
        fp <- Temp.writeSystemTempFile "juvix-toCore.ju" (Text.unpack code)
        Pipeline.toCore
          ("stdlib/Prelude.ju" : stdlibs b ++ [fp])

  typecheck ::
    Context.T Sexp.T Sexp.T Sexp.T ->
    Pipeline (ErasedAnn.AnnTermT (Ty b) (Val b))
  compile ::
    FilePath ->
    ErasedAnn.AnnTermT (Ty b) (Val b) ->
    Pipeline ()

-- | Write the output code to a given file.
writeout :: FilePath -> Text -> Pipeline ()
writeout fout code = liftIO $ T.writeFile fout code

parseExplicit :: b -> Text -> [FilePath] -> Pipeline (Context.T Sexp.T Sexp.T Sexp.T)
parseExplicit _b code libs = do
  core <- liftIO $ toCore_wrap code
  case core of
    Right ctx -> return ctx
    Left (Pipeline.ParseErr err) -> Feedback.fail $ P.errorBundlePretty err
    Left err -> Feedback.fail $ show err
  where
    toCore_wrap :: Text -> IO (Either Pipeline.Error (Context.T Sexp.T Sexp.T Sexp.T))
    toCore_wrap code = do
      fp <- Temp.writeSystemTempFile "juvix-toCore.ju" (Text.unpack code)
      Pipeline.toCore (fp : libs)

handleCore :: MonadFail m => Either Error a -> m a
handleCore core = case core of
  Right ctx -> return ctx
  Left (Pipeline.ParseErr err) -> Feedback.fail $ P.errorBundlePretty err
  Left err -> Feedback.fail $ show err

typchk :: forall ty val err.
  (Eq ty, Show ty, Eq val, Show err, Show val,CanApply  ty, CanApply (TypedPrim ty val), 
  IR.HasWeak val, IR.HasSubstValue IR.NoExt ty (TypedPrim ty val) ty,
  IR.HasPatSubstTerm (OnlyExts.T IR.NoExt) ty (TypedPrim ty val) ty,
  Show (ApplyErrorExtra ty), Show (ApplyErrorExtra (TypedPrim ty val)),
  Show (Arg ty), Show (Arg (TypedPrim ty val)), 
  IR.HasPatSubstTerm (OnlyExts.T IR.NoExt) ty val ty,
  IR.HasPatSubstTerm (OnlyExts.T IR.NoExt) ty val val,
  IR.HasPatSubstTerm (OnlyExts.T TypeChecker.T) ty (TypedPrim ty val) ty
  )
  => Context.T Sexp.T Sexp.T Sexp.T -> Param.Parameterisation ty val -> ty -> Proxy err -> Pipeline (ErasedAnn.AnnTermT ty val)
typchk ctx param ty _ =  do
    let res = Pipeline.contextToCore ctx param
    case res of
      Right (FF.CoreDefs _order globals) -> do
        let globalDefs = HM.mapMaybe toCoreDef globals
        let convGlobals = map (convGlobal ty) globalDefs
            newGlobals = HM.map (unsafeEvalGlobal convGlobals) convGlobals
            lookupGlobal = IR.rawLookupFun' globalDefs
        case HM.elems $ HM.filter isMain globalDefs of
          [] -> Feedback.fail $ "No main function found in " <> show globalDefs
          [f@(IR.RawGFunction _)]  ->
            case TransformExt.extForgetE <$> IR.toLambdaR @IR.NoExt f of
              Nothing -> do
                Feedback.fail "Unable to convert main to lambda" 
              Just (IR.Ann usage term ty _) -> do
                  let inlinedTerm = IR.inlineAllGlobals term lookupGlobal
                  (res, _) <- liftIO $ exec (CorePipeline.coreToAnn @err inlinedTerm usage ty) param newGlobals
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