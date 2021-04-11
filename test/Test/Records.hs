module Test.Records where

-- import Test.Contextify
import Juvix.Library hiding (head)
import qualified Juvix.Library.Sexp as Sexp
import Data.Curve.Weierstrass.BLS12381 (Fr)
import qualified Juvix.Backends.Plonk as Plonk
import qualified Juvix.Core.Common.Context as Context
import Juvix.ToCore.Types (execEnv, Env)
import Juvix.Core.HR (Term(..))
import qualified Juvix.ToCore.FromFrontend as FromFrontend
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.HR as HR
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Frontend.Types as Types
import qualified Juvix.Frontend.Sexp as Trans
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Test.Context.Helpers as ContextHelpers
import qualified Juvix.Pipeline as Pipeline
import Juvix.Pipeline (BPlonk(..))
import qualified Data.Aeson as A
import qualified Data.Scientific as S
import Data.Field.Galois (Prime, toP, fromP)
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import qualified Juvix.Core.Application as CoreApp
import Text.Pretty.Simple (pPrint)

import Prelude (head)

instance A.FromJSON Fr where
  parseJSON (A.Number n) = case S.floatingOrInteger n of
    Left floating -> panic $ "Can't parse floating :" <> show n
    Right f -> pure . toP $ toInteger f

instance A.ToJSON Fr where
  toJSON f = A.Number $ S.scientific (fromP f) 0


testModule :: NameSymbol.Mod
testModule = ["TestMod"]

makeFieldName :: Int -> NameSymbol.T
makeFieldName i = NameSymbol.fromText $ "$field" <> show i

singleEleErr :: Types.Header Types.TopLevel -> Sexp.T
singleEleErr = Trans.transTopLevel . head . noHeaderErr

noHeaderErr :: Types.Header topLevel -> [topLevel]
noHeaderErr (Types.NoHeader xs) = xs
noHeaderErr _ = panic "improper form"

-- A record with one field should be equivalent to a primitive type, i.e. we discard the constructor and the field name
recordOneField ::  Env (Plonk.PrimTy Fr) (Plonk.PrimVal Fr) (Juvix.Core.HR.Term (Plonk.PrimTy Fr) (Plonk.PrimVal Fr))
recordOneField = do
    let Right sexp = Sexp.parse "(:record-d x TopLevel.Prelude.Circuit.field)"
        Just record = head <$> Sexp.toList (Sexp.groupBy2 (Sexp.cdr sexp))
    FromFrontend.transformTermHR testModule (Sexp.cadr record)

-- A record with two fields should be equivalent to a sigma tyoe
recordTwoFields ::  Env (Plonk.PrimTy Fr) (Plonk.PrimVal Fr) (Juvix.Core.HR.Term (Plonk.PrimTy Fr) (Plonk.PrimVal Fr))
recordTwoFields = do
    let Right sexp = Sexp.parse "(:record-d x TopLevel.Prelude.Circuit.field y TopLevel.Prelude.Circuit.field)"
        Just (x:y:[]) = Sexp.toList (Sexp.groupBy2 (Sexp.cdr sexp))
    HR.Pi Usage.Omega (makeFieldName 1) <$> FromFrontend.transformTermHR testModule (Sexp.cadr x) <*> FromFrontend.transformTermHR testModule (Sexp.cadr y)


-- A record with multiple fields should be equivalent to a sigma tyoe
recordMultipleFields ::  Env (Plonk.PrimTy Fr) (Plonk.PrimVal Fr) (Juvix.Core.HR.Term (Plonk.PrimTy Fr) (Plonk.PrimVal Fr))
recordMultipleFields = do
    let Right sexp = Sexp.parse "(:record-d x TopLevel.Prelude.Circuit.field y TopLevel.Prelude.Circuit.field z TopLevel.Prelude.Circuit.field)"
        Just (x:xs) = Sexp.toList (Sexp.groupBy2 (Sexp.cdr sexp))

    HR.Pi Usage.Omega (makeFieldName 1) <$> FromFrontend.transformTermHR testModule (Sexp.cadr x) <*> go 2 xs
    where
        go _ [] = panic "Unexpected empty"
        go _ [x] = FromFrontend.transformTermHR testModule (Sexp.cadr x)
        go i (x:xs)=
            HR.Pi Usage.Omega (makeFieldName i) <$> FromFrontend.transformTermHR testModule (Sexp.cadr x) <*> go (i+1) xs

sumTypeOne ::  Env (Plonk.PrimTy Fr) (Plonk.PrimVal Fr) (Juvix.Core.HR.Term (Plonk.PrimTy Fr) (Plonk.PrimVal Fr))
sumTypeOne = do
    let Right sexp = Sexp.parse "(type Foo (a) (Bar a) (Baz a))"
    FromFrontend.transformTermHR testModule (Sexp.cdr sexp)




    -- HR.Pi Usage.Omega (makeFieldName 1) <$> FromFrontend.transformTermHR testModule (Sexp.cadr x) <*> FromFrontend.transformTermHR testModule (Sexp.cadr y)

    -- prim <- HR.Prim <$> FromFrontend.paramConstant a
    -- traceShow named notImplemented 


env :: Env (Plonk.PrimTy Fr) (Plonk.PrimVal Fr) (Juvix.Core.HR.Term (Plonk.PrimTy Fr) (Plonk.PrimVal Fr))
env = do
    let Right x = Sexp.parse "(:record-d x TopLevel.Prelude.Circuit.field)"
        -- Just h = head =<< Sexp.toList (Sexp.groupBy2 (Sexp.cdr x))
    FromFrontend.transformConSig ["Datatypes"] ("Po3" NonEmpty.:| []) Nothing x
    -- FromFrontend.transformTermHR ["Datatypes"] h 

testRecord :: IO (Either
       (FromFrontend.Error
          (Plonk.PrimTy Fr) (Plonk.PrimVal Fr))
       (Term (Plonk.PrimTy Fr) (Plonk.PrimVal Fr)))
testRecord = do
    let Right sexp = Sexp.parse "(type Baz (a) (Foo a) (Bar a))"
    -- ctx <- liftIO $ Context.add (NameSpace.Pub "Baz") (Context.TypeDeclar (Sexp.cdr sexp)) <$> Context.empty "Foo"
    ctx <- liftIO $ either notImplemented fst <$> ContextHelpers.contextualizeFoo "type Foo a = | Bar a | Baz a"
    -- Feedback.runFeedbackT
    pure $ execEnv ctx Plonk.plonk sumTypeOne

-- runPipeline :: FilePath -> Text -> Pipeline.Pipeline ()
-- runPipeline fou = Pipeline.parse @(BPlonk Fr) BPlonk
--                   >=> Pipeline.typecheck @(BPlonk Fr)
--                   >=> Pipeline.compile @(BPlonk Fr) fou



runPipeline :: Text -> IO ()
-- Pipeline.Pipeline (ErasedAnn.AnnTerm (Plonk.PrimTy Fr) (CoreApp.Return' ErasedAnn.T (NonEmpty (Plonk.PrimTy Fr)) (Plonk.PrimVal Fr)))
runPipeline t = do
  feedback <- Feedback.runFeedbackT $ Pipeline.parse @(BPlonk Fr) BPlonk t
                  >>= Pipeline.typecheck @(BPlonk Fr)
  case feedback of
    Feedback.Success msgs _ -> mapM_ pPrint msgs >> exitSuccess
    Feedback.Fail msgs -> mapM_ pPrint msgs >> exitFailure
    
    