{-# LANGUAGE LiberalTypeSynonyms #-}

module Pipeline where

import qualified Juvix.Backends.Michelson as Michelson
import qualified Juvix.Backends.Michelson.Compilation as M
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Pipeline as P
import qualified Juvix.Core.Types as Core
import Juvix.Library hiding (bool, identity, log)
import qualified Juvix.Library.Usage as Usage
import qualified Michelson.TypeCheck as Michelson
import qualified Michelson.Typed as MT
import qualified Michelson.Untyped as M
import qualified Michelson.Untyped as Michelson
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (String)

type RawMichelson f = f Michelson.PrimTy Michelson.RawPrimVal

type RawMichelsonTerm = RawMichelson IR.Term

type RawMichelsonElim = RawMichelson IR.Elim

type MichelsonCompConstraints m =
  P.CompConstraints' Michelson.PrimTy Michelson.RawPrimVal Michelson.CompilationError m

type MichelsonComp res =
  forall m.
  MichelsonCompConstraints m =>
  RawMichelsonTerm ->
  Usage.T ->
  RawMichelsonTerm ->
  m res

data Env primTy primVal
  = Env
      { parameterisation :: Core.Parameterisation primTy primVal,
        log :: [Core.PipelineLog primTy primVal],
        globals :: IR.GlobalsT primTy primVal
      }
  deriving (Generic)

type EnvExecAlias primTy primVal compErr =
  ExceptT
    (Core.PipelineError primTy primVal compErr)
    (StateT (Env primTy primVal) IO)

newtype EnvExec primTy primVal compErr a
  = EnvE (EnvExecAlias primTy primVal compErr a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving
    ( HasSink "log" [Core.PipelineLog primTy primVal],
      HasWriter "log" [Core.PipelineLog primTy primVal]
    )
    via WriterField "log" (EnvExecAlias primTy primVal compErr)
  deriving
    ( HasReader "parameterisation" (Core.Parameterisation primTy primVal),
      HasSource "parameterisation" (Core.Parameterisation primTy primVal)
    )
    via ReaderField "parameterisation" (EnvExecAlias primTy primVal compErr)
  deriving
    ( HasState "globals" (IR.GlobalsT primTy primVal),
      HasSource "globals" (IR.GlobalsT primTy primVal),
      HasSink "globals" (IR.GlobalsT primTy primVal)
    )
    via StateField "globals" (EnvExecAlias primTy primVal compErr)
  deriving
    (HasReader "globals" (IR.GlobalsT primTy primVal))
    via ReaderField "globals" (EnvExecAlias primTy primVal compErr)
  deriving
    (HasThrow "error" (Core.PipelineError primTy primVal compErr))
    via MonadError (EnvExecAlias primTy primVal compErr)

coreToMichelson :: MichelsonComp (Either Michelson.CompilationError Michelson.EmptyInstr)
coreToMichelson term usage ty = do
  ann <- P.coreToAnn term usage ty
  pure $ fst $ Michelson.compileExpr $ P.toRaw ann

coreToMichelsonContract :: MichelsonComp (Either Michelson.CompilationError (Michelson.Contract' Michelson.ExpandedOp, Michelson.SomeContract))
coreToMichelsonContract term usage ty = do
  ann <- P.coreToAnn term usage ty
  pure $ fst $ Michelson.compileContract $ P.toRaw ann

exec ::
  EnvExec primTy primVal Michelson.CompilationError a ->
  Core.Parameterisation primTy primVal ->
  IR.GlobalsT primTy primVal ->
  IO
    ( Either (Core.PipelineError primTy primVal Michelson.CompilationError) a,
      [Core.PipelineLog primTy primVal]
    )
exec (EnvE env) param globals = do
  (ret, env) <- runStateT (runExceptT env) (Env param [] globals)
  pure (ret, log env)

type Globals = IR.Globals Michelson.PrimTy Michelson.PrimValIR

type AnnTuple = (RawMichelsonTerm, Usage.T, RawMichelsonTerm)

shouldCompileTo ::
  String ->
  AnnTuple ->
  Globals ->
  Michelson.EmptyInstr ->
  T.TestTree
shouldCompileTo name (term, usage, ty) globals instr =
  T.testCase name $ do
    res <- toMichelson term usage ty globals
    show res T.@=? (show (Right instr :: Either String Michelson.EmptyInstr) :: String)

shouldCompileToContract ::
  String ->
  AnnTuple ->
  Globals ->
  Text ->
  T.TestTree
shouldCompileToContract name (term, usage, ty) globals contract =
  T.testCase name $ do
    res <- toMichelsonContract term usage ty globals
    res T.@=? Right contract

toMichelson ::
  RawMichelsonTerm ->
  Usage.T ->
  RawMichelsonTerm ->
  Globals ->
  IO (Either String Michelson.EmptyInstr)
toMichelson term usage ty globals = do
  (res, _) <- exec (coreToMichelson term usage ty) Michelson.michelson globals
  pure $ case res of
    Right r ->
      case r of
        Right e -> Right e
        Left err -> Left (show err)
    Left err -> Left (show err)

toMichelsonContract ::
  RawMichelsonTerm ->
  Usage.T ->
  RawMichelsonTerm ->
  Globals ->
  IO (Either String Text)
toMichelsonContract term usage ty globals = do
  (res, _) <- exec (coreToMichelsonContract term usage ty) Michelson.michelson globals
  pure $ case res of
    Right r ->
      case r of
        Right e -> Right (M.typedContractToSource $ snd e)
        Left err -> Left (show err)
    Left err -> Left (show err)

tests :: [T.TestTree]
tests =
  [ test_constant,
    test_erased_function,
    test_real_function_apply,
    test_partial_erase
  ]

test_constant :: T.TestTree
test_constant =
  shouldCompileTo
    "constant"
    (int 2, Usage.Omega, intTy)
    mempty
    (Michelson.EmptyInstr (MT.Nested (MT.PUSH (MT.VInt 2))))

test_erased_function :: T.TestTree
test_erased_function =
  shouldCompileTo
    "erased function"
    (erasedLamTerm, Usage.Omega, erasedLamTy)
    mempty
    (Michelson.EmptyInstr (MT.Nested (MT.PUSH (MT.VInt 2))))

test_real_function_apply :: T.TestTree
test_real_function_apply =
  shouldCompileTo
    "real function with application"
    (appLam, Usage.Omega, intTy)
    mempty
    (Michelson.EmptyInstr (MT.Nested (MT.PUSH (MT.VInt 5))))

test_partial_erase :: T.TestTree
test_partial_erase =
  shouldCompileTo
    "real function with partial erase"
    (appLam2, Usage.Omega, intTy)
    mempty
    (Michelson.EmptyInstr (MT.Nested (MT.PUSH (MT.VInt 12))))

erasedLamTerm :: RawMichelsonTerm
erasedLamTerm = IR.Lam $ int 2

erasedLamTy :: RawMichelsonTerm
erasedLamTy = IR.Pi zero intTy intTy

appLam :: RawMichelsonTerm
appLam = IR.Elim $ lamElim `IR.App` int 2 `IR.App` int 3

addTyT :: RawMichelsonTerm
addTyT = IR.Pi one intTy $ IR.Pi one intTy intTy

addElim :: RawMichelsonElim
addElim = IR.Ann Usage.Omega (IR.Prim Michelson.AddI) addTyT 0

lamTerm :: RawMichelsonTerm
lamTerm =
  IR.Lam $ IR.Lam $ IR.Elim $ addElim `IR.App` varT 1 `IR.App` varT 0

lamElim :: RawMichelsonElim
lamElim = IR.Ann one lamTerm lamTy 0

appLam2 :: RawMichelsonTerm
appLam2 = IR.Elim $ lamElim2 `IR.App` int 2 `IR.App` int 3

lamTerm2 :: RawMichelsonTerm
lamTerm2 =
  IR.Lam $ IR.Lam $ IR.Elim $ addElim `IR.App` varT 1 `IR.App` int 10

varT :: Natural -> RawMichelsonTerm
varT = IR.Elim . IR.Bound

lamTy :: RawMichelsonTerm
lamTy = intTy ~~> intTy ~~> intTy

lamTy2 :: RawMichelsonTerm
lamTy2 = intTy ~~> intTy ~@> intTy

lamElim2 :: RawMichelsonElim
lamElim2 = IR.Ann one lamTerm2 lamTy2 0

michelsonTy :: M.T -> RawMichelsonTerm
michelsonTy t = IR.PrimTy $ Michelson.PrimTy $ M.Type t M.noAnn

intTy :: RawMichelsonTerm
intTy = michelsonTy M.TInt

int :: Integer -> RawMichelsonTerm
int = IR.Prim . Michelson.Constant . M.ValueInt

arr :: Usage.T -> RawMichelsonTerm -> RawMichelsonTerm -> RawMichelsonTerm
arr π s t = IR.Pi π s (IR.weak t)

infixr 0 ~~>

(~~>) :: RawMichelsonTerm -> RawMichelsonTerm -> RawMichelsonTerm
(~~>) = arr one

infixr 0 ~@>

(~@>) :: RawMichelsonTerm -> RawMichelsonTerm -> RawMichelsonTerm
(~@>) = arr zero
