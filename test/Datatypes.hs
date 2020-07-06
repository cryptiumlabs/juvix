module Datatypes where

import qualified Data.HashMap.Strict as HM
import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Backends.Michelson.Parameterisation
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Typechecker as Typed
import qualified Juvix.Core.Pipeline as P
import qualified Juvix.Core.Types as Core
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (bool, identity, log)
import qualified Michelson.Typed as MT
import qualified Michelson.Untyped as M
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (String)

data Env primTy primVal
  = Env
      { parameterisation :: Core.Parameterisation primTy primVal,
        log :: [Core.PipelineLog primTy primVal],
        globals :: IR.Globals primTy primVal
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
    ( HasState "globals" (IR.Globals primTy primVal),
      HasSource "globals" (IR.Globals primTy primVal),
      HasSink "globals" (IR.Globals primTy primVal)
    )
    via StateField "globals" (EnvExecAlias primTy primVal compErr)
  deriving
    (HasReader "globals" (IR.Globals primTy primVal))
    via ReaderField "globals" (EnvExecAlias primTy primVal compErr)
  deriving
    (HasThrow "error" (Core.PipelineError primTy primVal compErr))
    via MonadError (EnvExecAlias primTy primVal compErr)

exec ::
  EnvExec primTy primVal CompErr a ->
  Core.Parameterisation primTy primVal ->
  IR.Globals primTy primVal ->
  IO
    ( Either (Core.PipelineError primTy primVal CompErr) a,
      [Core.PipelineLog primTy primVal]
    )
exec (EnvE env) param globals = do
  (ret, env) <- runStateT (runExceptT env) (Env param [] globals)
  pure (ret, log env)

shouldCompileTo ::
  String ->
  (HR.Term PrimTy PrimVal, Usage.T, HR.Term PrimTy PrimVal) ->
  Typed.Globals PrimTy PrimVal ->
  EmptyInstr ->
  T.TestTree
shouldCompileTo name (term, usage, ty) globals instr =
  T.testCase name $ do
    res <- toMichelson term usage ty globals
    show res T.@=? (show (Right instr :: Either String EmptyInstr) :: String)

toMichelson ::
  HR.Term PrimTy PrimVal ->
  Usage.T ->
  HR.Term PrimTy PrimVal ->
  Typed.Globals PrimTy PrimVal ->
  IO (Either String EmptyInstr)
toMichelson term usage ty globals = do
  (res, _) <- exec (P.coreToMichelson term usage ty) michelson globals
  pure $ case res of
    Right r ->
      case r of
        Right e -> Right e
        Left err -> Left (show err)
    Left err -> Left (show err)

tests :: [T.TestTree]
tests =
  [ test_constant,
    test_tuple,
    test_left,
    test_right,
    test_complex_a,
    test_complex_b,
    test_complex_c
  ]

test_constant :: T.TestTree
test_constant =
  shouldCompileTo
    "constant"
    (twoTerm, Usage.Omega, intTy)
    emptyGlobals
    (EmptyInstr (MT.Seq (MT.Nested (MT.PUSH (MT.VInt 2))) MT.Nop))

test_tuple :: T.TestTree
test_tuple =
  shouldCompileTo
    "tuple constructor"
    (HR.Elim (HR.App (HR.App (HR.Var "MkTuple") twoTerm) twoTerm), Usage.Omega, HR.Elim (HR.Var "tuple"))
    (HM.insert "tuple" (IR.GDatatype tupleTy) $ HM.insert "MkTuple" (IR.GDataCon tupleCon) emptyGlobals)
    (EmptyInstr (MT.PUSH (MT.VPair (MT.VInt 2, MT.VInt 2))))

test_left :: T.TestTree
test_left =
  shouldCompileTo
    "left constructor"
    (HR.Elim (HR.App (HR.Var "MkLeft") twoTerm), Usage.Omega, HR.Elim (HR.Var "either"))
    (HM.insert "either" (IR.GDatatype eitherTy) $ HM.insert "MkLeft" (IR.GDataCon leftCon) emptyGlobals)
    (EmptyInstr (MT.PUSH (MT.VInt 2)))

test_right :: T.TestTree
test_right =
  shouldCompileTo
    "right constructor"
    (HR.Elim (HR.App (HR.Var "MkRight") boolTerm), Usage.Omega, HR.Elim (HR.Var "either"))
    (HM.insert "either" (IR.GDatatype eitherTy) $ HM.insert "MkRight" (IR.GDataCon rightCon) emptyGlobals)
    (EmptyInstr (MT.PUSH (MT.VInt 2)))

test_complex_a :: T.TestTree
test_complex_a =
  shouldCompileTo
    "complex constructor a"
    (HR.Elim (HR.App (HR.App (HR.Var "MkComplexA") twoTerm) twoTerm), Usage.Omega, HR.Elim (HR.Var "complex"))
    (HM.insert "complex" (IR.GDatatype complexTy) $ HM.insert "MkComplexA" (IR.GDataCon complexConA) emptyGlobals)
    (EmptyInstr (MT.PUSH (MT.VInt 2)))

test_complex_b :: T.TestTree
test_complex_b =
  shouldCompileTo
    "complex constructor b"
    (HR.Elim (HR.App (HR.Var "MkComplexB") boolTerm), Usage.Omega, HR.Elim (HR.Var "complex"))
    (HM.insert "complex" (IR.GDatatype complexTy) $ HM.insert "MkComplexB" (IR.GDataCon complexConB) emptyGlobals)
    (EmptyInstr (MT.PUSH (MT.VInt 2)))

test_complex_c :: T.TestTree
test_complex_c =
  shouldCompileTo
    "complex constructor c"
    (HR.Elim (HR.App (HR.Var "MkComplexC") (HR.Elim (HR.App (HR.Var "MkRight") boolTerm))), Usage.Omega, HR.Elim (HR.Var "complex"))
    (HM.insert "either" (IR.GDatatype eitherTy) $ HM.insert "MkRight" (IR.GDataCon rightCon) $ HM.insert "complex" (IR.GDatatype complexTy) $ HM.insert "MkComplexC" (IR.GDataCon complexConC) emptyGlobals)
    (EmptyInstr (MT.PUSH (MT.VInt 2)))

twoTerm :: HR.Term PrimTy PrimVal
twoTerm = HR.Elim (HR.Prim (Constant (M.ValueInt 2)))

boolTerm :: HR.Term PrimTy PrimVal
boolTerm = HR.Elim (HR.Prim (Constant (M.ValueFalse)))

intTy :: HR.Term PrimTy PrimVal
intTy = HR.PrimTy int

int :: PrimTy
int = PrimTy (M.Type M.TInt "")

emptyGlobals :: Typed.Globals PrimTy PrimVal
emptyGlobals = mempty

tupleTy :: IR.Datatype PrimTy PrimVal
tupleTy =
  IR.Datatype
    "tuple"
    []
    0
    [tupleCon]

tupleCon :: IR.DataCon PrimTy PrimVal
tupleCon = IR.DataCon "MkTuple" (IR.VPi (Usage.SNat 1) (IR.VPrimTy int) (IR.VPi (Usage.SNat 1) (IR.VPrimTy int) (IR.VNeutral (IR.NFree (IR.Global "tuple")))))

eitherTy :: IR.Datatype PrimTy PrimVal
eitherTy =
  IR.Datatype
    "either"
    []
    0
    [leftCon, rightCon]

leftCon :: IR.DataCon PrimTy PrimVal
leftCon = IR.DataCon "MkLeft" (IR.VPi (Usage.SNat 1) (IR.VPrimTy int) (IR.VNeutral (IR.NFree (IR.Global "either"))))

rightCon :: IR.DataCon PrimTy PrimVal
rightCon = IR.DataCon "MkRight" (IR.VPi (Usage.SNat 1) (IR.VPrimTy bool) (IR.VNeutral (IR.NFree (IR.Global "either"))))

bool :: PrimTy
bool = PrimTy (M.Type M.TBool "")

complexTy :: IR.Datatype PrimTy PrimVal
complexTy =
  IR.Datatype
    "complex"
    []
    0
    [complexConA, complexConB, complexConC]

complexConA :: IR.DataCon PrimTy PrimVal
complexConA = IR.DataCon "MkComplexA" (IR.VPi (Usage.SNat 1) (IR.VPrimTy int) (IR.VPi (Usage.SNat 1) (IR.VPrimTy int) (IR.VNeutral (IR.NFree (IR.Global "complex")))))

complexConB :: IR.DataCon PrimTy PrimVal
complexConB = IR.DataCon "MkComplexB" (IR.VPi (Usage.SNat 1) (IR.VPrimTy bool) (IR.VNeutral (IR.NFree (IR.Global "complex"))))

complexConC :: IR.DataCon PrimTy PrimVal
complexConC = IR.DataCon "MkComplexC" (IR.VPi (Usage.SNat 1) (IR.VNeutral (IR.NFree (IR.Global "either"))) (IR.VNeutral (IR.NFree (IR.Global "complex"))))
