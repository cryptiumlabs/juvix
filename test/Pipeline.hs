module Pipeline where

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
  [test_constant]

test_constant :: T.TestTree
test_constant =
  shouldCompileTo
    "constant"
    (twoTerm, Usage.Omega, intTy)
    emptyGlobals
    (EmptyInstr (MT.Seq (MT.Nested (MT.PUSH (MT.VInt 2))) MT.Nop))

twoTerm :: HR.Term PrimTy PrimVal
twoTerm = HR.Elim (HR.Prim (Constant (M.ValueInt 2)))

emptyGlobals :: Typed.Globals PrimTy PrimVal
emptyGlobals = mempty

intTy :: HR.Term PrimTy PrimVal
intTy = HR.PrimTy int

int :: PrimTy
int = PrimTy (M.Type M.TInt "")
