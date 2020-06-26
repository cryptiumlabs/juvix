module Juvix.Core.IR.Typechecker.Env where

import Data.HashMap.Strict (HashMap)
import qualified Juvix.Core.IR.Evaluator as Eval
import Juvix.Core.IR.Typechecker.Types
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Datatype)

data EnvCtx primTy primVal
  = EnvCtx
      { globals :: Globals primTy primVal
      }
  deriving (Show, Eq, Generic)

type Globals primTy primVal =
  HashMap IR.GlobalName (Global primTy primVal)

data Global primTy primVal
  = GDatatype (IR.Datatype primTy primVal)
  | GDataCon (IR.DataCon primTy primVal)
  | GFunction (IR.Function primTy primVal)
  deriving (Show, Eq, Generic)

type EnvAlias primTy primVal =
  ExceptT
    (TypecheckError primTy primVal)
    (State (EnvCtx primTy primVal))

newtype EnvTypecheck primTy primVal a = EnvTyp (EnvAlias primTy primVal a)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasThrow "typecheckError" (TypecheckError primTy primVal)
    )
    via MonadError (EnvAlias primTy primVal)
  deriving
    ( HasSink "globals" (Globals primTy primVal),
      HasState "globals" (Globals primTy primVal)
    )
    via StateField "globals" (EnvAlias primTy primVal)
  deriving
    ( HasSource "globals" (Globals primTy primVal),
      HasReader "globals" (Globals primTy primVal)
    )
    via ReaderField "globals" (EnvAlias primTy primVal)

exec ::
  Globals primTy primVal ->
  EnvTypecheck primTy primVal a ->
  (Either (TypecheckError primTy primVal) a, EnvCtx primTy primVal)
exec globals (EnvTyp env) =
  runState (runExceptT env) $ EnvCtx globals

type Context primTy primVal = [Annotation primTy primVal]

lookupCtx ::
  Context primTy primVal ->
  IR.BoundVar ->
  Maybe (Annotation primTy primVal)
lookupCtx gam x = do
  Annotation π ty <- atMay gam (fromIntegral x)
  pure $ Annotation π (Eval.weakBy (x + 1) ty)

type UContext primTy primVal = [Usage.T]

type PatBinds primTy primVal = IntMap (Annotation primTy primVal)

type PatUsages primTy primVal = IntMap Usage.T

data InnerState primTy primVal
  = InnerState
      { param :: Param.Parameterisation primTy primVal,
        patBinds :: PatBinds primTy primVal,
        bound :: Context primTy primVal
      }
  deriving (Generic)

type InnerTCAlias primTy primVal =
  StateT (InnerState primTy primVal) (EnvTypecheck primTy primVal)

newtype InnerTC primTy primVal a
  = InnerTC {unInnerTC :: InnerTCAlias primTy primVal a}
  deriving newtype (Functor, Applicative, Monad)
  deriving
    ( HasThrow "typecheckError" (TypecheckError primTy primVal),
      HasSink "globals" (Globals primTy primVal),
      HasState "globals" (Globals primTy primVal),
      HasSource "globals" (Globals primTy primVal),
      HasReader "globals" (Globals primTy primVal)
    )
    via Lift (InnerTCAlias primTy primVal)
  deriving
    ( HasSource "param" (Param.Parameterisation primTy primVal),
      HasReader "param" (Param.Parameterisation primTy primVal)
    )
    via ReaderField "param" (InnerTCAlias primTy primVal)
  deriving
    ( HasSource "patBinds" (PatBinds primTy primVal),
      HasSink "patBinds" (PatBinds primTy primVal),
      HasState "patBinds" (PatBinds primTy primVal)
    )
    via StateField "patBinds" (InnerTCAlias primTy primVal)
  deriving
    ( HasSource "bound" (Context primTy primVal),
      HasSink "bound" (Context primTy primVal),
      HasState "bound" (Context primTy primVal)
    )
    via StateField "bound" (InnerTCAlias primTy primVal)

execInner ::
  InnerTC primTy primVal a ->
  InnerState primTy primVal ->
  EnvTypecheck primTy primVal a
execInner (InnerTC m) = evalStateT m

execInner' ::
  InnerTC primTy primVal a ->
  InnerState primTy primVal ->
  EnvTypecheck primTy primVal (a, InnerState primTy primVal)
execInner' (InnerTC m) = runStateT m
