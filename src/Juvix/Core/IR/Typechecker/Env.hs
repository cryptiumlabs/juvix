module Juvix.Core.IR.Typechecker.Env where

import Data.HashMap.Strict (HashMap)
import Juvix.Core.IR.Typechecker.Types
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Datatype)

data EnvCtx primTy primVal = EnvCtx
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



type TContext primTy primVal = [IR.Value primTy primVal]

lookupT :: TContext primTy primVal
        -> IR.BoundVar
        -> Maybe (IR.Value primTy primVal)
lookupT gam x = Eval.weakBy (x + 1) <$> atMay gam (fromIntegral x)


type UContext primTy primVal = [Usage.T]

lookupU :: UContext primTy primVal -> IR.BoundVar -> Maybe Usage.T
lookupU psi x = atMay psi (fromIntegral x)


type Context primTy primVal = [(IR.Value primTy primVal, Usage.T)]


type PatBinds primTy primVal =
  IntMap (IR.Value primTy primVal)


type IsTC primTy primVal m =
  ( HasThrowTC primTy primVal m,
    HasReader "globals" (Globals primTy primVal) m
  )

data InnerState primTy primVal =
  InnerState {
    param :: Param.Parameterisation primTy primVal,
    patBinds :: PatBinds primTy primVal,
    boundTypes :: TContext primTy primVal,
    boundUsages :: UContext primTy primVal
  }
  deriving Generic

type InnerTCAlias primTy primVal =
  StateT (InnerState primTy primVal) (EnvTypecheck primTy primVal)

newtype InnerTC primTy primVal a =
  InnerTC {unInnerTC :: InnerTCAlias primTy primVal a}
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
      HasReader "patBinds" (PatBinds primTy primVal)
    )
    via ReaderField "patBinds" (InnerTCAlias primTy primVal)
  deriving
    ( HasSource "boundTypes" (TContext primTy primVal),
      HasSink "boundTypes" (TContext primTy primVal),
      HasState "boundTypes" (TContext primTy primVal)
    )
    via StateField "boundTypes" (InnerTCAlias primTy primVal)
  deriving
    ( HasSource "boundUsages" (UContext primTy primVal),
      HasSink "boundUsages" (UContext primTy primVal),
      HasState "boundUsages" (UContext primTy primVal)
    )
    via StateField "boundUsages" (InnerTCAlias primTy primVal)


execInner :: InnerTC primTy primVal a
          -> InnerState primTy primVal
          -> EnvTypecheck primTy primVal a
execInner (InnerTC m) = evalStateT m

execInner' :: InnerTC primTy primVal a
           -> InnerState primTy primVal
           -> EnvTypecheck primTy primVal (a, InnerState primTy primVal)
execInner' (InnerTC m) = runStateT m
