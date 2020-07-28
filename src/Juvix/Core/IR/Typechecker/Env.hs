module Juvix.Core.IR.Typechecker.Env where

import Data.Foldable (foldr1) -- (on a NonEmpty)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Juvix.Core.IR.Evaluator as Eval
import Juvix.Core.IR.Typechecker.Types
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Datatype)

data EnvCtx' ext primTy primVal
  = EnvCtx
      { globals :: Globals' ext primTy primVal
      }
  deriving Generic

type EnvCtx = EnvCtx' IR.NoExt

type Globals' ext primTy primVal =
  HashMap IR.GlobalName (Global' ext primTy primVal)

type Globals primTy primVal = Globals' IR.NoExt primTy primVal

data Global' ext primTy primVal
  = GDatatype (IR.Datatype' ext primTy primVal)
  | GDataCon (IR.DataCon' ext primTy primVal)
  | GFunction (IR.Function' ext primTy primVal)
  | GAbstract IR.GlobalUsage (IR.Value' ext primTy primVal)
  deriving ({-Show, Eq,-} Generic)

type Global = Global' IR.NoExt

type EnvAlias' ext primTy primVal =
  ExceptT
    (TypecheckError' ext primTy primVal)
    (State (EnvCtx' ext primTy primVal))

newtype EnvTypecheck' ext primTy primVal a =
    EnvTyp (EnvAlias' ext primTy primVal a)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasThrow "typecheckError" (TypecheckError' ext primTy primVal)
    )
    via MonadError (EnvAlias' ext primTy primVal)
  deriving
    ( HasSource "globals" (Globals' ext primTy primVal),
      HasReader "globals" (Globals' ext primTy primVal)
    )
    via ReaderField "globals" (EnvAlias' ext primTy primVal)

type EnvTypecheck = EnvTypecheck' IR.NoExt

type HasGlobals' ext primTy primVal =
  HasReader "globals" (Globals' ext primTy primVal)

type HasGlobals primTy primVal = HasGlobals' IR.NoExt primTy primVal

type CanTC' ext primTy primVal m =
  (HasThrowTC' ext primTy primVal m,
   HasGlobals' ext primTy primVal m)

type CanTC primTy primVal m = CanTC' IR.NoExt primTy primVal m

exec ::
  Globals primTy primVal ->
  EnvTypecheck primTy primVal a ->
  (Either (TypecheckError primTy primVal) a, EnvCtx primTy primVal)
exec globals (EnvTyp env) =
  runState (runExceptT env) $ EnvCtx globals

type Context' ext primTy primVal = [Annotation' ext primTy primVal]
type Context primTy primVal = Context' IR.NoExt primTy primVal

lookupCtx ::
  (IR.ValueAll Eval.HasWeak ext primTy primVal,
   IR.NeutralAll Eval.HasWeak ext primTy primVal) =>
  Context' ext primTy primVal ->
  IR.BoundVar ->
  Maybe (Annotation' ext primTy primVal)
lookupCtx gam x = do
  Annotation π ty <- atMay gam (fromIntegral x)
  pure $ Annotation π (Eval.weakBy (x + 1) ty)

lookupGlobal ::
  (HasGlobals primTy primVal m, HasThrowTC primTy primVal m) =>
  IR.GlobalName ->
  m (IR.Value primTy primVal, IR.GlobalUsage)
lookupGlobal x = do
  mdefn <- asks @"globals" $ HashMap.lookup x
  case mdefn of
    Just defn -> pure $ makeGAnn defn
    Nothing -> throwTC (UnboundGlobal x)
  where
    makeGAnn (GDatatype (IR.Datatype {dataArgs, dataLevel})) =
      (foldr makePi (IR.VStar' dataLevel mempty) dataArgs, IR.GZero)
    makeGAnn (GDataCon (IR.DataCon {conType})) =
      (conType, IR.GOmega)
    makeGAnn (GFunction (IR.Function {funType, funUsage})) =
      (funType, funUsage)
    makeGAnn (GAbstract absUsage absType) =
      (absType, absUsage)
    makePi (IR.DataArg {argUsage, argType}) res =
      IR.VPi' argUsage argType res mempty

primType :: HasReader "param" (Param.Parameterisation primTy primVal) m
         => primVal -> m (IR.Value primTy primVal)
primType v = do
  asks @"param" \param ->
    Param.typeOf param v
      |> fmap IR.VPrimTy
      |> foldr1 (IR.VPi Usage.Omega)

type UContext primTy primVal = [Usage.T]

type PatBinds' ext primTy primVal = IntMap (Annotation' ext primTy primVal)
type PatBinds primTy primVal = PatBinds' IR.NoExt primTy primVal

type PatUsages primTy primVal = IntMap Usage.T

data InnerState' ext primTy primVal
  = InnerState
      { param :: Param.Parameterisation primTy primVal,
        patBinds :: PatBinds' ext primTy primVal,
        bound :: Context' ext primTy primVal
      }
  deriving (Generic)

type InnerState = InnerState' IR.NoExt

type InnerTCAlias' ext primTy primVal =
  StateT (InnerState' ext primTy primVal) (EnvTypecheck' ext primTy primVal)

newtype InnerTC' ext primTy primVal a =
    InnerTC (InnerTCAlias' ext primTy primVal a)
  deriving newtype (Functor, Applicative, Monad)
  deriving
    ( HasThrow "typecheckError" (TypecheckError' ext primTy primVal),
      HasSource "globals" (Globals' ext primTy primVal),
      HasReader "globals" (Globals' ext primTy primVal)
    )
    via Lift (InnerTCAlias' ext primTy primVal)
  deriving
    ( HasSource "param" (Param.Parameterisation primTy primVal),
      HasReader "param" (Param.Parameterisation primTy primVal)
    )
    via ReaderField "param" (InnerTCAlias' ext primTy primVal)
  deriving
    ( HasSource "patBinds" (PatBinds' ext primTy primVal),
      HasSink "patBinds" (PatBinds' ext primTy primVal),
      HasState "patBinds" (PatBinds' ext primTy primVal)
    )
    via StateField "patBinds" (InnerTCAlias' ext primTy primVal)
  deriving
    ( HasSource "bound" (Context' ext primTy primVal),
      HasSink "bound" (Context' ext primTy primVal),
      HasState "bound" (Context' ext primTy primVal)
    )
    via StateField "bound" (InnerTCAlias' ext primTy primVal)

type InnerTC = InnerTC' IR.NoExt

type HasParam primTy primVal =
  HasReader "param" (Param.Parameterisation primTy primVal)

type HasPatBinds' ext primTy primVal =
  HasState "patBinds" (PatBinds' ext primTy primVal)

type HasPatBinds primTy primVal = HasPatBinds' IR.NoExt primTy primVal

type HasBound' ext primTy primVal =
  HasState "bound" (Context' ext primTy primVal)

type HasBound primTy primVal = HasBound' IR.NoExt primTy primVal

type CanInnerTC' ext primTy primVal m =
  (CanTC' ext primTy primVal m,
   HasParam primTy primVal m,
   HasPatBinds' ext primTy primVal m,
   HasBound' ext primTy primVal m)

type CanInnerTC primTy primVal m = CanInnerTC' IR.NoExt primTy primVal m

execInner ::
  InnerTC' ext primTy primVal a ->
  InnerState' ext primTy primVal ->
  EnvTypecheck' ext primTy primVal a
execInner (InnerTC m) = evalStateT m

execInner' ::
  InnerTC' ext primTy primVal a ->
  InnerState' ext primTy primVal ->
  EnvTypecheck' ext primTy primVal (a, InnerState' ext primTy primVal)
execInner' (InnerTC m) = runStateT m
