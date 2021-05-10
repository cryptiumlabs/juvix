module Juvix.Core.Unify.Subst
  ( Subst (..),
    appSubstV,
    appSubstN,
    subst1,
    addSubst,
    idSubst,
  )
where

import Juvix.Core.IR.Evaluator.Weak
import Juvix.Core.Unify.MetaVar (MetaMap, MetaVar)
import qualified Juvix.Core.Unify.MetaVar as Meta
import Juvix.Core.Unify.Term
import Juvix.Library

newtype Subst primTy primVal = Subst {getSubst :: MetaMap (Value primTy primVal)}
  deriving newtype (Eq, Show)

data AppSubstEnv primTy primVal = AppSubstEnv
  { weaken :: Natural,
    subst :: Subst primTy primVal
  }
  deriving (Generic)

type AppSubstAlias primTy primVal = Reader (AppSubstEnv primTy primVal)

newtype AppSubst primTy primVal a = AS (AppSubstAlias primTy primVal a)
  deriving newtype (Functor, Applicative, Monad)
  deriving
    ( HasSource "weaken" Natural,
      HasReader "weaken" Natural
    )
    via Field "weaken" () (MonadReader (AppSubstAlias primTy primVal))
  deriving
    ( HasSource "subst" (Subst primTy primVal),
      HasReader "subst" (Subst primTy primVal)
    )
    via Field "subst" () (MonadReader (AppSubstAlias primTy primVal))

runAppSubst :: Subst primTy primVal -> AppSubst primTy primVal a -> a
runAppSubst σ (AS m) = runReader m env
  where
    env = AppSubstEnv {weaken = 0, subst = σ}

under :: AppSubst primTy primVal a -> AppSubst primTy primVal a
under = local @"weaken" succ

appSubstV ::
  (HasWeak primTy, HasWeak primVal) =>
  Subst primTy primVal ->
  Value primTy primVal ->
  Value primTy primVal
appSubstV σ = runAppSubst σ . appSubstV'

appSubstN ::
  (HasWeak primTy, HasWeak primVal) =>
  Subst primTy primVal ->
  Neutral primTy primVal ->
  Neutral primTy primVal
appSubstN σ = runAppSubst σ . appSubstN'

appSubstV' ::
  (HasWeak primTy, HasWeak primVal) =>
  Value primTy primVal ->
  AppSubst primTy primVal (Value primTy primVal)
appSubstV' = \case
  VStar ℓ -> pure $ VStar ℓ
  VPrimTy t -> pure $ VPrimTy t
  VPrim p -> pure $ VPrim p
  VPi π a b -> VPi π <$> appSubstV' a <*> under (appSubstV' b)
  VLam t -> VLam <$> under (appSubstV' t)
  VSig π a b -> VSig π <$> appSubstV' a <*> under (appSubstV' b)
  VPair a b -> VPair <$> appSubstV' a <*> appSubstV' b
  VUnitTy -> pure VUnitTy
  VUnit -> pure VUnit
  VNeutral n -> VNeutral <$> appSubstN' n
  v@(VMeta α) -> do
    w <- ask @"weaken"
    t <- asks @"subst" (Meta.lookupM α . getSubst)
    pure $ maybe v (weak' w) t

appSubstN' ::
  (HasWeak primTy, HasWeak primVal) =>
  Neutral primTy primVal ->
  AppSubst primTy primVal (Neutral primTy primVal)
appSubstN' = \case
  NBound i -> pure $ NBound i
  NFree x -> pure $ NFree x
  NApp f s -> NApp <$> appSubstN' f <*> appSubstV' s

subst1 ::
  (HasWeak primTy, HasWeak primVal) =>
  MetaVar ->
  Value primTy primVal ->
  Value primTy primVal ->
  Value primTy primVal
subst1 α t = appSubstV $ Subst $ Meta.singleM α t

addSubst ::
  (HasWeak primTy, HasWeak primVal) =>
  MetaVar ->
  Value primTy primVal ->
  Subst primTy primVal ->
  Subst primTy primVal
addSubst α t (Subst σ) =
  Subst $ Meta.insertM α t $ fmap (subst1 α t) σ

idSubst :: Subst primTy primVal
idSubst = Subst mempty
