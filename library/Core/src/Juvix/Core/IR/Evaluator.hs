{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- This includes the evaluators (evalTerm and evalElim),
-- the value application function (vapp) and
-- the substitution functions (substTerm and substElim).
module Juvix.Core.IR.Evaluator
  ( module Juvix.Core.IR.Evaluator,
    module Juvix.Core.IR.Evaluator.Types,
    module Juvix.Core.IR.Evaluator.Weak,
    module Juvix.Core.IR.Evaluator.Subst,
    module Juvix.Core.IR.Evaluator.SubstV,
    module Juvix.Core.IR.Evaluator.PatSubst,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import qualified Juvix.Core.Base.Types as Core
import Juvix.Core.IR.Evaluator.PatSubst
import Juvix.Core.IR.Evaluator.Subst
import Juvix.Core.IR.Evaluator.SubstV
import Juvix.Core.IR.Evaluator.Types
import Juvix.Core.IR.Evaluator.Weak
import Juvix.Core.IR.TransformExt
import qualified Juvix.Core.IR.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library

type NoExtensions ext primTy primVal =
  ( Core.TermX ext primTy primVal ~ Void,
    Core.ElimX ext primTy primVal ~ Void
  )

type EvalPatSubst ext primTy primVal =
  ( HasPatSubst (OnlyExts.T ext) primTy primVal (Core.TermX ext primTy primVal),
    HasPatSubst (OnlyExts.T ext) primTy primVal (Core.ElimX ext primTy primVal),
    -- FIXME?
    HasPatSubstTerm (OnlyExts.T ext) primTy primVal primTy,
    HasPatSubstTerm (OnlyExts.T ext) primTy primVal primVal
  )

-- |
-- * @extT@: extension of current term
-- * @extG@: extension of terms in globals
type CanEval extT extG primTy primVal =
  ( Param.CanApply primTy,
    Param.CanApply primVal,
    EvalPatSubst extT primTy primVal,
    -- no extensions (only annotations) allowed in global context
    NoExtensions extG primTy primVal,
    HasSubstValue IR.T primTy primVal primTy,
    HasSubstValue IR.T primTy primVal primVal
  )

inlineAllGlobals ::
  ( EvalPatSubst ext primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  Core.Term' ext primTy primVal ->
  LookupFun ext primTy primVal ->
  Core.Term' ext primTy primVal
inlineAllGlobals t map =
  case t of
    Core.Unit' {} -> t
    Core.UnitTy' {} -> t
    Core.Pair' p1 p2 ann ->
      Core.Pair' (inlineAllGlobals p1 map) (inlineAllGlobals p2 map) ann
    Core.Elim' elim ann ->
      Core.Elim' (inlineAllGlobalsElim elim map) ann
    Core.Sig' u t1 t2 ann ->
      Core.Sig' u (inlineAllGlobals t1 map) (inlineAllGlobals t2 map) ann
    Core.Let' u e t ann ->
      Core.Let' u (inlineAllGlobalsElim e map) (inlineAllGlobals t map) ann
    Core.Lam' t ann ->
      Core.Lam' (inlineAllGlobals t map) ann
    Core.Pi' u t1 t2 ann ->
      Core.Pi' u (inlineAllGlobals t1 map) (inlineAllGlobals t2 map) ann
    Core.Prim' {} -> t
    Core.PrimTy' {} -> t
    Core.Star' {} -> t
    Core.TermX {} -> t

inlineAllGlobalsElim ::
  ( EvalPatSubst ext primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  Core.Elim' ext primTy primVal ->
  LookupFun ext primTy primVal ->
  Core.Elim' ext primTy primVal
inlineAllGlobalsElim t map =
  case t of
    Core.Bound' {} -> t
    Core.Free' (Core.Global name) _ann -> fromMaybe t $ map name
    Core.Free' {} -> t
    Core.App' elim term ann ->
      Core.App' (inlineAllGlobalsElim elim map) (inlineAllGlobals term map) ann
    Core.Ann' u t1 t2 uni ann ->
      Core.Ann' u (inlineAllGlobals t1 map) (inlineAllGlobals t2 map) uni ann
    Core.ElimX {} -> t

-- annotations are discarded
evalTermWith ::
  CanEval extT extG primTy primVal =>
  LookupFun extG primTy primVal ->
  ExtFuns extG extT primTy primVal ->
  Core.Term' (OnlyExts.T extT) primTy primVal ->
  Either (Error IR.T extT primTy primVal) (IR.Value primTy primVal)
evalTermWith _ _ (Core.Star' u _) =
  pure $ IR.VStar u
evalTermWith _ _ (Core.PrimTy' p _) =
  pure $ IR.VPrimTy p
evalTermWith _ _ (Core.Prim' p _) =
  pure $ IR.VPrim p
evalTermWith g exts (Core.Pi' π s t _) =
  IR.VPi π <$> evalTermWith g exts s <*> evalTermWith g exts t
evalTermWith g exts (Core.Lam' t _) =
  IR.VLam <$> evalTermWith g exts t
evalTermWith g exts (Core.Sig' π s t _) =
  IR.VSig π <$> evalTermWith g exts s <*> evalTermWith g exts t
evalTermWith g exts (Core.Pair' s t _) =
  IR.VPair <$> evalTermWith g exts s <*> evalTermWith g exts t
evalTermWith _ _ (Core.UnitTy' _) =
  pure IR.VUnitTy
evalTermWith _ _ (Core.Unit' _) =
  pure IR.VUnit
evalTermWith g exts (Core.Let' _ l b _) = do
  l' <- evalElimWith g exts l
  b' <- evalTermWith g exts b
  substV l' b'
evalTermWith g exts (Core.Elim' e _) =
  evalElimWith g exts e
evalTermWith g exts (Core.TermX a) =
  tExtFun exts g a

evalElimWith ::
  CanEval extT extG primTy primVal =>
  LookupFun extG primTy primVal ->
  ExtFuns extG extT primTy primVal ->
  Core.Elim' (OnlyExts.T extT) primTy primVal ->
  Either (Error IR.T extT primTy primVal) (IR.Value primTy primVal)
evalElimWith _ _ (Core.Bound' i _) =
  pure $ IR.VBound i
evalElimWith g exts (Core.Free' x _)
  | Core.Global x <- x,
    Just e <- g x =
    evalElimWith g exts $ toOnlyExtsE e
  | otherwise = pure $ IR.VFree x
evalElimWith g exts (Core.App' s t _) =
  join $
    vapp <$> evalElimWith g exts s
      <*> evalTermWith g exts t
      <*> pure ()
evalElimWith g exts (Core.Ann' _ s _ _ _) =
  evalTermWith g exts s
evalElimWith g exts (Core.ElimX a) =
  eExtFun exts g a

evalTerm ::
  CanEval extT extG primTy primVal =>
  LookupFun extG primTy primVal ->
  Core.Term' extT primTy primVal ->
  Either (Error IR.T extT primTy primVal) (IR.Value primTy primVal)
evalTerm g t = evalTermWith g rejectExts $ OnlyExts.onlyExtsT t

evalElim ::
  CanEval extT extG primTy primVal =>
  LookupFun extG primTy primVal ->
  Core.Elim' extT primTy primVal ->
  Either (Error IR.T extT primTy primVal) (IR.Value primTy primVal)
evalElim g e = evalElimWith g rejectExts $ OnlyExts.onlyExtsE e

-- TODO generalise the @IR.T@s
toLambda' ::
  forall ext' ext primTy primVal.
  ( EvalPatSubst ext' primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  Core.GlobalUsage ->
  IR.Term primTy primVal ->
  [Core.Pattern' ext primTy primVal] ->
  Core.Term' ext primTy primVal ->
  Maybe (Core.Elim' (OnlyExts.T ext') primTy primVal)
toLambda' π' ty' pats rhs = do
  patVars <- traverse singleVar pats
  let len = fromIntegral $ length patVars
  let vars = map bound $ genericTake len (iterate (subtract 1) (len - 1))
  let patMap = IntMap.fromList $ zip patVars vars
  let π = IR.globalToUsage π'
  let ty = OnlyExts.injectT ty'
  case patSubst patMap $ weakBy len $ toOnlyExtsT rhs of
    Left _ -> Nothing
    Right x -> pure $ IR.Ann π (applyN len lam x) ty 0 -- FIXME universe
  where
    applyN 0 _ x = x
    applyN n f x = applyN (n - 1) f (f $! x)
    bound :: Core.BoundVar -> Core.Elim' (OnlyExts.T ext') primTy primVal
    bound x = Core.Bound' x ()
    lam ::
      Core.Term' (OnlyExts.T z) primTy primVal ->
      Core.Term' (OnlyExts.T z) primTy primVal
    lam x = Core.Lam' x ()

singleVar :: Alternative f => Core.Pattern' ext primTy primVal -> f Core.PatternVar
singleVar (Core.PVar' p _) = pure p
singleVar _ = empty

toOnlyExtsT ::
  NoExtensions ext1 primTy primVal =>
  Core.Term' ext1 primTy primVal ->
  Core.Term' (OnlyExts.T ext2) primTy primVal
toOnlyExtsT = extTransformT $ OnlyExts.injector `compose` forgetter

toOnlyExtsE ::
  NoExtensions ext1 primTy primVal =>
  Core.Elim' ext1 primTy primVal ->
  Core.Elim' (OnlyExts.T ext2) primTy primVal
toOnlyExtsE = extTransformE $ OnlyExts.injector `compose` forgetter

toLambda ::
  forall ext ext' primTy primVal.
  ( EvalPatSubst ext' primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  Core.Global' IR.T ext primTy primVal ->
  Maybe (Core.Elim' (OnlyExts.T ext') primTy primVal)
toLambda (Core.GFunction (Core.Function {funUsage = π, funType = ty, funClauses}))
  | Core.FunClause _ pats rhs _ _ _ :| [] <- funClauses =
    toLambda' π (IR.quote ty) pats rhs
toLambda _ = Nothing

toLambdaR ::
  forall ext' ext primTy primVal.
  ( EvalPatSubst ext' primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  Core.RawGlobal' ext primTy primVal ->
  Maybe (Core.Elim' (OnlyExts.T ext') primTy primVal)
toLambdaR (Core.RawGFunction f)
  | Core.RawFunction {rawFunUsage = π, rawFunType = ty, rawFunClauses} <- f,
    Core.RawFunClause _ pats rhs _ :| [] <- rawFunClauses =
    toLambda' π (extForgetT ty) pats rhs
toLambdaR _ = Nothing

lookupFun ::
  forall ext' ext primTy primVal.
  ( EvalPatSubst ext' primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  Core.Globals' IR.T ext primTy primVal ->
  LookupFun (OnlyExts.T ext') primTy primVal
lookupFun globals x =
  HashMap.lookup x globals >>= toLambda

rawLookupFun ::
  forall ext' ext primTy primVal.
  ( EvalPatSubst ext' primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  Core.RawGlobals' ext primTy primVal ->
  LookupFun (OnlyExts.T ext') primTy primVal
rawLookupFun globals x =
  HashMap.lookup x globals >>= toLambdaR

lookupFun' ::
  EvalPatSubst IR.T primTy primVal =>
  IR.Globals primTy primVal ->
  LookupFun IR.T primTy primVal
lookupFun' globals x = lookupFun @IR.T globals x >>| extForgetE

rawLookupFun' ::
  EvalPatSubst IR.T primTy primVal =>
  IR.RawGlobals primTy primVal ->
  LookupFun IR.T primTy primVal
rawLookupFun' globals x = rawLookupFun @IR.T globals x >>| extForgetE
