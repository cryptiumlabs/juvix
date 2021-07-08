{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- This includes the evaluators (`evalTerm` and `evalElim`),
-- the value application function (`vapp`) and
-- the substitution functions (`substV`).
module Juvix.Core.IR.Evaluator
  ( inlineAllGlobals,
    evalTerm,
    NoExtensions,
    CanEval,
    EvalPatSubst,
    toLambdaR,
    lookupFun,
    lookupFun',
    rawLookupFun',
    module Juvix.Core.IR.Evaluator.Types,
    module Juvix.Core.IR.Evaluator.Weak,
    module Juvix.Core.IR.Evaluator.SubstV,
    module Juvix.Core.IR.Evaluator.PatSubst,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import qualified Data.IntMap.Strict as PM
import Juvix.Core.Base.TransformExt
import qualified Juvix.Core.Base.TransformExt as TransformExt
import qualified Juvix.Core.Base.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.Base.Types as Core
import Juvix.Core.IR.Evaluator.PatSubst
import Juvix.Core.IR.Evaluator.SubstV
import Juvix.Core.IR.Evaluator.Types
import Juvix.Core.IR.Evaluator.Weak
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library

-- | Constraint for terms and eliminations without extensions.
type NoExtensions ext primTy primVal =
  ( Core.TermX ext primTy primVal ~ Void,
    Core.ElimX ext primTy primVal ~ Void,
    TransformExt.ForgotExt ext primTy primVal
  )

type EvalPatSubst ext primTy primVal =
  ( HasPatSubst (OnlyExts.T ext) primTy primVal (Core.TermX ext primTy primVal),
    HasPatSubst (OnlyExts.T ext) primTy primVal (Core.ElimX ext primTy primVal),
    -- FIXME?
    HasPatSubstTerm (OnlyExts.T ext) primTy primVal primTy,
    HasPatSubstTerm (OnlyExts.T ext) primTy primVal primVal
  )

-- | Alias for a type constraint for terms that can be evaluated.
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

-- | Perform inlining of variables to globals in the input program.
inlineAllGlobals ::
  ( EvalPatSubst ext primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  -- | Term to perform inlining on.
  Core.Term' ext primTy primVal ->
  -- | Lookup function used to dereference the variables.
  LookupFun ext primTy primVal ->
  -- | Maps @Int@s to @GlobalName@s. Passed to @inlineAllGlobalsElim@ as a pattern is an integer encoded in a free variable
  Core.PatternMap Core.GlobalName ->
  Core.Term' ext primTy primVal
inlineAllGlobals t lookupFun patternMap =
  case t of
    Core.Unit' {} -> t
    Core.UnitTy' {} -> t
    Core.Pair' p1 p2 ann ->
      Core.Pair' (inlineAllGlobals p1 lookupFun patternMap) (inlineAllGlobals p2 lookupFun patternMap) ann
    Core.Elim' elim ann ->
      Core.Elim' (inlineAllGlobalsElim elim lookupFun patternMap) ann
    Core.Sig' u t1 t2 ann ->
      Core.Sig' u (inlineAllGlobals t1 lookupFun patternMap) (inlineAllGlobals t2 lookupFun patternMap) ann
    Core.Let' u e t ann ->
      Core.Let' u (inlineAllGlobalsElim e lookupFun patternMap) (inlineAllGlobals t lookupFun patternMap) ann
    Core.Lam' t ann ->
      Core.Lam' (inlineAllGlobals t lookupFun patternMap) ann
    Core.Pi' u t1 t2 ann ->
      Core.Pi' u (inlineAllGlobals t1 lookupFun patternMap) (inlineAllGlobals t2 lookupFun patternMap) ann
    Core.Prim' {} -> t
    Core.PrimTy' {} -> t
    Core.Star' {} -> t
    Core.TermX {} -> t

-- | Perform inling of references to globals in an elimination.
inlineAllGlobalsElim ::
  ( EvalPatSubst ext primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  -- | Elimination to perform inlining on.
  Core.Elim' ext primTy primVal ->
  -- | Lookup function used to dereference the variables.
  LookupFun ext primTy primVal ->
  Core.PatternMap Core.GlobalName ->
  Core.Elim' ext primTy primVal
inlineAllGlobalsElim t lookupFun patternMap =
  case t of
    Core.Bound' {} -> t
    Core.Free' (Core.Global name) _ann -> fromMaybe t $ lookupFun name
    Core.Free' (Core.Pattern i) _ -> fromMaybe t $ PM.lookup i patternMap >>= lookupFun
    Core.App' elim term ann ->
      Core.App' (inlineAllGlobalsElim elim lookupFun patternMap) (inlineAllGlobals term lookupFun patternMap) ann
    Core.Ann' u t1 t2 uni ann ->
      Core.Ann' u (inlineAllGlobals t1 lookupFun patternMap) (inlineAllGlobals t2 lookupFun patternMap) uni ann
    Core.ElimX {} -> t

-- | Evaluate a term with extensions, discards annotations but keeps the
-- extensions.
evalTermWith ::
  CanEval extT extG primTy primVal =>
  -- | Lookup function for globals that may be present in the term.
  LookupFun extG primTy primVal ->
  -- | Functions to transform term and elim extensions.
  ExtFuns extG extT primTy primVal ->
  -- | The term to evaluate.
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

-- | Evaluate an elimination with extensions, discards annotations but keeps
-- the extensions.
evalElimWith ::
  CanEval extT extG primTy primVal =>
  -- | Lookup function for globals that may be present in the elimination.
  LookupFun extG primTy primVal ->
  -- | Functions to transform term and elim extensions.
  ExtFuns extG extT primTy primVal ->
  -- | The elimination to evaluate.
  Core.Elim' (OnlyExts.T extT) primTy primVal ->
  Either (Error IR.T extT primTy primVal) (IR.Value primTy primVal)
evalElimWith _ _ (Core.Bound' i _) =
  pure $ Core.VBound i
evalElimWith g exts (Core.Free' x _)
  | Core.Global x <- x,
    Just e <- g x =
    evalElimWith g exts $ toOnlyExtsE e
  | otherwise = pure $ Core.VFree x
evalElimWith g exts (Core.App' s t _) =
  join $
    vapp <$> evalElimWith g exts s
      <*> evalTermWith g exts t
      <*> pure ()
evalElimWith g exts (Core.Ann' _ s _ _ _) =
  evalTermWith g exts s
evalElimWith g exts (Core.ElimX a) =
  eExtFun exts g a

-- | Evaluate a term, discarding annotations.
-- Throws 'UnsupportedTermExt' or 'UnsupportedElimExt' if the
-- input contains any extension constructors.
evalTerm ::
  CanEval extT extG primTy primVal =>
  -- | Lookup function for globals that may be present in the term.
  LookupFun extG primTy primVal ->
  -- | Term to evaluate.
  Core.Term' extT primTy primVal ->
  Either (Error IR.T extT primTy primVal) (IR.Value primTy primVal)
evalTerm g t = evalTermWith g rejectExts $ OnlyExts.onlyExtsT t

-- | Translate a function termin into an elimination.
-- The arguments to this function are basically the components of a function.
--
-- @
-- foo : int -> int
-- foo x = add x x
-- @
--
-- becomes:
--
-- @
-- (ω | \x -> add x x : int -> int)
-- @
toLambda' ::
  forall ext' ext primTy primVal.
  ( EvalPatSubst ext' primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  -- | Usage information of the function.
  Core.GlobalUsage ->
  -- | The type of the function.
  Core.Term' IR.T primTy primVal ->
  -- | List of arguments to the function.
  [Core.Pattern' ext primTy primVal] ->
  -- | The body of the function.
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

-- | Extract variable from a pattern definition.
singleVar :: Alternative f => Core.Pattern' ext primTy primVal -> f Core.PatternVar
singleVar (Core.PVar' p _) = pure p
singleVar _ = empty

-- | Discard annotations from a `Term'`, but keeps the extended constructors.
-- Requires the input to be free from any extensions, allowing the result to
-- have other extensions.
toOnlyExtsT ::
  ( NoExtensions ext1 primTy primVal
  ) =>
  Core.Term' ext1 primTy primVal ->
  Core.Term' (OnlyExts.T ext2) primTy primVal
toOnlyExtsT = extTransformT $ OnlyExts.injector

-- | Discard annotations from a `ELim'`, but keeps the extended constructors.
-- Requires the input to be free from any extensions, allowing the result to
-- have other extensions.
toOnlyExtsE ::
  ( NoExtensions ext1 primTy primVal
  ) =>
  Core.Elim' ext1 primTy primVal ->
  Core.Elim' (OnlyExts.T ext2) primTy primVal
toOnlyExtsE = extTransformE $ OnlyExts.injector

-- | Translate a `Global'` function definition into an elimination,
-- consisting of a λ-term with a type annotation. For example:
--
-- @
-- foo : int -> int
-- foo x = add x x
-- @
--
-- becomes:
--
-- @
-- (ω | \x -> add x x : int -> int)
-- @
toLambda ::
  forall ext ext' primTy primVal.
  ( EvalPatSubst ext' primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  Core.Global' IR.T ext primTy primVal ->
  Maybe (Core.Elim' (OnlyExts.T ext') primTy primVal)
toLambda (Core.GFunction (Core.Function {funUsage = π, funType = ty, funClauses}))
  | Core.FunClause _ pats rhs _ _ _ :| [] <- funClauses =
    toLambda' π (Core.quote ty) pats rhs
toLambda _ = Nothing

-- | Translate a `RawGlobal'` function definition into an elimination term.
-- See 'toLambda' for an example.
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

-- | Given an environment of global definitions, and a name to lookup,
-- translate the (possible) term into an elimination.
lookupFun ::
  forall ext' ext primTy primVal.
  ( EvalPatSubst ext' primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  Core.Globals' IR.T ext primTy primVal ->
  LookupFun (OnlyExts.T ext') primTy primVal
lookupFun globals x =
  HashMap.lookup x globals >>= toLambda

-- | Variant of `lookupFun` that works on `RawGlobals'`.
rawLookupFun ::
  forall ext' ext primTy primVal.
  ( EvalPatSubst ext' primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  Core.RawGlobals' ext primTy primVal ->
  LookupFun (OnlyExts.T ext') primTy primVal
rawLookupFun globals x =
  HashMap.lookup x globals >>= toLambdaR

-- | Variant of `lookupFun` that creates a extension free elimination.
lookupFun' ::
  EvalPatSubst IR.T primTy primVal =>
  IR.Globals primTy primVal ->
  LookupFun IR.T primTy primVal
lookupFun' globals x = lookupFun @IR.T globals x >>| extForgetE

-- | Variant of `lookupFun'` that works on `RawGlobals`.
rawLookupFun' ::
  EvalPatSubst IR.T primTy primVal =>
  IR.RawGlobals primTy primVal ->
  LookupFun IR.T primTy primVal
rawLookupFun' globals x = rawLookupFun @IR.T globals x >>| extForgetE
