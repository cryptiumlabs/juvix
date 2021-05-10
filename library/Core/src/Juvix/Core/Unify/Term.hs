{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Juvix.Core.Unify.Term
  ( T,

    -- * Term
    Term,
    pattern Star,
    pattern PrimTy,
    pattern Prim,
    pattern Pi,
    pattern Lam,
    pattern Sig,
    pattern Pair,
    pattern Let,
    pattern UnitTy,
    pattern Unit,
    pattern Elim,
    pattern Meta,
    inTerm,
    outTerm,

    -- * Elim
    Elim,
    pattern Bound,
    pattern Free,
    pattern App,
    pattern Ann,
    pattern IR.Apps,
    inElim,
    outElim,

    -- * Value
    Value,
    pattern VStar,
    pattern VPrimTy,
    pattern VPrim,
    pattern VPi,
    pattern VLam,
    pattern VSig,
    pattern VPair,
    pattern VUnitTy,
    pattern VUnit,
    pattern VNeutral,
    pattern VMeta,
    inValue,
    outValue,

    -- * Neutral
    Neutral,
    pattern NBound,
    pattern NFree,
    pattern NApp,
    pattern IR.NApps,
    inNeutral,
    outNeutral,
  )
where

import qualified Juvix.Core.IR.TransformExt as TE
import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.IR.Types.Base
import Juvix.Core.Unify.Extend
import Juvix.Core.Unify.MetaVar (MetaSet)
import qualified Juvix.Core.Unify.MetaVar as Meta
import Juvix.Library

data T

extendTerm "Term" [] [t|T|] extTerm

extendElim "Elim" [] [t|T|] extElim

injectorTE :: TE.ExtTransformTE IR.NoExt T primTy primVal
injectorTE =
  TE.ExtTransformTE
    { etStar = identity,
      etPrimTy = identity,
      etPrim = identity,
      etPi = identity,
      etSig = identity,
      etPair = identity,
      etUnitTy = identity,
      etUnit = identity,
      etLam = identity,
      etLet = identity,
      etElim = identity,
      etBound = identity,
      etFree = identity,
      etApp = identity,
      etAnn = identity,
      etTermX = absurd,
      etElimX = identity
    }

inTerm :: IR.Term primTy primVal -> Term primTy primVal
inTerm = TE.extTransformT injectorTE

inElim :: IR.Elim primTy primVal -> Elim primTy primVal
inElim = TE.extTransformE injectorTE

data UnsolvedMetas a = Unsolved MetaSet | Solved a deriving (Functor)

instance Applicative UnsolvedMetas where
  pure = Solved
  Unsolved xs <*> Unsolved ys = Unsolved $ xs <> ys
  Unsolved xs <*> Solved _ = Unsolved xs
  Solved _ <*> Unsolved ys = Unsolved ys
  Solved f <*> Solved x = Solved $ f x

ejectorTE :: TE.ExtTransformTEF UnsolvedMetas T IR.NoExt primTy primVal
ejectorTE =
  TE.ExtTransformTEF
    { etfStar = pure,
      etfPrimTy = pure,
      etfPrim = pure,
      etfPi = pure,
      etfLam = pure,
      etfSig = pure,
      etfUnitTy = pure,
      etfUnit = pure,
      etfPair = pure,
      etfLet = pure,
      etfElim = pure,
      etfBound = pure,
      etfFree = pure,
      etfApp = pure,
      etfAnn = pure,
      etfTermX = Unsolved . Meta.singleS,
      etfElimX = pure
    }

outTerm :: Term primTy primVal -> UnsolvedMetas (IR.Term primTy primVal)
outTerm = TE.extTransformTF ejectorTE

outElim :: Elim primTy primVal -> UnsolvedMetas (IR.Elim primTy primVal)
outElim = TE.extTransformEF ejectorTE

extendValue "Value" [] [t|T|] extValue

extendNeutral "Neutral" [] [t|T|] extNeutral

injectorVN :: TE.ExtTransformVN IR.NoExt T primTy primVal
injectorVN =
  TE.ExtTransformVN
    { etVStar = identity,
      etVPrimTy = identity,
      etVPrim = identity,
      etVPi = identity,
      etVSig = identity,
      etVPair = identity,
      etVUnitTy = identity,
      etVUnit = identity,
      etVLam = identity,
      etVNeutral = identity,
      etNBound = identity,
      etNFree = identity,
      etNApp = identity,
      etValueX = absurd,
      etNeutralX = identity
    }

inValue :: IR.Value primTy primVal -> Value primTy primVal
inValue = TE.extTransformV injectorVN

inNeutral :: IR.Neutral primTy primVal -> Neutral primTy primVal
inNeutral = TE.extTransformN injectorVN

ejectorVN :: TE.ExtTransformVNF UnsolvedMetas T IR.NoExt primTy primVal
ejectorVN =
  TE.ExtTransformVNF
    { etfVStar = pure,
      etfVPrimTy = pure,
      etfVPrim = pure,
      etfVPi = pure,
      etfVLam = pure,
      etfVSig = pure,
      etfVUnitTy = pure,
      etfVUnit = pure,
      etfVPair = pure,
      etfVNeutral = pure,
      etfNBound = pure,
      etfNFree = pure,
      etfNApp = pure,
      etfValueX = Unsolved . Meta.singleS,
      etfNeutralX = pure
    }

outValue :: Value primTy primVal -> UnsolvedMetas (IR.Value primTy primVal)
outValue = TE.extTransformVF ejectorVN

outNeutral :: Neutral primTy primVal -> UnsolvedMetas (IR.Neutral primTy primVal)
outNeutral = TE.extTransformNF ejectorVN
