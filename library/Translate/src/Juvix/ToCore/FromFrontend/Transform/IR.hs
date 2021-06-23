{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Juvix.ToCore.FromFrontend.Transform.IR (transformTermIR) where

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NonEmpty
import qualified Generics.SYB as SYB
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Core.Translate (hrToIR)
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Sexp as Sexp
import Juvix.ToCore.FromFrontend.Transform.HR
import Juvix.ToCore.FromFrontend.Transform.Helpers
  ( ReduceEff,
    getParamConstant,
    getSpecialSig,
    lookupSigWithSymbol,
    parseVarArg,
    parseVarPat,
    toElim,
  )
import Juvix.ToCore.FromFrontend.Transform.Usage
import Juvix.ToCore.Types
  ( Error (..),
    HasPatVars,
    Special (..),
    throwFF,
  )
import Prelude (error)

transformTermIR ::
  ( Show primTy,
    Show primVal,
    ReduceEff primTy primVal m,
    HasPatVars m
  ) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m (IR.Term primTy primVal)
transformTermIR q fe = do
  transformAllPatVars . hrToIR =<< transformTermHR q fe

transformAllPatVars ::
  HasPatVars m => IR.Term primTy primVal -> m (IR.Term primTy primVal)
transformAllPatVars = \case
  IR.Star ℓ -> pure $ IR.Star ℓ
  IR.PrimTy t -> pure $ IR.PrimTy t
  IR.Prim p -> pure $ IR.Prim p
  IR.Pi π a b -> IR.Pi π <$> transformAllPatVars a <*> transformAllPatVars b
  IR.Lam t -> IR.Lam <$> transformAllPatVars t
  IR.Sig π a b -> IR.Sig π <$> transformAllPatVars a <*> transformAllPatVars b
  IR.Pair s t -> IR.Pair <$> transformAllPatVars s <*> transformAllPatVars t
  IR.Let π e b -> IR.Let π <$> transformAllPatVarsE e <*> transformAllPatVars b
  IR.UnitTy -> pure IR.UnitTy
  IR.Unit -> pure IR.Unit
  IR.Elim e -> IR.Elim <$> transformAllPatVarsE e

transformAllPatVarsE ::
  HasPatVars m => IR.Elim primTy primVal -> m (IR.Elim primTy primVal)
transformAllPatVarsE = \case
  IR.Bound i -> pure $ IR.Bound i
  IR.Free x -> IR.Free <$> transformPatVar x
  IR.App f s -> IR.App <$> transformAllPatVarsE f <*> transformAllPatVars s
  IR.Ann π s a ℓ ->
    IR.Ann π <$> transformAllPatVars s <*> transformAllPatVars a <*> pure ℓ

transformPatVar :: HasPatVars m => IR.Name -> m IR.Name
transformPatVar orig@(IR.Global name) =
  gets @"patVars" $ maybe orig IR.Pattern . HM.lookup name
transformPatVar orig = pure orig
