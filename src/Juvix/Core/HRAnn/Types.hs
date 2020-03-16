module Juvix.Core.HRAnn.Types where

import Juvix.Library
import qualified Extensible as Ext
import qualified Juvix.Core.IR.Types.Base
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Usage as Usage

data T

-- TODO: add combinators to @extensible-data@ for pairing like this
IR.extendTerm "Term" [t|T|] $ IR.defaultExtTerm {
  IR.nameLam = "Lam0",
  IR.typeLam = Ext.Ann $ \primTy primVal ->
    [t|(Symbol, Usage.T, IR.Term' T $primTy $primVal)|],
  -- IR.typePi = \primTy primVal ->
  --   [t|(Symbol, Usage.T, Term' T $primTy $primVal)|],
  IR.nameElim = "Elim0",
  IR.typeElim = Ext.Ann $ \primTy primVal ->
    [t|(Usage.T, IR.Term' T $primTy $primVal)|]
}

-- TODO allow extendTerm to reorder fields?
pattern Lam π x s t = Lam0 t (x, π, s)
pattern Elim π s t = Elim0 s (π, t)
{-# COMPLETE Star, PrimTy, Pi, Lam, Elim #-}

IR.extendElim "Elim" [t|T|] $ IR.defaultExtElim {
  IR.typeBound = Ext.Disabled,
  IR.typeFree = Ext.Disabled,
  IR.nameApp = "App0",
  IR.typeApp = Ext.Ann $ \primTy primVal ->
    [t|(Usage.T, IR.Term' T $primTy $primVal,
        Usage.T, IR.Term' T $primTy $primVal)|],
  IR.typeElimX = [("Var", \_primTy _primVal -> [t|Symbol|])]
}

pattern App π s ts ρ t tt = App0 s t (π, ts, ρ, tt)
{-# COMPLETE Var, Prim, App, Ann #-}
