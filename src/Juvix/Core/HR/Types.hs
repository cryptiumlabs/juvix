module Juvix.Core.HR.Types where

import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.Usage
import Juvix.Library


data HR

instance IR.TEExt HR where
  -- FIXME: shouldn't XPi also be a name?
  type XLam HR primTy primVal = Symbol

  -- (disable these constructors)
  type XFree HR primTy primVal = Void
  type XBound HR primTy primVal = Void
  type ElimX HR primTy primVal = Symbol


type Term = IR.Term' HR

pattern Star ∷ Natural → Term primTy primVal
pattern Star i = IR.Star' i ()

pattern PrimTy :: primTy → Term primTy primVal
pattern PrimTy t = IR.PrimTy' t ()

pattern Pi ∷
  Usage → Term primTy primVal → Term primTy primVal → Term primTy primVal
pattern Pi π s t = IR.Pi' π s t ()

pattern Lam ∷ Symbol → Term primTy primVal → Term primTy primVal
pattern Lam x t = IR.Lam' t x

pattern Elim ∷ Elim primTy primVal → Term primTy primVal
pattern Elim e = IR.Elim' e ()

{-# COMPLETE Star, PrimTy, Pi, Lam, Elim #-}


type Elim = IR.Elim' HR

pattern Var ∷ Symbol → Elim primTy primVal
pattern Var x = IR.ElimX x

pattern Prim ∷ primVal → Elim primTy primVal
pattern Prim x = IR.Prim' x ()

pattern App ∷ Elim primTy primVal → Term primTy primVal → Elim primTy primVal
pattern App s t = IR.App' s t ()

pattern Ann ∷
  Usage → Term primTy primVal → Term primTy primVal → Elim primTy primVal
pattern Ann π s t = IR.Ann' π s t ()

{-# COMPLETE Var, Prim, App, Ann #-}
