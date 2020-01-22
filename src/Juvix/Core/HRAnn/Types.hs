module Juvix.Core.HRAnn.Types where

import Juvix.Library
import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.Usage

data HRAnn

instance IR.TEExt HRAnn where
  type XLam HRAnn primTy primVal = (Symbol, Annotation primTy primVal)
  type XElim HRAnn primTy primVal = Annotation primTy primVal
  type XBound HRAnn primTy primVal = Void
  type XFree HRAnn primTy primVal = Void
  type XApp HRAnn primTy primVal =
    (Annotation primTy primVal, Annotation primTy primVal)
  type ElimX HRAnn primTy primVal = Symbol

type Annotation primTy primVal = (Usage, Term primTy primVal)


type Term = IR.Term' HRAnn

pattern Star :: Natural -> Term primTy primVal
pattern Star i = IR.Star' i ()

pattern PrimTy :: primTy -> Term primTy primVal
pattern PrimTy t = IR.PrimTy' t ()

pattern Pi :: Usage -> Term primTy primVal -> Term primTy primVal
           -> Term primTy primVal
pattern Pi π s t = IR.Pi' π s t ()

pattern Lam :: Symbol -> Usage -> Term primTy primVal -> Term primTy primVal
            -> Term primTy primVal
pattern Lam x π s t = IR.Lam' t (x, (π, s))

pattern Elim :: Usage -> Term primTy primVal -> Elim primTy primVal
             -> Term primTy primVal
pattern Elim π s e = IR.Elim' e (π, s)

{-# COMPLETE Star, PrimTy, Pi, Lam, Elim #-}


type Elim = IR.Elim' HRAnn

pattern Var :: Symbol -> Elim primTy primVal
pattern Var x = IR.ElimX x

pattern Prim :: primVal -> Elim primTy primVal
pattern Prim x = IR.Prim' x ()

pattern App :: Usage -> Term primTy primVal -> Elim primTy primVal
            -> Usage -> Term primTy primVal -> Term primTy primVal
            -> Elim primTy primVal
pattern App π a s ρ b t = IR.App' s t ((π, a), (ρ, b))

pattern Ann :: Usage -> Term primTy primVal -> Term primTy primVal
            -> Elim primTy primVal
pattern Ann π s t = IR.Ann' π s t ()

{-# COMPLETE Var, Prim, App, Ann #-}
