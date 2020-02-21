module Juvix.Core.HRAnn.Types where

import Juvix.Library
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Usage as Usage

data T

instance IR.TEExt T where
  type XLam T primTy primVal = (Symbol, Annotation primTy primVal)
  type XElim T primTy primVal = Annotation primTy primVal
  type XBound T primTy primVal = Void
  type XFree T primTy primVal = Void
  type XApp T primTy primVal =
    (Annotation primTy primVal, Annotation primTy primVal)
  type ElimX T primTy primVal = Symbol

type Annotation primTy primVal = (Usage.T, Term primTy primVal)


type Term = IR.Term' T

pattern Star ∷ Natural → Term primTy primVal
pattern Star i = IR.Star' i ()

pattern PrimTy ∷ primTy → Term primTy primVal
pattern PrimTy t = IR.PrimTy' t ()

pattern Pi ::
  Usage.T → Term primTy primVal → Term primTy primVal → Term primTy primVal
pattern Pi π s t = IR.Pi' π s t ()

pattern Lam ∷
  Symbol → Usage.T → Term ty primVal → Term ty primVal → Term ty primVal
pattern Lam x π s t = IR.Lam' t (x, (π, s))

pattern Elim ::
  Usage.T → Term primTy primVal → Elim primTy primVal → Term primTy primVal
pattern Elim π s e = IR.Elim' e (π, s)

{-# COMPLETE Star, PrimTy, Pi, Lam, Elim #-}


type Elim = IR.Elim' T

pattern Var :: Symbol -> Elim primTy primVal
pattern Var x = IR.ElimX x

pattern Prim :: primVal -> Elim primTy primVal
pattern Prim x = IR.Prim' x ()

pattern App ∷ Usage.T → Term primTy primVal → Elim primTy primVal
            → Usage.T → Term primTy primVal → Term primTy primVal
            → Elim primTy primVal
pattern App π a s ρ b t = IR.App' s t ((π, a), (ρ, b))

pattern Ann ∷
  Usage.T → Term primTy primVal → Term primTy primVal → Elim primTy primVal
pattern Ann π s t = IR.Ann' π s t ()

{-# COMPLETE Var, Prim, App, Ann #-}
