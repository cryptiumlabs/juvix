module Juvix.Core.IRAnn.Types where

import Juvix.Library
import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.Usage

data Ann

instance IR.TEExt Ann where
  type XLam  Ann primTy primVal = Annotation primTy primVal
  type XElim Ann primTy primVal = Annotation primTy primVal
  type XApp  Ann primTy primVal =
    (Annotation primTy primVal, Annotation primTy primVal)

type Annotation primTy primVal = (Usage, Term primTy primVal)


type Term = IR.Term' Ann

pattern Star :: Natural -> Term primTy primVal
pattern Star i = IR.Star' i ()

pattern PrimTy :: primTy -> Term primTy primVal
pattern PrimTy t = IR.PrimTy' t ()

pattern Pi :: Usage -> Term primTy primVal -> Term primTy primVal
           -> Term primTy primVal
pattern Pi π s t = IR.Pi' π s t ()

pattern Lam :: Usage -> Term primTy primVal -> Term primTy primVal
            -> Term primTy primVal
pattern Lam π s t = IR.Lam' t (π, s)

pattern Elim :: Usage -> Term primTy primVal -> Elim primTy primVal
             -> Term primTy primVal
pattern Elim π s e = IR.Elim' e (π, s)

{-# COMPLETE Star, PrimTy, Pi, Lam, Elim #-}


type Elim = IR.Elim' Ann

pattern Bound :: Natural -> Elim primTy primVal
pattern Bound x = IR.Bound' x ()

pattern Free :: IR.Name -> Elim primTy primVal
pattern Free x = IR.Free' x ()

pattern Prim :: primVal -> Elim primTy primVal
pattern Prim x = IR.Prim' x ()

pattern App :: Usage -> Term primTy primVal -> Elim primTy primVal
            -> Usage -> Term primTy primVal -> Term primTy primVal
            -> Elim primTy primVal
pattern App π a s ρ b t = IR.App' s t ((π, a), (ρ, b))

pattern Ann :: Usage -> Term primTy primVal -> Term primTy primVal
            -> Elim primTy primVal
pattern Ann π s t = IR.Ann' π s t ()

{-# COMPLETE Bound, Free, Prim, App, Ann #-}

