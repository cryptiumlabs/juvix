{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Types.Base where

import Data.Kind (Constraint, Type)
import Extensible
import Juvix.Core.Usage
import Juvix.Library
import Prelude (String)

data Name
  = -- | Global variables are represented by name thus type string
    Global String -- FIXME Text???
  | -- | to convert a bound variable into a free one
    Local Natural
  | Quote Natural
  deriving (Show, Eq)

extensible
  [d|
  data Term primTy primVal
    = -- | (sort i) i th ordering of (closed) universe.
      Star Natural
    | -- | PrimTy primitive type
      PrimTy primTy
    | -- | formation rule of the dependent function type PI.
      -- the Usage(π) tracks how many times x is used.
      Pi Usage (Term primTy primVal) (Term primTy primVal)
    | -- | LAM Introduction rule of PI.
      -- The abstracted variables usage is tracked with the Usage(π).
      Lam (Term primTy primVal)
    | -- | CONV conversion rule. TODO make sure 0Γ ⊢ S≡T
      -- Elim is the constructor that embeds Elim to Term
      Elim (Elim primTy primVal)

  -- | inferable terms
  data Elim primTy primVal
    = -- | Bound variables, in de Bruijn indices
      Bound Natural
    | -- | Free variables of type name (see below)
      Free Name
    | -- | primitive constant
      Prim primVal
    | -- | elimination rule of PI (APP).
      App (Elim primTy primVal) (Term primTy primVal)
    | -- | Annotation with usage.
      Ann Usage (Term primTy primVal) (Term primTy primVal)
  |]

-- FIXME generate these in @extensible-data@

-- | A bundle constraint that requires each annotation type in 'Term' to have an
-- instance of @c@.
type TermAll (c :: Type -> Constraint) ext primTy primVal =
  ( c (XStar ext primTy primVal),
    c (XPrimTy ext primTy primVal),
    c (XPi ext primTy primVal),
    c (XLam ext primTy primVal),
    c (XElim ext primTy primVal),
    c (TermX ext primTy primVal)
  )

-- | A bundle constraint that requires each annotation type in 'Elim' to have an
-- instance of @c@.
type ElimAll (c :: Type -> Constraint) ext primTy primVal =
  ( c (XBound ext primTy primVal),
    c (XFree ext primTy primVal),
    c (XPrim ext primTy primVal),
    c (XApp ext primTy primVal),
    c (XAnn ext primTy primVal),
    c (ElimX ext primTy primVal)
  )

-- FIXME support deriving in @extensible-data@

deriving instance
  ( Eq primTy,
    Eq primVal,
    TermAll Eq ext primTy primVal,
    ElimAll Eq ext primTy primVal
  ) =>
  Eq (Term' ext primTy primVal)

deriving instance
  ( Show primTy,
    Show primVal,
    TermAll Show ext primTy primVal,
    ElimAll Show ext primTy primVal
  ) =>
  Show (Term' ext primTy primVal)

deriving instance
  ( Eq primTy,
    Eq primVal,
    TermAll Eq ext primTy primVal,
    ElimAll Eq ext primTy primVal
  ) =>
  Eq (Elim' ext primTy primVal)

deriving instance
  ( Show primTy,
    Show primVal,
    TermAll Show ext primTy primVal,
    ElimAll Show ext primTy primVal
  ) =>
  Show (Elim' ext primTy primVal)
