{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Types.Base where

import Extensible
import Juvix.Core.Usage
import Juvix.Library
import Prelude (String)

data Name
  = -- | Global variables are represented by name thus type string
    Global String -- FIXME Text???
  | -- | to convert a bound variable into a free one
    Local Natural
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
      deriving (Eq, Show)

    -- | inferable terms
    data Elim primTy primVal
      = -- | Bound variables, in de Bruijn indices
        Bound Natural
      | -- | Free variables of type name (see above)
        Free Name
      | -- | primitive constant
        Prim primVal
      | -- | elimination rule of PI (APP).
        App (Elim primTy primVal) (Term primTy primVal)
      | -- | Annotation with usage.
        Ann Usage (Term primTy primVal) (Term primTy primVal)
      deriving (Eq, Show)
  |]


extensible [d|
  -- | Values/types
  data Value primTy primVal
    = VStar Natural
    | VPrimTy primTy
    | VPi Usage (Value primTy primVal) (Value primTy primVal)
    | VLam (Value primTy primVal)
    | VNeutral (Neutral primTy primVal)
    | VPrim primVal
    deriving (Eq, Show)

  -- | A neutral term is either a variable or an application of a neutral term
  -- to a value
  data Neutral primTy primVal
    = NBound Natural
    | NFree Name
    | NApp (Neutral primTy primVal) (Value primTy primVal)
    deriving (Eq, Show)
  |]
