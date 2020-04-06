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


-- the kind signatures on @m@ are needed for e.g.
-- @type family VStarX primTy primVal (m ∷ Type → Type)@
-- otherwise it defaults to @Type@ and breaks

extensible [d|
  -- | Values/types
  data Value primTy primVal (m ∷ Type → Type)
    = VStar Natural
    | VPrimTy primTy
    | VPi
        Usage
        (Value primTy primVal m)
        (Value primTy primVal m → m (Value primTy primVal m))
    | VLam (Value primTy primVal m → m (Value primTy primVal m))
    | VNeutral (Neutral primTy primVal m)
    | VPrim primVal

  -- | A neutral term is either a variable or an application of a neutral term to a value
  data Neutral primTy primVal (m ∷ Type → Type)
    = NFree Name
    | NApp (Neutral primTy primVal m) (Value primTy primVal m)
  |]
