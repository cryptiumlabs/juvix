{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.Base.Types.Base where

import Extensible (extensible)
import Juvix.Library hiding (Pos)
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Usage (Usage)

type Universe = Natural

type GlobalName = NameSymbol.T

type PatternVar = Int

-- | set of pattern variables
type PatternSet = IntSet

-- | map from pattern variables to e.g. their types
type PatternMap = IntMap

type BoundVar = Natural

data Name
  = -- | Global variables are represented by name thus type string
    Global GlobalName
  | -- | Pattern variable, unique within a scope
    Pattern PatternVar
  deriving (Show, Eq, Generic, Data, NFData)

-- TODO: maybe global functions can have any usage? (for private defs)
data GlobalUsage = GZero | GOmega
  deriving (Show, Eq, Generic, Data, Bounded, Enum, NFData)

extensible
  [d|
    data Term primTy primVal
      = -- | (sort i) i th ordering of (closed) universe.
        Star Universe
      | -- | PrimTy primitive type
        PrimTy primTy
      | -- | primitive constant
        Prim primVal
      | -- | formation rule of the dependent function type PI.
        -- the Usage(π) tracks how many times x is used.
        Pi Usage (Term primTy primVal) (Term primTy primVal)
      | -- | LAM Introduction rule of PI.
        -- The abstracted variables usage is tracked with the Usage(π).
        Lam (Term primTy primVal)
      | -- | Dependent pair (Σ) type, with each half having its own usage
        Sig Usage (Term primTy primVal) (Term primTy primVal)
      | -- | Pair value
        Pair (Term primTy primVal) (Term primTy primVal)
      | -- | Let binder.
        -- the local definition is bound to de Bruijn index 0.
        Let Usage (Elim primTy primVal) (Term primTy primVal)
      | -- | Unit type.
        UnitTy
      | -- | Unit Value
        Unit
      | -- | CONV conversion rule. TODO make sure 0Γ ⊢ S≡T
        -- Elim is the constructor that embeds Elim to Term
        Elim (Elim primTy primVal)
      deriving (Eq, Show, Generic, Data, NFData)

    -- inferable terms
    data Elim primTy primVal
      = -- | Bound variables, in de Bruijn indices
        Bound BoundVar
      | -- | Free variables of type name (see above)
        Free Name
      | -- | elimination rule of PI (APP).
        App (Elim primTy primVal) (Term primTy primVal)
      | -- | Annotation with usage.
        Ann Usage (Term primTy primVal) (Term primTy primVal) Universe
      deriving (Eq, Show, Generic, Data, NFData)

    -- Values/types
    data Value primTy primVal
      = VStar Universe
      | VPrimTy primTy
      | VPi Usage (Value primTy primVal) (Value primTy primVal)
      | VLam (Value primTy primVal)
      | VSig Usage (Value primTy primVal) (Value primTy primVal)
      | VPair (Value primTy primVal) (Value primTy primVal)
      | VUnitTy
      | VUnit
      | VNeutral (Neutral primTy primVal)
      | VPrim primVal
      deriving (Eq, Show, Generic, Data, NFData)

    -- A neutral term is either a variable or an application of a neutral term
    -- to a value
    data Neutral primTy primVal
      = NBound BoundVar
      | NFree Name
      | NApp (Neutral primTy primVal) (Value primTy primVal)
      deriving (Eq, Show, Generic, Data, NFData)

    -- TODO absurd pattern
    data Pattern primTy primVal
      = PCon GlobalName [Pattern primTy primVal]
      | PPair (Pattern primTy primVal) (Pattern primTy primVal)
      | PUnit
      | PVar PatternVar
      | PDot (Term primTy primVal)
      | PPrim primVal
      deriving (Show, Eq, Generic, Data, NFData)
    |]

type CoreShow ext primTy primVal = (
    Show (XStar ext primTy primVal)
  , Show (XPrimTy ext primTy primVal)
  , Show (XPrim ext primTy primVal)
  , Show (XPi ext primTy primVal)
  , Show (XLam ext primTy primVal)
  , Show (XSig ext primTy primVal)
  , Show (XPair ext primTy primVal)
  , Show (XLet ext primTy primVal)
  , Show (XUnitTy ext primTy primVal)
  , Show (XUnit ext primTy primVal)
  , Show (XElim ext primTy primVal)
  , Show (TermX ext primTy primVal)
  , Show (XBound ext primTy primVal)
  , Show (XFree ext primTy primVal)
  , Show (XApp ext primTy primVal)
  , Show (XAnn ext primTy primVal)
  , Show (ElimX ext primTy primVal)
  , Show (XPCon ext primTy primVal)
  , Show (XPPair ext primTy primVal)
  , Show (XPUnit ext primTy primVal)
  , Show (XPVar ext primTy primVal)
  , Show (XPDot ext primTy primVal)
  , Show (XPPrim ext primTy primVal)
  , Show (PatternX ext primTy primVal)
  )

type CoreEq ext primTy primVal = (
    Eq (XStar ext primTy primVal)
  , Eq (XPrimTy ext primTy primVal)
  , Eq (XPrim ext primTy primVal)
  , Eq (XPi ext primTy primVal)
  , Eq (XLam ext primTy primVal)
  , Eq (XSig ext primTy primVal)
  , Eq (XPair ext primTy primVal)
  , Eq (XLet ext primTy primVal)
  , Eq (XUnitTy ext primTy primVal)
  , Eq (XUnit ext primTy primVal)
  , Eq (XElim ext primTy primVal)
  , Eq (TermX ext primTy primVal)
  , Eq (XBound ext primTy primVal)
  , Eq (XFree ext primTy primVal)
  , Eq (XApp ext primTy primVal)
  , Eq (XAnn ext primTy primVal)
  , Eq (ElimX ext primTy primVal)
  , Eq (XPCon ext primTy primVal)
  , Eq (XPPair ext primTy primVal)
  , Eq (XPUnit ext primTy primVal)
  , Eq (XPVar ext primTy primVal)
  , Eq (XPDot ext primTy primVal)
  , Eq (XPPrim ext primTy primVal)
  , Eq (PatternX ext primTy primVal)
  )