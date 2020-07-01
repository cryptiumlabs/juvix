{-# LANGUAGE UndecidableInstances #-}
module Juvix.Core.IR.Types.Base where

import Extensible
import Juvix.Core.Usage
import Juvix.Library

type Universe = Natural

type GlobalName = Symbol

type PatternVar = Int

type BoundVar = Natural

data Name
  = -- | Global variables are represented by name thus type string
    Global GlobalName
  | -- | Pattern variable, unique within a scope
    Pattern PatternVar
  deriving (Show, Eq)

data GlobalUsage = GZero | GOmega
  deriving (Show, Eq, Generic, Bounded, Enum)

extensible
  [d|
    data Term primTy primVal
      = -- | (sort i) i th ordering of (closed) universe.
        Star Universe
      | -- | PrimTy primitive type
        PrimTy primTy
      | -- | formation rule of the dependent function type PI.
        -- the Usage(π) tracks how many times x is used.
        Pi Usage (Term primTy primVal) (Term primTy primVal)
      | -- | LAM Introduction rule of PI.
        -- The abstracted variables usage is tracked with the Usage(π).
        Lam (Term primTy primVal)
      | -- | Let binder.
        -- the local definition is bound to de Bruijn index 0.
        Let Usage (Elim primTy primVal) (Term primTy primVal)
      | -- | CONV conversion rule. TODO make sure 0Γ ⊢ S≡T
        -- Elim is the constructor that embeds Elim to Term
        Elim (Elim primTy primVal)
      deriving (Eq, Show)

    -- | inferable terms
    data Elim primTy primVal
      = -- | Bound variables, in de Bruijn indices
        Bound BoundVar
      | -- | Free variables of type name (see above)
        Free Name
      | -- | primitive constant
        Prim primVal
      | -- | elimination rule of PI (APP).
        App (Elim primTy primVal) (Term primTy primVal)
      | -- | Annotation with usage.
        Ann Usage (Term primTy primVal) (Term primTy primVal) Universe
      deriving (Eq, Show)

    -- | Values/types
    data Value primTy primVal
      = VStar Universe
      | VPrimTy primTy
      | VPi Usage (Value primTy primVal) (Value primTy primVal)
      | VLam (Value primTy primVal)
      | VNeutral (Neutral primTy primVal)
      | VPrim primVal
      deriving (Eq, Show)

    -- | A neutral term is either a variable or an application of a neutral term
    -- to a value
    data Neutral primTy primVal
      = NBound BoundVar
      | NFree Name
      | NApp (Neutral primTy primVal) (Value primTy primVal)
      deriving (Eq, Show)

    data Datatype primTy primVal
      = Datatype
          { dataName :: GlobalName,
            -- | the type constructor's arguments
            dataArgs :: [DataArg primTy primVal],
            -- | the type constructor's target universe level
            dataLevel :: Natural,
            dataCons :: [DataCon primTy primVal]
          }
      deriving (Show, Eq, Generic)

    data DataArg primTy primVal
      = DataArg
          { argName :: GlobalName,
            argUsage :: Usage,
            argType :: Value primTy primVal,
            argIsParam :: Bool
          }
      deriving (Show, Eq, Generic)

    data DataCon primTy primVal
      = DataCon
          { conName :: GlobalName,
            conType :: Value primTy primVal
          }
      deriving (Show, Eq, Generic)

    data Function primTy primVal
      = Function
          { funName :: GlobalName,
            funUsage :: GlobalUsage,
            funType :: Value primTy primVal,
            funClauses :: NonEmpty (FunClause primTy primVal)
          }
      deriving (Show, Eq, Generic)

    data FunClause primTy primVal
      = FunClause [Pattern primTy primVal] (Term primTy primVal)
      deriving (Show, Eq, Generic)

    data Pattern primTy primVal
      = PCon GlobalName [Pattern primTy primVal]
      | PVar PatternVar
      | PDot (Term primTy primVal)
      | PPrim primVal
      deriving (Show, Eq, Generic)
    |]
