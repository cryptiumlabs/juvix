{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Types.Base where

import Data.Kind (Constraint)
import Extensible
import Juvix.Core.Usage
import Juvix.Library
import Juvix.Library.HashMap

type Universe = Natural

type GlobalName = Symbol

type PatternVar = Int

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
      | -- | Let binder.
        -- the local definition is bound to de Bruijn index 0.
        Let Usage (Elim primTy primVal) (Term primTy primVal)
      | -- | CONV conversion rule. TODO make sure 0Γ ⊢ S≡T
        -- Elim is the constructor that embeds Elim to Term
        Elim (Elim primTy primVal)
      deriving (Eq, Show, Generic, Data, NFData)

    -- | inferable terms
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

    -- | Values/types
    data Value primTy primVal
      = VStar Universe
      | VPrimTy primTy
      | VPi Usage (Value primTy primVal) (Value primTy primVal)
      | VLam (Value primTy primVal)
      | VNeutral (Neutral primTy primVal)
      | VPrim primVal
      deriving (Eq, Show, Generic, Data, NFData)

    -- | A neutral term is either a variable or an application of a neutral term
    -- to a value
    data Neutral primTy primVal
      = NBound BoundVar
      | NFree Name
      | NApp (Neutral primTy primVal) (Value primTy primVal)
      deriving (Eq, Show, Generic, Data, NFData)

    -- TODO absurd pattern
    data Pattern primTy primVal
      = PCon GlobalName [Pattern primTy primVal]
      | PVar PatternVar
      | PDot (Term primTy primVal)
      | PPrim primVal
      deriving (Show, Eq, Generic, Data, NFData)
    |]

type GlobalAll (c :: * -> Constraint) ext primTy primVal =
  ( c primTy,
    c primVal,
    TermAll c ext primTy primVal,
    ElimAll c ext primTy primVal,
    ValueAll c ext primTy primVal,
    NeutralAll c ext primTy primVal,
    PatternAll c ext primTy primVal
  )

data Datatype' ext primTy primVal
  = Datatype
      { dataName :: GlobalName,
        -- | the type constructor's arguments
        dataArgs :: [DataArg' ext primTy primVal],
        -- | the type constructor's target universe level
        dataLevel :: Natural,
        dataCons :: [DataCon' ext primTy primVal]
      }
  deriving (Generic)

deriving instance
  GlobalAll Show ext primTy primVal =>
  Show (Datatype' ext primTy primVal)

deriving instance
  GlobalAll Eq ext primTy primVal =>
  Eq (Datatype' ext primTy primVal)

deriving instance
  (Data ext, GlobalAll Data ext primTy primVal) =>
  Data (Datatype' ext primTy primVal)

deriving instance
  GlobalAll NFData ext primTy primVal =>
  NFData (Datatype' ext primTy primVal)

data DataArg' ext primTy primVal
  = DataArg
      { argName :: GlobalName,
        argUsage :: Usage,
        argType :: Value' ext primTy primVal,
        argIsParam :: Bool
      }
  deriving (Generic)

deriving instance
  GlobalAll Show ext primTy primVal =>
  Show (DataArg' ext primTy primVal)

deriving instance
  GlobalAll Eq ext primTy primVal =>
  Eq (DataArg' ext primTy primVal)

deriving instance
  (Data ext, GlobalAll Data ext primTy primVal) =>
  Data (DataArg' ext primTy primVal)

deriving instance
  GlobalAll NFData ext primTy primVal =>
  NFData (DataArg' ext primTy primVal)

data DataCon' ext primTy primVal
  = DataCon
      { conName :: GlobalName,
        conType :: Value' ext primTy primVal
      }
  deriving (Generic)

deriving instance
  GlobalAll Show ext primTy primVal =>
  Show (DataCon' ext primTy primVal)

deriving instance
  GlobalAll Eq ext primTy primVal =>
  Eq (DataCon' ext primTy primVal)

deriving instance
  (Data ext, GlobalAll Data ext primTy primVal) =>
  Data (DataCon' ext primTy primVal)

deriving instance
  GlobalAll NFData ext primTy primVal =>
  NFData (DataCon' ext primTy primVal)

data Function' ext primTy primVal
  = Function
      { funName :: GlobalName,
        funUsage :: GlobalUsage,
        funType :: Value' ext primTy primVal,
        funClauses :: NonEmpty (FunClause' ext primTy primVal)
      }
  deriving (Generic)

deriving instance
  GlobalAll Show ext primTy primVal =>
  Show (Function' ext primTy primVal)

deriving instance
  GlobalAll Eq ext primTy primVal =>
  Eq (Function' ext primTy primVal)

deriving instance
  (Data ext, GlobalAll Data ext primTy primVal) =>
  Data (Function' ext primTy primVal)

deriving instance
  GlobalAll NFData ext primTy primVal =>
  NFData (Function' ext primTy primVal)

data FunClause' ext primTy primVal
  = FunClause [Pattern' ext primTy primVal] (Term' ext primTy primVal)
  deriving (Generic)

deriving instance
  GlobalAll Show ext primTy primVal =>
  Show (FunClause' ext primTy primVal)

deriving instance
  GlobalAll Eq ext primTy primVal =>
  Eq (FunClause' ext primTy primVal)

deriving instance
  (Data ext, GlobalAll Data ext primTy primVal) =>
  Data (FunClause' ext primTy primVal)

deriving instance
  GlobalAll NFData ext primTy primVal =>
  NFData (FunClause' ext primTy primVal)

data Global' ext primTy primVal
  = GDatatype (Datatype' ext primTy primVal)
  | GDataCon (DataCon' ext primTy primVal)
  | GFunction (Function' ext primTy primVal)
  | GAbstract GlobalUsage (Value' ext primTy primVal)
  deriving (Generic)

deriving instance
  GlobalAll Eq ext primTy primVal =>
  Eq (Global' ext primTy primVal)

deriving instance
  GlobalAll Show ext primTy primVal =>
  Show (Global' ext primTy primVal)

deriving instance
  (Data ext, GlobalAll Data ext primTy primVal) =>
  Data (Global' ext primTy primVal)

deriving instance
  GlobalAll NFData ext primTy primVal =>
  NFData (Global' ext primTy primVal)

type Globals' ext primTy primVal =
  HashMap GlobalName (Global' ext primTy primVal)
