{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Types.Base where

import Data.Kind (Constraint)
import Extensible
import Juvix.Library
import Juvix.Library.HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Usage

type Universe = Natural

type GlobalName = NameSymbol.T

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

type GlobalAll (c :: * -> Constraint) ext primTy primVal =
  ( c primTy,
    c primVal,
    TermAll c ext primTy primVal,
    ElimAll c ext primTy primVal,
    PatternAll c ext primTy primVal
  )

type GlobalAllWith (c :: * -> Constraint) ty ext primTy primVal =
  ( c (ty primTy primVal),
    c primTy,
    c primVal,
    TermAll c ext primTy primVal,
    ElimAll c ext primTy primVal,
    PatternAll c ext primTy primVal
  )

data DatatypeWith ty primTy primVal
  = Datatype
      { dataName :: GlobalName,
        -- | the type constructor's arguments
        dataArgs :: [DataArgWith ty primTy primVal],
        -- | the type constructor's target universe level
        dataLevel :: Natural,
        dataCons :: [DataConWith ty primTy primVal]
      }
  deriving (Eq, Show, Data, NFData, Generic)

type RawDatatype' ext = DatatypeWith (Term' ext)

type Datatype' extV = DatatypeWith (Value' extV)

data DataArgWith ty primTy primVal
  = DataArg
      { argName :: GlobalName,
        argUsage :: Usage,
        argType :: ty primTy primVal,
        argIsParam :: Bool
      }
  deriving (Eq, Show, Data, NFData, Generic)

type RawDataArg' ext = DataArgWith (Term' ext) ext

type DataArg' extV = DataArgWith (Value' extV)

data DataConWith ty primTy primVal
  = DataCon
      { conName :: GlobalName,
        conType :: ty primTy primVal
      }
  deriving (Eq, Show, Data, NFData, Generic)

type RawDataCon' ext = DataConWith (Term' ext) ext

type DataCon' extV = DataConWith (Value' extV)

data FunctionWith ty ext primTy primVal
  = Function
      { funName :: GlobalName,
        funUsage :: GlobalUsage,
        funType :: ty primTy primVal,
        funClauses :: NonEmpty (FunClause' ext primTy primVal)
      }
  deriving (Generic)

type RawFunction' ext = FunctionWith (Term' ext) ext

type Function' extV = FunctionWith (Value' extV)

deriving instance
  GlobalAllWith Show ty ext primTy primVal =>
  Show (FunctionWith ty ext primTy primVal)

deriving instance
  GlobalAllWith Eq ty ext primTy primVal =>
  Eq (FunctionWith ty ext primTy primVal)

deriving instance
  (Typeable ty, Data ext, GlobalAllWith Data ty ext primTy primVal) =>
  Data (FunctionWith ty ext primTy primVal)

deriving instance
  GlobalAllWith NFData ty ext primTy primVal =>
  NFData (FunctionWith ty ext primTy primVal)

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
  ( Data ext,
    GlobalAll Data ext primTy primVal
  ) =>
  Data (FunClause' ext primTy primVal)

deriving instance
  GlobalAll NFData ext primTy primVal =>
  NFData (FunClause' ext primTy primVal)

data AbstractWith ty ext primTy primVal
  = Abstract
      { absName :: GlobalName,
        absUsage :: GlobalUsage,
        absType :: ty primTy primVal
      }
  deriving (Generic)

type RawAbstract' ext = AbstractWith (Term' ext) ext

type Abstract' extV = AbstractWith (Value' extV)

deriving instance
  GlobalAllWith Show ty ext primTy primVal =>
  Show (AbstractWith ty ext primTy primVal)

deriving instance
  GlobalAllWith Eq ty ext primTy primVal =>
  Eq (AbstractWith ty ext primTy primVal)

deriving instance
  (Typeable ty, Data ext, GlobalAllWith Data ty ext primTy primVal) =>
  Data (AbstractWith ty ext primTy primVal)

deriving instance
  GlobalAllWith NFData ty ext primTy primVal =>
  NFData (AbstractWith ty ext primTy primVal)

data GlobalWith ty ext primTy primVal
  = GDatatype (DatatypeWith ty primTy primVal)
  | GDataCon (DataConWith ty primTy primVal)
  | GFunction (FunctionWith ty ext primTy primVal)
  | GAbstract (AbstractWith ty ext primTy primVal)
  deriving (Generic)

type RawGlobal' ext = GlobalWith (Term' ext) ext

type Global' extV = GlobalWith (Value' extV)

deriving instance
  GlobalAllWith Show ty ext primTy primVal =>
  Show (GlobalWith ty ext primTy primVal)

deriving instance
  GlobalAllWith Eq ty ext primTy primVal =>
  Eq (GlobalWith ty ext primTy primVal)

deriving instance
  (Typeable ty, Data ext, GlobalAllWith Data ty ext primTy primVal) =>
  Data (GlobalWith ty ext primTy primVal)

deriving instance
  GlobalAllWith NFData ty ext primTy primVal =>
  NFData (GlobalWith ty ext primTy primVal)

type GlobalsWith ty ext primTy primVal =
  HashMap GlobalName (GlobalWith ty ext primTy primVal)

type RawGlobals' ext primTy primVal =
  GlobalsWith (Term' ext) ext primTy primVal

type Globals' extV extT primTy primVal =
  GlobalsWith (Value' extV) extT primTy primVal
