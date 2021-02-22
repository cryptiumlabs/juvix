{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Types.Base where

import Data.Kind (Constraint)
import qualified Data.Map as Map
import Extensible (extensible)
import Juvix.Library hiding (Pos)
import Juvix.Library.HashMap (HashMap)
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Usage (Usage)

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
    ValueAll c ext primTy primVal,
    NeutralAll c ext primTy primVal,
    PatternAll c ext primTy primVal
  )

type GlobalAllWith (c :: * -> Constraint) ty ext primTy primVal =
  ( c (ty primTy primVal),
    c primTy,
    c primVal,
    TermAll c ext primTy primVal,
    ElimAll c ext primTy primVal,
    ValueAll c ext primTy primVal,
    NeutralAll c ext primTy primVal,
    PatternAll c ext primTy primVal
  )

data RawDatatype' ext primTy primVal
  = RawDatatype
      { rawDataName :: GlobalName,
        -- | the positivity of its parameters
        rawDataPos :: [Pos],
        -- | the type constructor's arguments
        rawDataArgs :: [RawArgType' ext primTy primVal],
        -- | the type constructor's target universe level
        rawDataLevel :: Natural,
        rawDataCons :: [RawDataCon' ext primTy primVal]
      }
  deriving (Generic)

deriving instance
  GlobalAll Show ext primTy primVal =>
  Show (RawDatatype' ext primTy primVal)

deriving instance
  GlobalAll Eq ext primTy primVal =>
  Eq (RawDatatype' ext primTy primVal)

deriving instance
  (Data ext, GlobalAll Data ext primTy primVal) =>
  Data (RawDatatype' ext primTy primVal)

deriving instance
  GlobalAll NFData ext primTy primVal =>
  NFData (RawDatatype' ext primTy primVal)

data Datatype' ext primTy primVal
  = Datatype
      { dataName :: GlobalName,
        -- | the positivity of its parameters
        dataPos :: [Pos],
        -- | type checked arguments
        dataArgs :: [ArgType' ext primTy primVal],
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

data RawArgType' ext primTy primVal
  = RawDataArg
      { rawArgName :: GlobalName,
        rawArgUsage :: Usage,
        rawArgType :: Term' ext primTy primVal
      }
  deriving (Generic)

data ArgType' ext primTy primVal
  = DataArg
      { argName :: GlobalName,
        argUsage :: Usage,
        argType :: Value' ext primTy primVal
      }
  deriving (Generic)

type RawDataArg' = RawArgType'

type DataArg' = ArgType'

deriving instance
  GlobalAll Show ext primTy primVal =>
  Show (RawArgType' ext primTy primVal)

deriving instance
  GlobalAll Eq ext primTy primVal =>
  Eq (RawArgType' ext primTy primVal)

deriving instance
  (Data ext, GlobalAll Data ext primTy primVal) =>
  Data (RawArgType' ext primTy primVal)

deriving instance
  GlobalAll NFData ext primTy primVal =>
  NFData (RawArgType' ext primTy primVal)

deriving instance
  GlobalAll Show ext primTy primVal =>
  Show (ArgType' ext primTy primVal)

deriving instance
  GlobalAll Eq ext primTy primVal =>
  Eq (ArgType' ext primTy primVal)

deriving instance
  (Data ext, GlobalAll Data ext primTy primVal) =>
  Data (ArgType' ext primTy primVal)

deriving instance
  GlobalAll NFData ext primTy primVal =>
  NFData (ArgType' ext primTy primVal)

data RawDataCon' ext primTy primVal
  = RawDataCon
      { rawConName :: GlobalName,
        rawConType :: Term' ext primTy primVal
      }
  deriving (Generic)

deriving instance
  GlobalAll Show ext primTy primVal =>
  Show (RawDataCon' ext primTy primVal)

deriving instance
  GlobalAll Eq ext primTy primVal =>
  Eq (RawDataCon' ext primTy primVal)

deriving instance
  (Data ext, GlobalAll Data ext primTy primVal) =>
  Data (RawDataCon' ext primTy primVal)

deriving instance
  GlobalAll NFData ext primTy primVal =>
  NFData (RawDataCon' ext primTy primVal)

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

data RawFunction' ext primTy primVal
  = RawFunction
      { rawFunName :: GlobalName,
        rawFunUsage :: GlobalUsage,
        rawFunType :: Term' ext primTy primVal,
        rawFunClauses :: NonEmpty (FunClause' ext primTy primVal)
      }
  deriving (Generic)

deriving instance
  GlobalAll Show ext primTy primVal =>
  Show (RawFunction' ext primTy primVal)

deriving instance
  GlobalAll Eq ext primTy primVal =>
  Eq (RawFunction' ext primTy primVal)

deriving instance
  (Data ext, GlobalAll Data ext primTy primVal) =>
  Data (RawFunction' ext primTy primVal)

deriving instance
  GlobalAll NFData ext primTy primVal =>
  NFData (RawFunction' ext primTy primVal)


--type RawFunction' ext = FunctionWith (Term' ext) ext

--type Function' extV = FunctionWith (Value' extV)

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

data Abstract' ext (primTy :: *) (primVal :: *)
  = Abstract
      { absName :: GlobalName,
        absUsage :: GlobalUsage,
        absType :: Value' ext primTy primVal
      }
  deriving (Generic)

deriving instance
  GlobalAll Show ext primTy primVal =>
  Show (Abstract' ext primTy primVal)

deriving instance
  GlobalAll Eq ext primTy primVal =>
  Eq (Abstract' ext primTy primVal)

deriving instance
  (Data ext, GlobalAll Data ext primTy primVal) =>
  Data (Abstract' ext primTy primVal)

deriving instance
  GlobalAll NFData ext primTy primVal =>
  NFData (Abstract' ext primTy primVal)


--deriving instance
--  Show (primTy primVal) =>
--  Show (Abstract' primTy primVal)
--
--deriving instance
--  Eq (primTy primVal) =>
--  Eq (Abstract' primTy primVal)
--
--deriving instance
--  (Typeable primTy, Typeable primVal, Data (primTy primVal)) =>
--  Data (Abstract' primTy primVal)
--
--deriving instance
--  NFData (primTy primVal) =>
--  NFData (Abstract' primTy primVal)
--

data RawAbstract' ext (primTy :: *) (primVal :: *)
  = RawAbstract
      { rawAbsName :: GlobalName,
        rawAbsUsage :: GlobalUsage,
        rawAbsType :: Term' ext primTy primVal
      }
  deriving (Generic)


-- type RawAbstract' ext = AbstractWith (Term' ext)

-- type Abstract' extV = AbstractWith (Value' extV)
deriving instance
  GlobalAll Show ext primTy primVal =>
  Show (RawAbstract' ext primTy primVal)

deriving instance
  GlobalAll Eq ext primTy primVal =>
  Eq (RawAbstract' ext primTy primVal)

deriving instance
  (Data ext, GlobalAll Data ext primTy primVal) =>
  Data (RawAbstract' ext primTy primVal)

deriving instance
  GlobalAll NFData ext primTy primVal =>
  NFData (RawAbstract' ext primTy primVal)


--deriving instance
--  Show (primTy primVal) =>
--  Show (RawAbstract' primTy primVal)
--
--deriving instance
--  Eq (primTy primVal) =>
--  Eq (RawAbstract' primTy primVal)
--
--deriving instance
--  (Typeable primTy, Typeable primVal, Data (primTy primVal)) =>
--  Data (RawAbstract' primTy primVal)
--
--deriving instance
--  NFData (primTy primVal) =>
--  NFData (RawAbstract' ext primTy primVal)
--
data Global' ext primTy primVal
  = GDatatype (Datatype' ext primTy primVal)
  | GDataCon (DataCon' ext primTy primVal)
  | GFunction (Function' ext primTy primVal)
  | GAbstract (Abstract' ext primTy primVal)
  deriving (Generic)

deriving instance
  GlobalAll Show ext primTy primVal =>
  Show (Global' ext primTy primVal)

deriving instance
  GlobalAll Eq ext primTy primVal =>
  Eq (Global' ext primTy primVal)

deriving instance
  (Data ext, GlobalAll Data ext primTy primVal) =>
  Data (Global' ext primTy primVal)

deriving instance
  GlobalAll NFData ext primTy primVal =>
  NFData (Global' ext primTy primVal)


data RawGlobal' ext primTy primVal
  = RawGDatatype (RawDatatype' ext primTy primVal)
  | RawGDataCon (RawDataCon' ext primTy primVal)
  | RawGFunction (RawFunction' ext primTy primVal)
  | RawGAbstract (RawAbstract' ext primTy primVal)

deriving instance
  GlobalAll Show ext primTy primVal =>
  Show (RawGlobal' ext primTy primVal)

deriving instance
  GlobalAll Eq ext primTy primVal =>
  Eq (RawGlobal' ext primTy primVal)

deriving instance
  (Data ext, GlobalAll Data ext primTy primVal) =>
  Data (RawGlobal' ext primTy primVal)

deriving instance
  GlobalAll NFData ext primTy primVal =>
  NFData (RawGlobal' ext primTy primVal)


--type RawGlobal' ext = GlobalWith (Term' ext) ext

--type Global' extV = GlobalWith (Value' extV)

type Globals' ext primTy primVal =
  HashMap GlobalName (Global' ext primTy primVal)

type RawGlobals' ext primTy primVal =
  HashMap GlobalName (RawGlobal' ext primTy primVal)

--type RawGlobals' ext primTy primVal =
--  GlobalsWith (Term' ext) ext primTy primVal

--type Globals' ext primTy primVal =
--  GlobalsWith (Value' ext) ext primTy primVal

type Telescope ext primTy primVal =
  [(Name, Term' ext primTy primVal)]

data FunClause' ext primTy primVal
  = -- | Clause has been labelled as unreachable by the coverage checker.
    --   @Nothing@ means coverage checker has not run yet (clause may be unreachable).
    --   @Just False@ means clause is not unreachable.
    --   @Just True@ means clause is unreachable.
    FunClause
      { -- | @Δ@: The types of the pattern variables in dependency order.
        -- , namedClausePats :: NAPs (Using Name instead atm)
        -- ^ @Δ ⊢ ps@.  The de Bruijn indices refer to @Δ@.
        clauseTel :: Telescope ext primTy primVal,
        namedClausePats :: [Pattern' ext primTy primVal], --TODO [SplitPattern]
            -- TODO make it a Maybe
            -- @Just v@ for a regular clause, @Nothing@ for an absurd one.
        clauseBody :: Term' ext primTy primVal,
        -- | @Δ ⊢ t@.  The type of the rhs under @clauseTel@.
        clauseType :: Maybe (Value' ext primTy primVal),
        -- | Clause has been labelled as CATCHALL.
        -- , clauseRecursive   :: Maybe Bool TODO add this when termination checking
        -- ^ @clauseBody@ contains recursive calls; computed by termination checker.
        --   @Nothing@ means that termination checker has not run yet,
        --   or that @clauseBody@ contains meta-variables;
        --   these could be filled with recursive calls later!
        --   @Just False@ means definitely no recursive call.
        --   @Just True@ means definitely a recursive call.
        clauseCatchall :: Bool,
        clauseUnreachable :: Maybe Bool
      }
  deriving (Generic)

type Signature ty ext primTy primVal = Map.Map GlobalName (SigDef ty ext primTy primVal)

-- Return type of all type-checking functions.
-- state monad for global signature
type TypeCheck ty ext primTy primVal a = StateT (Signature ty ext primTy primVal) IO a

data SigDef ty ext primTy primVal
  = -- function constant to its type, clauses, whether it's type checked
    FunSig (Value' ext primTy primVal) [NonEmpty (FunClause' ext primTy primVal)] Bool
  | ConSig (Value' ext primTy primVal) -- constructor constant to its type
        -- data type constant to # parameters, positivity of parameters, type
  | DataSig Int [Pos] (Value' ext primTy primVal)

data Pos -- positivity
  = SPos
  | NSPos
  deriving (Generic, Eq, Show, Data, NFData)
