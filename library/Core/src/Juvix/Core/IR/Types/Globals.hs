{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Types.Globals where

import Juvix.Core.IR.Types.Base
import Data.Kind (Constraint)
import qualified Data.Map as Map
import Juvix.Library hiding (Pos)
import Juvix.Library.HashMap (HashMap)
import Juvix.Library.Usage (Usage)


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
        rawDataArgs :: [RawDataArg' ext primTy primVal],
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

data RawDataArg' ext primTy primVal
  = RawDataArg
      { rawArgName :: GlobalName,
        rawArgUsage :: Usage,
        rawArgType :: Term' ext primTy primVal
      }
  deriving (Generic)

data DataArg' ext primTy primVal
  = DataArg
      { argName :: GlobalName,
        argUsage :: Usage,
        argType :: Value' ext primTy primVal
      }
  deriving (Generic)

deriving instance
  GlobalAll Show ext primTy primVal =>
  Show (RawDataArg' ext primTy primVal)

deriving instance
  GlobalAll Eq ext primTy primVal =>
  Eq (RawDataArg' ext primTy primVal)

deriving instance
  (Data ext, GlobalAll Data ext primTy primVal) =>
  Data (RawDataArg' ext primTy primVal)

deriving instance
  GlobalAll NFData ext primTy primVal =>
  NFData (RawDataArg' ext primTy primVal)

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

data AbstractWith ty (primTy :: *) (primVal :: *)
  = Abstract
      { absName :: GlobalName,
        absUsage :: GlobalUsage,
        absType :: ty primTy primVal
      }
  deriving (Generic)

type RawAbstract' ext = AbstractWith (Term' ext)

type Abstract' extV = AbstractWith (Value' extV)

deriving instance
  Show (ty primTy primVal) =>
  Show (AbstractWith ty primTy primVal)

deriving instance
  Eq (ty primTy primVal) =>
  Eq (AbstractWith ty primTy primVal)

deriving instance
  (Typeable ty, Typeable primTy, Typeable primVal, Data (ty primTy primVal)) =>
  Data (AbstractWith ty primTy primVal)

deriving instance
  NFData (ty primTy primVal) =>
  NFData (AbstractWith ty primTy primVal)

data GlobalWith ty ext primTy primVal
  = GDatatype (Datatype' ext primTy primVal)
  | RawGDatatype (RawDatatype' ext primTy primVal)
  | GDataCon (DataCon' ext primTy primVal)
  | RawGDataCon (RawDataCon' ext primTy primVal)
  | GFunction (FunctionWith ty ext primTy primVal)
  | GAbstract (AbstractWith ty primTy primVal)
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

type Globals' ext primTy primVal =
  GlobalsWith (Value' ext) ext primTy primVal

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
-- TODO move this somewhere
type TypeCheck ty ext primTy primVal a =
    StateT (Signature ty ext primTy primVal) IO a

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
