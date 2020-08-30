module Juvix.Core.Erased.Types
  ( module Juvix.Core.Erased.Types,
    Term' (..),
    Type' (..),
    TypeAssignment',
  )
where

import Juvix.Core.Erased.Extend
import qualified Data.HashMap.Strict as HM
import Juvix.Core.Erased.Types.Base
import qualified Juvix.Core.IR.Types.Base as IR
import Juvix.Core.IR.Types.Base hiding
  ( Term' (..),
    defaultExtTerm,
    extDataArg,
    extDataCon,
    extDatatype,
    extFunction,
    extendTerm,
  )
import Juvix.Core.Usage (Usage)
import Juvix.Library hiding (Type, Datatype)

data T

extendTerm "Term" [] [t|T|] (\_ -> defaultExtTerm)

extendType "Type" [] [t|T|] (\_ -> defaultExtType)

-- TODO: Figure out how to do this with extensible.
-- IR.extendDatatype "Datatype" [] [t|T|] extDatatype

data Datatype primTy
  = Datatype
      { dataName :: GlobalName,
        dataArgs :: [DataArg primTy],
        dataLevel :: Natural,
        dataCons :: [DataCon primTy]
      }
  deriving (Show, Eq, Generic)

-- TODO: Figure out how to do this with extensible.
-- IR.extendDataArg "DataArg" [] [t|T|] extDataArg

data DataArg primTy
  = DataArg
      { argName :: GlobalName,
        argUsage :: Usage,
        argType :: Type primTy,
        argIsParam :: Bool
      }
  deriving (Show, Eq, Generic)

-- TODO: Figure out how to do this with extensible.
-- IR.extendDataCon "DataCon" [] [t|T|] extDataCon

data DataCon primTy
  = DataCon
      { conName :: GlobalName,
        conType :: Type primTy
      }
  deriving (Show, Eq, Generic)

-- TODO: Figure out how to do this with extensible.
-- IR.extendFunction "Function" [] [t|T|] extFunction

data Function primTy primVal
  = Function
      { funName :: GlobalName,
        funUsage :: GlobalUsage,
        funType :: Type primTy,
        funClauses :: NonEmpty (FunClause primVal)
      }
  deriving (Show, Eq, Generic)

-- TODO: Figure out how to do this with extensible.
-- IR.extendFunClause "FunClause" [] [t|T|] extFunClause

data FunClause primVal
  = FunClause [Pattern primVal] (Term primVal)
  deriving (Show, Eq, Generic)

-- TODO: Figure out how to do this with extensible.
-- IR.extendPattern "Pattern" [] [t|T|] extPattern

data Pattern primVal
  = PCon GlobalName [Pattern primVal]
  | PVar PatternVar
  | PDot (Term primVal)
  | PPrim primVal
  deriving (Show, Eq, Generic)

data Global primTy primVal
  = GDatatype (Datatype primTy)
  | GDataCon (DataCon primTy)
  | GFunction (Function primTy primVal)
  | GAbstract GlobalUsage (Term primVal)
  deriving (Show, Eq, Generic)

type Globals primTy primVal = HM.HashMap GlobalName (Global primTy primVal)

type TypeAssignment primTy = TypeAssignment' T primTy

data EvaluationError primVal
  = PrimitiveApplicationError primVal primVal
  deriving (Show, Eq, Generic)
