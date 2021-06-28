module Juvix.Core.Erased.Types
  ( module Juvix.Core.Erased.Types,
    Term' (..),
    Type' (..),
    TypeAssignment',
  )
where

-- TODO shouldn't this module be using these?
-- import Juvix.Core.Erased.Extend
import Juvix.Core.Erased.Types.Base
import qualified Juvix.Core.IR.Typechecker.Types as TC
import qualified Juvix.Core.IR.Types.Globals as IR
import Juvix.Library hiding (Datatype, Type)

data T

extendTerm "Term" [] [t|T|] (\_ -> defaultExtTerm)

type TermT primTy primVal = Term (TC.TypedPrim primTy primVal)

extendType "Type" [] [t|T|] (\_ -> defaultExtType)

type Datatype = IR.Datatype' T T

type DataArg = IR.DataArg' T

type DataCon = IR.DataCon' T T

type Function = IR.Function' T

type FunClause primTy primVal = IR.FunClause' T primTy primVal

type TypeAssignment primTy = TypeAssignment' T primTy

data EvaluationError primVal
  = PrimitiveApplicationError primVal primVal
  deriving (Show, Eq, Generic)
