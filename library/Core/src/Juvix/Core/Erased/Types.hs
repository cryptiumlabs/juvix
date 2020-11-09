module Juvix.Core.Erased.Types
  ( module Juvix.Core.Erased.Types,
    Term' (..),
    Type' (..),
    TypeAssignment',
  )
where

import Juvix.Core.Erased.Extend
import Juvix.Core.Erased.Types.Base
import Juvix.Core.IR.Types.Base hiding
  ( Term' (..),
    defaultExtTerm,
    extendTerm,
  )
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.IR.Typechecker.Types as TC
import Juvix.Library hiding (Datatype, Type)
import Juvix.Library.Usage (Usage)

data T

extendTerm "Term" [] [t|T|] (\_ -> defaultExtTerm)

type TermT primTy primVal = Term (TC.TypedPrim primTy primVal)

extendType "Type" [] [t|T|] (\_ -> defaultExtType)

type Datatype = Datatype' T

type DataArg = DataArg' T

type DataCon = DataCon' T

type Function = Function' T

type FunClause = FunClause' T

type TypeAssignment primTy = TypeAssignment' T primTy

data EvaluationError primVal
  = PrimitiveApplicationError primVal primVal
  deriving (Show, Eq, Generic)
