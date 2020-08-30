module Juvix.Core.Erased.Types
  ( module Juvix.Core.Erased.Types,
    Term' (..),
    Type' (..),
    TypeAssignment',
  )
where

import qualified Data.HashMap.Strict as HM
import Juvix.Core.Erased.Extend
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
import Juvix.Library hiding (Datatype, Type)

data T

extendTerm "Term" [] [t|T|] (\_ -> defaultExtTerm)

extendType "Type" [] [t|T|] (\_ -> defaultExtType)

type TypeAssignment primTy = TypeAssignment' T primTy

data EvaluationError primVal
  = PrimitiveApplicationError primVal primVal
  deriving (Show, Eq, Generic)
