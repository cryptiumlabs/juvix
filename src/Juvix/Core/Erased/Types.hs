module Juvix.Core.Erased.Types
  ( module Juvix.Core.Erased.Types,
    Term' (..),
    Type' (..),
    TypeAssignment',
    NoExt,
  )
  where

import Juvix.Library
import Juvix.Core.Erased.Types.Base
import Juvix.Core.IR.Types (NoExt)

extendTerm "Term" [] [t|NoExt|] $ \_ -> defaultExtTerm

extendType "Type" [] [t|NoExt|] $ \_ -> defaultExtType

type TypeAssignment primTy = TypeAssignment' NoExt primTy

data EvaluationError primVal
  = PrimitiveApplicationError primVal primVal
  deriving (Show, Eq, Generic)
