module Juvix.Core.Types
  ( module Juvix.Core.Types,
    module Juvix.Core.Parameterisation,
  )
where

import qualified Juvix.Core.EAC.Types as EAC
import qualified Juvix.Core.Erased as EC
import qualified Juvix.Core.Erasure.Types as Erasure
import qualified Juvix.Core.HR.Types as HR
import qualified Juvix.Core.IR.Typechecker as TC
import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.Parameterisation
import Juvix.Library

data PipelineError primTy primVal compErr
  = InternalInconsistencyError Text
  | TypecheckerError (TC.TypecheckError primTy primVal)
  | EACError (EAC.Errors primTy primVal)
  | ErasureError (Erasure.Error primTy primVal)
  | PrimError compErr
  deriving (Show, Generic)

data PipelineLog primTy primVal
  = LogHRtoIR (HR.Term primTy primVal) (IR.Term primTy primVal)
  | LogRanZ3 Double
  deriving (Show, Generic)

-- compErr serves to resolve the compilation error type
-- needed to promote a backend specific compilation error
data TermAssignment primTy primVal compErr = Assignment
  { term :: EC.Term primVal,
    assignment :: EC.TypeAssignment primTy
  }
  deriving (Show, Generic)

data AssignWithType primTy primVal compErr = WithType
  { termAssign :: TermAssignment primTy primVal compErr,
    type' :: EC.Type primTy
  }
  deriving (Show, Generic)
