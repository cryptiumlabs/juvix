module Juvix.Core.Types where

import qualified Juvix.Core.EAC.Types as EAC
import qualified Juvix.Core.Erased as EC
import qualified Juvix.Core.Erasure.Types as Erasure
import qualified Juvix.Core.HR.Types as HR
import qualified Juvix.Core.IR.Types as IR
import Juvix.Library
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

data Parameterisation primTy primVal
  = Parameterisation
      { -- Returns an arrow.
        typeOf ∷ primVal → NonEmpty primTy,
        apply ∷ primVal → primVal → Maybe primVal,
        parseTy ∷ Token.GenTokenParser String () Identity → Parser primTy,
        parseVal ∷ Token.GenTokenParser String () Identity → Parser primVal,
        reservedNames ∷ [String],
        reservedOpNames ∷ [String]
      }
  deriving (Generic)

arity ∷ ∀ primTy primVal. Parameterisation primTy primVal → primVal → Int
arity param = length . typeOf param

data PipelineError primTy primVal
  = InternalInconsistencyError Text
  | TypecheckerError Text
  | EACError (EAC.Errors primTy primVal)
  | ErasureError Erasure.Error
  deriving (Show, Generic)

data PipelineLog primTy primVal
  = LogHRtoIR (HR.Term primTy primVal) (IR.Term primTy primVal)
  | LogRanZ3 Double
  deriving (Show, Generic)

data TermAssignment primTy primVal
  = Assignment
      { term ∷ EC.Term primVal,
        assignment ∷ EC.TypeAssignment primTy
      }
  deriving (Show, Generic)

data AssignWithType primTy primVal
  = WithType
      { termAssign ∷ TermAssignment primTy primVal,
        type' ∷ EC.Type primTy
      }
  deriving (Show, Generic)
