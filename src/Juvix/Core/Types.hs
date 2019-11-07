module Juvix.Core.Types where

import qualified Juvix.Core.EAC.Types as EAC
import qualified Juvix.Core.Erasure.Types as ET
import Juvix.Library
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

data Parameterisation primTy primVal
  = Parameterisation
      { typeOf ∷ ∀ a. ([primTy] → a) → primVal → Either a primTy,
        apply ∷ primVal → primVal → Maybe primVal,
        parseTy ∷ Token.GenTokenParser String () Identity → Parser primTy,
        parseVal ∷ Token.GenTokenParser String () Identity → Parser primVal,
        reservedNames ∷ [String],
        reservedOpNames ∷ [String]
      }

data PipelineError primTy primVal
  = InternalInconsistencyError Text
  | TypecheckerError Text
  | EACError (EAC.Errors primTy primVal)
  | ErasureError ET.ErasureError
  deriving (Show, Generic)

data PipelineLog primTy primVal
  = LogTodo
  deriving (Show, Generic)
