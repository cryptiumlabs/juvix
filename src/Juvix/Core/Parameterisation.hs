module Juvix.Core.Parameterisation where

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
