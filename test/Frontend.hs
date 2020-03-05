module Frontend where

import Data.Attoparsec.ByteString
import qualified Juvix.Frontend.Lexer as Lexer
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Library

--------------------------------------------------------------------------------
-- Arrow Testing
--------------------------------------------------------------------------------

superArrowCase =
  parse
    Parser.arrowType
    "( b : Bah -> \n c : B -o Foo) -> Foo a b -> #a : Bah a c -o ( HAHAHHA -> foo )"

--------------------------------------------------------------------------------
-- Type tests
--------------------------------------------------------------------------------

-- alias test

typeTest =
  parseOnly Parser.typeP "type Foo a b c d = | Foo Bea"

--------------------------------------------------------------------------------
-- Spacer tests
--------------------------------------------------------------------------------

spacerSymb ∷ Bool
spacerSymb =
  case parse (Parser.spacer Parser.prefixSymbol) "Foo   f" of
    Done f s → f == "f" && s == "Foo"
    _ → False

--------------------------------------------------------------------------------
-- validPrefixSymbols
--------------------------------------------------------------------------------

vpsDashFrontFail ∷ Bool
vpsDashFrontFail =
  isLeft (parseOnly Parser.prefixSymbol "-Foo")

vpsDashMiddle ∷ Bool
vpsDashMiddle =
  isRight (parseOnly Parser.prefixSymbol "Foo-Foo")
