module Frontend where

import Data.Attoparsec.ByteString
import qualified Juvix.Frontend.Lexer as Lexer
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Library

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
