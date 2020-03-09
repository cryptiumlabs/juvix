module Frontend where

import Data.Attoparsec.ByteString
import qualified Juvix.Frontend.Lexer as Lexer
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Library

--------------------------------------------------------------------------------
-- Type tests
--------------------------------------------------------------------------------

--------------------------------------------------
-- adt testing
--------------------------------------------------

sumTypeTest =
  parseOnly
    Parser.typeP
    ( "type Foo a b c = | A b : a -> b -> c \n"
        <> "            | B d \n"
        <> "            | C { a : Int, #b : Int } \n"
        <> "            | D { a : Int, #b : Int } : Foo Int (Fooy -> Nada)"
    )

--------------------------------------------------
-- Arrow Testing
--------------------------------------------------

superArrowCase =
  parse
    Parser.arrowType
    "( b : Bah -> \n c : B -o Foo) -> Foo a b -> #a : Bah a c -o ( HAHAHHA -> foo )"

--------------------------------------------------
-- alias tests
--------------------------------------------------

typeTest =
  parseOnly Parser.typeP "type Foo a b c d = | Foo Bea"

--------------------------------------------------
-- typeName tests
--------------------------------------------------

typeNameNoUniverse =
  parseOnly Parser.typeNameParser "Foo a b c (b -o d) a c u"

--------------------------------------------------------------------------------
-- Match tests
--------------------------------------------------------------------------------

simpleNamedCon = parseOnly Parser.matchLogic "foo@( Hi a b c )"

matchMoreComplex =
  parseOnly Parser.matchLogic "foo@( Hi nah@{ a = nah , f } b c )"

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

condTest1 =
  parseOnly
    Parser.cond
    $ "if  | foo  = a\n"
      <> " | else = b "

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
