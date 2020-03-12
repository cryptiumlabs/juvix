module Frontend where

import Data.Attoparsec.ByteString
import qualified Juvix.Frontend.Lexer as Lexer
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Library

--------------------------------------------------------------------------------
-- Parse Many at once
--------------------------------------------------------------------------------

many1FunctionsParser =
  parseOnly
    (many Parser.topLevelSN)
    ( "let foo a b c = (+) (a + b) c\n"
        <> "let bah = foo 1 2 3\n"
        <> "let nah \n"
        <> "  | bah == 5 = 7 \n"
        <> "  | else     = 11"
        <> "let test = \n"
        <> "  let check = nah in \n"
        <> "  case check of \n"
        <> "  | seven -> 11 \n"
        <> "  | eleven -> 7 \n"
        <> "  | f  -> open Fails in \n"
        <> "          print failed; \n"
        <> "          fail"
    )

--------------------------------------------------------------------------------
-- Sig Test
--------------------------------------------------------------------------------

sigTest1 = parseOnly Parser.topLevel "sig foo 0 : Int -> Int"

sigTest2 = parseOnly Parser.topLevel "sig foo 0 : i : Int{i > 0} -> Int{i > 1}"

--------------------------------------------------------------------------------
-- Function Testing
--------------------------------------------------------------------------------

fun1 = parseOnly Parser.topLevel "let f foo@(A b c d) = 3"

fun2 = parseOnly Parser.topLevel "let f foo | foo = 2 | else = 3"
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

--------------------------------------------------------------------------------
-- Modules test
--------------------------------------------------------------------------------

moduleOpen =
  parseOnly
    Parser.topLevel
    ( ""
        <> "let Foo Int = \n"
        <> "  type T = Int.t \n"
        <> "  sig bah : T -> T \n"
        <> "  let bah t = Int.(t + 3) \n"
        <> "end"
    )

moduleOpen' =
  parseOnly
    Parser.topLevel
    ( ""
        <> "let Bah M = \n"
        <> "  open M"
        <> "  sig bah : Rec \n"
        <> "  let bah t = \n"
        <> "     { a = (t + 3)"
        <> "     , b = expr M.N.t}"
        <> "end"
    )

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
  parseOnly Parser.matchLogic "foo@( Hi nah@{ a = nah , f } b 5 )"

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

condTest1 =
  parseOnly
    Parser.cond
    $ "if  | foo  = a\n"
      <> " | else = b "

--------------------------------------------------
-- Record
--------------------------------------------------

record1 = parseOnly Parser.expression "{a, b = 3+5}"

--------------------------------------------------
-- parens
--------------------------------------------------

parens1 = parseOnly Parser.expression "(       ( \n(({a, b = 3+5})))\n)"

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
