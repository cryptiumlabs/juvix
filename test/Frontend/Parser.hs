module Frontend.Parser where

import Data.Attoparsec.ByteString
  ( IResult (Done, Fail, Partial),
    Parser,
    Result,
    many',
    parse,
    parseOnly,
  )
import qualified Data.Attoparsec.ByteString.Char8 as Char8
import qualified Juvix.Core.Common.NameSymbol as NameSym
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Frontend.Types (Expression, TopLevel)
import qualified Juvix.Frontend.Types as AST
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (String, error)

allParserTests :: T.TestTree
allParserTests =
  T.testGroup
    "Parser Tests"
    [ many1FunctionsParser,
      sigTest1,
      sigTest2,
      fun1,
      fun2,
      sumTypeTest,
      superArrowCase,
      typeTest,
      moduleOpen,
      moduleOpen',
      typeNameNoUniverse,
      simpleNamedCon,
      matchMoreComplex,
      condTest1,
      record1,
      parens1,
      -- pre-processor tests
      removeNoComment,
      removeNewLineBefore,
      removeSpaceBefore,
      nonassocTest,
      infxlTest,
      infxrTest,
      infixFail,
      spacerSymb,
      vpsDashFrontFail,
      vpsDashMiddle,
      infxPlusTest,
      infixPlusFail,
      reservedInfix,
      letwordFail,
      reservedInfix,
      caseOfWords,
      questionMarktest,
      bangtest
    ]

--------------------------------------------------------------------------------
-- Parser Checker
--------------------------------------------------------------------------------

space :: Parser Word8
space = Char8.char8 ' '

test :: Either String [Expression]
test =
  parseOnly
    (many' Parser.expressionSN)
    "let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo \
    \= 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let \
    \foo = 3 let foo = 3 let foo = 3 let foo = 3 "

takeResult :: Result a -> a
takeResult (Done _ x) = x
takeResult (Partial y) = takeResult (y "")
takeResult (Fail _ _ errorMsg) = error errorMsg

shouldParseAs ::
  (Show a, Eq a) => T.TestName -> (ByteString -> Result a) -> ByteString -> a -> T.TestTree
shouldParseAs name parses x y =
  T.testGroup
    "Parse tests"
    [ T.testCase
        ("parse: " <> name <> " " <> show x <> " should parse to " <> show y)
        (takeResult (parses x) T.@=? y)
    ]

--------------------------------------------------------------------------------
-- Pre-processor test
--------------------------------------------------------------------------------
removeSpaceBefore :: T.TestTree
removeSpaceBefore =
  Parser.removeComments "let foo = 3 \n + \n -- foo foo foo \n 4"
    |> (T.@=? "let foo = 3 \n + \n\n 4")
    |> T.testCase "test remove comments: let foo = 3 \n + \n -- foo foo foo \n 4"

removeNewLineBefore :: T.TestTree
removeNewLineBefore =
  Parser.removeComments "let foo = 3 \n + \n-- foo foo foo \n 4"
    |> (T.@=? "let foo = 3 \n + \n\n 4")
    |> T.testCase "test remove comments: let foo = 3 \n + \n-- foo foo foo \n 4"

-- TODO :: use quick check!

removeNoComment :: T.TestTree
removeNoComment =
  let str = "let foo = 3 \n + \n \n 4"
   in Parser.removeComments str
        |> (T.@=? str)
        |> T.testCase ("test remove comments: " <> str)

--------------------------------------------------------------------------------
-- Parse Many at once
--------------------------------------------------------------------------------

many1FunctionsParser :: T.TestTree
many1FunctionsParser =
  shouldParseAs
    "many1FunctionsParser"
    (parse $ many Parser.topLevelSN)
    ( ""
        <> "let foo a b c = (+) (a + b) c\n"
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
    [ ( AST.Inf
          (AST.Name (NameSym.fromSymbol "a"))
          (NameSym.fromSymbol "+")
          (AST.Name (NameSym.fromSymbol "b"))
          |> AST.Infix
          |> AST.Parened
      )
        :| [AST.Name (NameSym.fromSymbol "c")]
        |> AST.App
          (AST.Name (NameSym.fromSymbol "+"))
        |> AST.Application
        |> AST.Body
        |> AST.Like
          "foo"
          [ AST.ConcreteA (AST.MatchLogic (AST.MatchName "a") Nothing),
            AST.ConcreteA (AST.MatchLogic (AST.MatchName "b") Nothing),
            AST.ConcreteA (AST.MatchLogic (AST.MatchName "c") Nothing)
          ]
        |> AST.Func
        |> AST.Function,
      --
      ( AST.Constant (AST.Number (AST.Integer' 1))
          :| [ AST.Constant (AST.Number (AST.Integer' 2)),
               AST.Constant (AST.Number (AST.Integer' 3))
             ]
      )
        |> AST.App
          (AST.Name (NameSym.fromSymbol "foo"))
        |> AST.Application
        |> AST.Body
        |> AST.Like "bah" []
        |> AST.Func
        |> AST.Function,
      --
      ( AST.CondExpression
          { condLogicPred =
              AST.Integer' 5
                |> AST.Number
                |> AST.Constant
                |> AST.Inf (AST.Name (NameSym.fromSymbol "bah")) (NameSym.fromSymbol "==")
                |> AST.Infix,
            condLogicBody =
              AST.Constant (AST.Number (AST.Integer' 7))
          }
          :| [ AST.Integer' 11
                 |> AST.Number
                 |> AST.Constant
                 |> AST.CondExpression (AST.Name (NameSym.fromSymbol "else"))
             ]
      )
        |> AST.C
        |> AST.Guard
        |> AST.Like "nah" []
        |> AST.Func
        |> AST.Function,
      --

      AST.Let''
        { letBindings =
            AST.Name (NameSym.fromSymbol "nah")
              |> AST.Body
              |> AST.Like "check" [],
          letBody =
            ( AST.MatchL
                { matchLPattern =
                    AST.MatchLogic (AST.MatchName "seven") Nothing,
                  matchLBody =
                    AST.Constant (AST.Number (AST.Integer' 11))
                }
                :| [ AST.Integer' 7
                       |> AST.Number
                       |> AST.Constant
                       |> AST.MatchL (AST.MatchLogic (AST.MatchName "eleven") Nothing),
                     --

                     (AST.Name (NameSym.fromSymbol "failed") :| [])
                       |> AST.App (AST.Name (NameSym.fromSymbol "print"))
                       |> AST.Application
                       |> AST.DoBody Nothing
                       |> (:| [AST.DoBody Nothing (AST.Name (NameSym.fromSymbol "fail"))])
                       |> AST.Do''
                       |> AST.Do
                       |> AST.OpenExpress (NameSym.fromSymbol "Fails")
                       |> AST.OpenExpr
                       |> AST.MatchL (AST.MatchLogic (AST.MatchName "f") Nothing)
                   ]
            )
              |> AST.Match'' (AST.Name (NameSym.fromSymbol "check"))
              |> AST.Match
        }
        |> AST.Let
        |> AST.Body
        |> AST.Like "test" []
        |> AST.Func
        |> AST.Function
    ]

--------------------------------------------------------------------------------
-- Sig Test
--------------------------------------------------------------------------------

sigTest1 :: T.TestTree
sigTest1 =
  shouldParseAs
    "sigTest1"
    Parser.parse
    "sig foo 0 : Int -> Int"
    [ AST.Name (NameSym.fromSymbol "Int")
        |> AST.Inf (AST.Name (NameSym.fromSymbol "Int")) (NameSym.fromSymbol "->")
        |> AST.Infix
        |> flip (AST.Sig "foo" (Just (AST.Constant (AST.Number (AST.Integer' 0))))) []
        |> AST.Signature
    ]

sigTest2 :: T.TestTree
sigTest2 =
  shouldParseAs
    "sigTest2"
    Parser.parse
    "sig foo 0 : i : Int{i > 0} -> Int{i > 1}"
    [ AST.Integer' 1
        |> AST.Number
        |> AST.Constant
        |> AST.Inf (AST.Name (NameSym.fromSymbol "i")) (NameSym.fromSymbol ">")
        |> AST.Infix
        |> AST.TypeRefine (AST.Name (NameSym.fromSymbol "Int"))
        |> AST.RefinedE
        |> AST.Inf
          ( AST.Integer' 0
              |> AST.Number
              |> AST.Constant
              |> AST.Inf (AST.Name (NameSym.fromSymbol "i")) (NameSym.fromSymbol ">")
              |> AST.Infix
              |> AST.TypeRefine (AST.Name (NameSym.fromSymbol "Int"))
              |> AST.RefinedE
          )
          (NameSym.fromSymbol "->")
        |> AST.Infix
        |> AST.Inf (AST.Name (NameSym.fromSymbol "i")) (NameSym.fromSymbol ":")
        |> AST.Infix
        |> flip (AST.Sig "foo" (Just (AST.Constant (AST.Number (AST.Integer' 0))))) []
        |> AST.Signature
    ]

-- --------------------------------------------------------------------------------
-- -- Function Testing
-- --------------------------------------------------------------------------------

fun1 :: T.TestTree
fun1 =
  shouldParseAs
    "fun1"
    Parser.parse
    "let f foo@(A b c d) = 3"
    [ AST.Integer' 3
        |> AST.Number
        |> AST.Constant
        |> AST.Body
        |> AST.Like
          "f"
          [ [ AST.MatchLogic (AST.MatchName "b") Nothing,
              AST.MatchLogic (AST.MatchName "c") Nothing,
              AST.MatchLogic (AST.MatchName "d") Nothing
            ]
              |> AST.MatchCon (NameSym.fromSymbol "A")
              |> flip AST.MatchLogic (Just "foo")
              |> AST.ConcreteA
          ]
        |> AST.Func
        |> AST.Function
    ]

fun2 :: T.TestTree
fun2 =
  shouldParseAs
    "fun2"
    Parser.parse
    "let f foo | foo = 2 | else = 3"
    [ ( AST.Integer' 2
          |> AST.Number
          |> AST.Constant
          |> AST.CondExpression (AST.Name (NameSym.fromSymbol "foo"))
      )
        :| [ AST.Integer' 3
               |> AST.Number
               |> AST.Constant
               |> AST.CondExpression (AST.Name (NameSym.fromSymbol "else"))
           ]
        |> AST.C
        |> AST.Guard
        |> AST.Like
          "f"
          [AST.ConcreteA (AST.MatchLogic (AST.MatchName "foo") Nothing)]
        |> AST.Func
        |> AST.Function
    ]

--------------------------------------------------------------------------------
-- Type tests
--------------------------------------------------------------------------------

--------------------------------------------------
-- adt testing
--------------------------------------------------

sumTypeTest :: T.TestTree
sumTypeTest =
  shouldParseAs
    "sumTypeTest"
    Parser.parse
    ( "type Foo a b c = | A : b : a -> b -> c \n"
        <> "            | B : d -> Foo \n"
        <> "            | C { a : Int, #b : Int } \n"
        <> "            | D { a : Int, #b : Int } : Foo Int (Fooy -> Nada)"
    )
    [ ( AST.Name (NameSym.fromSymbol "c")
          |> AST.Inf (AST.Name (NameSym.fromSymbol "b")) (NameSym.fromSymbol "->")
          |> AST.Infix
          |> AST.Inf (AST.Name (NameSym.fromSymbol "a")) (NameSym.fromSymbol "->")
          |> AST.Infix
          |> AST.Inf (AST.Name (NameSym.fromSymbol "b")) (NameSym.fromSymbol ":")
          |> AST.Infix
          |> AST.Arrow
          |> Just
          |> AST.S "A"
      )
        :| [ AST.Name (NameSym.fromSymbol "Foo")
               |> AST.Inf (AST.Name (NameSym.fromSymbol "d")) (NameSym.fromSymbol "->")
               |> AST.Infix
               |> AST.Arrow
               |> Just
               |> AST.S "B",
             --
             AST.NameType' (AST.Name (NameSym.fromSymbol "Int")) (AST.Concrete "a")
               :| [AST.NameType' (AST.Name (NameSym.fromSymbol "Int")) (AST.Implicit "b")]
               |> flip AST.Record'' Nothing
               |> AST.Record
               |> Just
               |> AST.S "C",
             --
             (AST.Name (NameSym.fromSymbol "Int"))
               :| [ (AST.Name (NameSym.fromSymbol "Nada"))
                      |> AST.Inf
                        (AST.Name (NameSym.fromSymbol "Fooy"))
                        (NameSym.fromSymbol "->")
                      |> AST.Infix
                      |> AST.Parened
                  ]
               |> AST.App (AST.Name (NameSym.fromSymbol "Foo"))
               |> AST.Application
               |> Just
               |> AST.Record''
                 ( AST.NameType' (AST.Name (NameSym.fromSymbol "Int")) (AST.Concrete "a")
                     :| [AST.NameType' (AST.Name (NameSym.fromSymbol "Int")) (AST.Implicit "b")]
                 )
               |> AST.Record
               |> Just
               |> AST.S "D"
           ]
        |> AST.Sum
        |> AST.NonArrowed
        |> AST.Typ Nothing "Foo" ["a", "b", "c"]
        |> AST.Type
    ]

--------------------------------------------------
-- Arrow Testing
--------------------------------------------------

superArrowCase :: T.TestTree
superArrowCase =
  AST.Name (NameSym.fromSymbol "foo")
    |> AST.Inf (AST.Name (NameSym.fromSymbol "HAHAHHA")) (NameSym.fromSymbol "->")
    |> AST.Infix
    |> AST.Parened
    |> AST.Inf
      ( AST.App
          (AST.Name (NameSym.fromSymbol "Bah"))
          (AST.Name (NameSym.fromSymbol "a") :| [(AST.Name (NameSym.fromSymbol "c"))])
          |> AST.Application
      )
      (NameSym.fromSymbol "-o")
    |> AST.Infix
    |> AST.Inf (AST.Name (NameSym.fromSymbol "a")) (NameSym.fromSymbol ":")
    |> AST.Infix
    |> AST.Inf
      ( AST.App
          (AST.Name (NameSym.fromSymbol "Foo"))
          (AST.Name (NameSym.fromSymbol "a") :| [(AST.Name (NameSym.fromSymbol "b"))])
          |> AST.Application
      )
      (NameSym.fromSymbol "->")
    |> AST.Infix
    |> AST.Inf
      ( AST.Name (NameSym.fromSymbol "Foo")
          |> AST.Inf (AST.Name (NameSym.fromSymbol "B")) (NameSym.fromSymbol "-o")
          |> AST.Infix
          |> AST.Inf (AST.Name (NameSym.fromSymbol "c")) (NameSym.fromSymbol ":")
          |> AST.Infix
          |> AST.Inf (AST.Name (NameSym.fromSymbol "Bah")) (NameSym.fromSymbol "->")
          |> AST.Infix
          |> AST.Inf (AST.Name (NameSym.fromSymbol "b")) (NameSym.fromSymbol ":")
          |> AST.Infix
          |> AST.Parened
      )
      (NameSym.fromSymbol "->")
    |> AST.Infix
    |> shouldParseAs
      "superArrowCase"
      (parse Parser.expression)
      "( b : Bah ->  c : B -o Foo) -> Foo a b -> a : Bah a c -o ( HAHAHHA -> foo )"

--------------------------------------------------
-- alias tests
--------------------------------------------------

typeTest :: T.TestTree
typeTest =
  shouldParseAs
    "typeTest"
    Parser.parse
    "type Foo a b c d = | Foo nah bah sad"
    [ [ AST.Name (NameSym.fromSymbol "nah"),
        AST.Name (NameSym.fromSymbol "bah"),
        AST.Name (NameSym.fromSymbol "sad")
      ]
        |> AST.ADTLike
        |> Just
        |> AST.S "Foo"
        |> (:| [])
        |> AST.Sum
        |> AST.NonArrowed
        |> AST.Typ Nothing "Foo" ["a", "b", "c", "d"]
        |> AST.Type
    ]

--------------------------------------------------------------------------------
-- Modules test
--------------------------------------------------------------------------------

moduleOpen :: T.TestTree
moduleOpen =
  shouldParseAs
    "moduleOpen"
    Parser.parse
    ( ""
        <> "mod Foo Int = \n"
        <> "  let T = Int.t \n"
        <> "  sig bah : T -> T \n"
        <> "  let bah t = Int.(t + 3) \n"
        <> "end"
    )
    [ ( AST.Name (NameSym.fromSymbol "Int.t")
          |> AST.Body
          |> AST.Like "T" []
          |> AST.Func
          |> AST.Function
      )
        :| [ AST.Inf
               (AST.Name (NameSym.fromSymbol "T"))
               (NameSym.fromSymbol "->")
               (AST.Name (NameSym.fromSymbol "T"))
               |> AST.Infix
               |> flip (AST.Sig "bah" Nothing) []
               |> AST.Signature,
             --
             AST.Inf
               (AST.Name (NameSym.fromSymbol "t"))
               (NameSym.fromSymbol "+")
               (AST.Constant (AST.Number (AST.Integer' 3)))
               |> AST.Infix
               |> AST.OpenExpress (NameSym.fromSymbol "Int")
               |> AST.OpenExpr
               |> AST.Body
               |> AST.Like
                 "bah"
                 [AST.ConcreteA (AST.MatchLogic (AST.MatchName "t") Nothing)]
               |> AST.Func
               |> AST.Function
           ]
        |> AST.Body
        |> AST.Like
          "Foo"
          [ AST.MatchLogic (AST.MatchCon (NameSym.fromSymbol "Int") []) Nothing
              |> AST.ConcreteA
          ]
        |> AST.Mod
        |> AST.Module
    ]

moduleOpen' :: T.TestTree
moduleOpen' =
  shouldParseAs
    "moduleOpen'"
    Parser.parse
    ( ""
        <> "mod Bah M = \n"
        <> "  open M"
        <> "  sig bah : Rec \n"
        <> "  let bah t = \n"
        <> "     { a = t + 3"
        <> "     , b = expr M.N.t}"
        <> "end"
    )
    [ AST.ModuleOpen (AST.Open (NameSym.fromSymbol "M"))
        :| [ AST.Sig "bah" Nothing (AST.Name (NameSym.fromSymbol "Rec")) []
               |> AST.Signature,
             AST.NonPunned
               (NameSym.fromSymbol "a")
               ( AST.Inf
                   (AST.Name (NameSym.fromSymbol "t"))
                   (NameSym.fromSymbol "+")
                   (AST.Constant (AST.Number (AST.Integer' 3)))
                   |> AST.Infix
               )
               :| [ AST.Name (NameSym.fromSymbol "M.N.t") :| []
                      |> AST.App (AST.Name (NameSym.fromSymbol "expr"))
                      |> AST.Application
                      |> AST.NonPunned (NameSym.fromSymbol "b")
                  ]
                 |> AST.ExpressionRecord
                 |> AST.ExpRecord
                 |> AST.Body
                 |> AST.Like
                   "bah"
                   [AST.ConcreteA (AST.MatchLogic (AST.MatchName "t") Nothing)]
                 |> AST.Func
                 |> AST.Function
           ]
        |> AST.Body
        |> AST.Like
          "Bah"
          -- this shouldn't be a matchCon but a match argument
          [ AST.MatchLogic (AST.MatchCon (NameSym.fromSymbol "M") []) Nothing
              |> AST.ConcreteA
          ]
        |> AST.Mod
        |> AST.Module
    ]

--------------------------------------------------
-- typeName tests
--------------------------------------------------

typeNameNoUniverse :: T.TestTree
typeNameNoUniverse =
  AST.Name (NameSym.fromSymbol "a")
    :| [ AST.Name (NameSym.fromSymbol "b"),
         AST.Name (NameSym.fromSymbol "c"),
         AST.Name (NameSym.fromSymbol "d")
           |> AST.Inf (AST.Name (NameSym.fromSymbol "b")) (NameSym.fromSymbol "-o")
           |> AST.Infix
           |> AST.Parened,
         AST.Name (NameSym.fromSymbol "a"),
         AST.Name (NameSym.fromSymbol "c"),
         AST.Name (NameSym.fromSymbol "u")
       ]
    |> AST.App (AST.Name (intern "Foo" :| []))
    |> AST.Application
    |> shouldParseAs
      "typeNameNoUniverse"
      (parse Parser.expression)
      "Foo a b c (b -o d) a c u"

--------------------------------------------------------------------------------
-- Match tests
--------------------------------------------------------------------------------

simpleNamedCon :: T.TestTree
simpleNamedCon =
  [ AST.MatchLogic (AST.MatchName "a") Nothing,
    AST.MatchLogic (AST.MatchName "b") Nothing,
    AST.MatchLogic (AST.MatchName "c") Nothing
  ]
    |> AST.MatchCon (NameSym.fromSymbol "Hi")
    |> flip AST.MatchLogic (Just "foo")
    |> shouldParseAs
      "simpleNamedCon"
      (parse Parser.matchLogic)
      "foo@( Hi a b c )"

matchMoreComplex :: T.TestTree
matchMoreComplex =
  [ Nothing
      |> AST.MatchLogic (AST.MatchName "nah")
      |> AST.NonPunned (NameSym.fromSymbol "a")
      |> (:| [AST.Punned (NameSym.fromSymbol "f")])
      |> AST.MatchRecord
      |> flip AST.MatchLogic (Just "nah"),
    --
    AST.MatchLogic (AST.MatchName "b") Nothing,
    --
    AST.MatchLogic (AST.MatchConst (AST.Number (AST.Integer' 5))) Nothing
  ]
    |> AST.MatchCon (NameSym.fromSymbol "Hi")
    |> flip AST.MatchLogic (Just "foo")
    |> shouldParseAs
      "matchMoreComplex"
      (parse Parser.matchLogic)
      "foo@( Hi nah@{ a = nah , f } b 5 )"

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

condTest1 :: T.TestTree
condTest1 =
  AST.CondExpression
    { condLogicPred = AST.Name (NameSym.fromSymbol "foo"),
      condLogicBody = AST.Name (NameSym.fromSymbol "a")
    }
    :| [ AST.CondExpression
           { condLogicPred = AST.Name (NameSym.fromSymbol "else"),
             condLogicBody = AST.Name (NameSym.fromSymbol "b")
           }
       ]
    |> AST.C
    |> shouldParseAs
      "condTest1"
      (parse Parser.cond)
      ( ""
          <> "if  | foo  = a\n"
          <> "    | else = b "
      )

--------------------------------------------------
-- Record
--------------------------------------------------

record1 :: T.TestTree
record1 =
  AST.Punned (NameSym.fromSymbol "a")
    :| [ AST.Inf
           { infixLeft = AST.Constant (AST.Number (AST.Integer' 3)),
             infixOp = NameSym.fromSymbol "+",
             infixRight = AST.Constant (AST.Number (AST.Integer' 5))
           }
           |> AST.Infix
           |> AST.NonPunned (NameSym.fromSymbol "b")
       ]
    |> AST.ExpressionRecord
    |> AST.ExpRecord
    |> shouldParseAs
      "record1"
      (parse Parser.expression)
      "{a, b = 3+5}"

--------------------------------------------------
-- parens
--------------------------------------------------

parens1 :: T.TestTree
parens1 =
  AST.Punned (NameSym.fromSymbol "a")
    :| [ AST.Integer' 5
           |> AST.Number
           |> AST.Constant
           |> AST.Inf
             (AST.Constant (AST.Number (AST.Integer' 3)))
             (NameSym.fromSymbol "+")
           |> AST.Infix
           |> AST.NonPunned (NameSym.fromSymbol "b")
       ]
    |> AST.ExpressionRecord
    |> AST.ExpRecord
    |> AST.Parened
    |> AST.Parened
    |> AST.Parened
    |> AST.Parened
    |> shouldParseAs
      "parens1"
      (parse Parser.expression)
      "(       ( (({a, b = 3+5}))))"

--------------------------------------------------
-- Infix Tests
--------------------------------------------------

nonassocTest :: T.TestTree
nonassocTest =
  shouldParseAs
    "declare infix foo 5"
    Parser.parse
    "declare infix foo 5"
    [AST.Declaration (AST.Infixivity (AST.NonAssoc "foo" 5))]

infxrTest :: T.TestTree
infxrTest =
  shouldParseAs
    "declare infixr foo 5"
    Parser.parse
    "declare infixr foo 5"
    [AST.Declaration (AST.Infixivity (AST.AssocR "foo" 5))]

infxlTest :: T.TestTree
infxlTest =
  shouldParseAs
    "declare infixl foo 5"
    Parser.parse
    "declare infixl foo 5"
    [AST.Declaration (AST.Infixivity (AST.AssocL "foo" 5))]

infxPlusTest :: T.TestTree
infxPlusTest =
  shouldParseAs
    "declare infixl (+) 5"
    Parser.parse
    "declare infixl (+) 5"
    [AST.Declaration (AST.Infixivity (AST.AssocL "+" 5))]

infixPlusFail :: T.TestTree
infixPlusFail =
  T.testCase
    ("parse: declare infixl + 5 should fail")
    (isLeft (Parser.parseOnly "declare infixl + 5") T.@=? True)

infixFail :: T.TestTree
infixFail =
  T.testCase
    ("parse: declare infixl foo.o 5 should fail")
    (isLeft (Parser.parseOnly "declare infixl foo.o 5") T.@=? True)

--------------------------------------------------
-- reserved word tests
--------------------------------------------------

letwordFail :: T.TestTree
letwordFail =
  T.testCase
    ("parse: letfad = 3 should fail")
    (isLeft (Parser.parseOnly "letfad = 3") T.@=? True)

reservedInfix :: T.TestTree
reservedInfix =
  shouldParseAs
    "reserved then infex"
    Parser.parse
    "let(+) = %Michelson.plus"
    [ NameSym.fromSymbol "Michelson.plus"
        |> AST.Prim
        |> AST.Primitive
        |> AST.Body
        |> AST.Like "+" []
        |> AST.Func
        |> AST.Function
    ]

caseOfWords :: T.TestTree
caseOfWords =
  shouldParseAs
    "caseOfWords"
    Parser.parse
    "let foo = case x-of (of-x)of | x -> y"
    [ NameSym.fromSymbol "y"
        |> AST.Name
        |> AST.MatchL (AST.MatchLogic (AST.MatchName "x") Nothing)
        |> (:| [])
        |> AST.Match''
          ( NameSym.fromSymbol "of-x"
              |> AST.Name
              |> AST.Parened
              |> (:| [])
              |> AST.App (AST.Name (NameSym.fromSymbol "x-of"))
              |> AST.Application
          )
        |> AST.Match
        |> AST.Body
        |> AST.Like "foo" []
        |> AST.Func
        |> AST.Function
    ]

--------------------------------------------------------------------------------
-- Spacer tests
--------------------------------------------------------------------------------

spacerSymb :: T.TestTree
spacerSymb =
  let res =
        case parse (Parser.spacer Parser.prefixSymbol) "Foo   f" of
          Done f s -> f == "f" && s == "Foo"
          _ -> False
   in T.testCase "symbol parser test: Foo f" (res T.@=? True)

--------------------------------------------------------------------------------
-- validPrefixSymbols
--------------------------------------------------------------------------------

vpsDashFrontFail :: T.TestTree
vpsDashFrontFail =
  T.testCase
    "-Foo is not a valid prefix symbol"
    (isLeft (parseOnly Parser.prefixSymbol "-Foo") T.@=? True)

vpsDashMiddle :: T.TestTree
vpsDashMiddle =
  T.testCase
    "Foo-Foo is a valid prefix symbol"
    (isRight (parseOnly Parser.prefixSymbol "Foo-Foo") T.@=? True)

questionMarktest :: T.TestTree
questionMarktest =
  T.testCase
    "foo? is a valid prefix symbol"
    (parseOnly Parser.prefixSymbol "foo?" T.@=? Right "foo?")

bangtest :: T.TestTree
bangtest =
  T.testCase
    "foo! is a valid prefix symbol"
    (parseOnly Parser.prefixSymbol "foo!" T.@=? Right "foo!")

--------------------------------------------------------------------------------
-- Examples for testing
--------------------------------------------------------------------------------

contractTest :: Either String [TopLevel]
contractTest =
  parseOnly
    (many Parser.topLevelSN)
    ( ""
        <> "mod Token = "
        <> "  let Address = s : String.T {String.length s == 36} \n"
        <> "\n"
        <> "  type Storage = { \n"
        <> "    total-supply : Nat.T, \n"
        <> "    accounts     : Accounts.T { Accounts.measure-value == total-supply } \n"
        <> "  }"
        <> "  sig empty-storage : Storage \n"
        <> "  let empty-storage = { \n"
        <> "    total-supply = 0, \n"
        <> "    accounts     = Accounts.empty, \n"
        <> "  } \n"
        <> " \n"
        <> "  type T = { \n"
        <> "    storage : Storage, \n"
        <> "    version : Nat.T, \n"
        <> "    name    : String.T, \n"
        <> "    symbol  : Char.T, \n"
        <> "    owner   : Address, \n"
        <> "  } \n"
        <> "end"
        <> " \n"
        <> "mod Transaction = \n"
        <> "  type Transfer = { \n"
        <> "    from-account : Token.Address, \n"
        <> "    to-account   : Token.Address, \n"
        <> "    ammount      : Nat.T, \n"
        <> "  } \n"
        <> " \n"
        <> "  type Mint = { \n"
        <> "    mint-amount     : Nat.T, \n"
        <> "    mint-to-account : Token.Address, \n"
        <> "  } \n"
        <> " \n"
        <> "  type Burn = { \n"
        <> "    burn-amount       : Nat.T, \n"
        <> "    burn-from-account : Token.Address, \n"
        <> "  } \n"
        <> " \n"
        <> "  type Data = \n"
        <> "    | Transfer : Transfer -> Data \n"
        <> "    | Mint     : Mint     -> Data \n"
        <> "    | Burn     : Burn     -> Data \n"
        <> " \n"
        <> "  type T = { \n"
        <> "    data               : Data, \n"
        <> "    authorized-account : Token.Address, \n"
        <> "  } \n"
        <> "end \n"
        <> " \n"
        <> "sig has-n : Accounts.T -> Token.Address -> Nat -> Bool \n"
        <> "let has-n accounts add to-transfer = \n"
        <> "  case Accounts.select accounts add of \n"
        <> "  | Just n  -> to-transfer <= n \n"
        <> "  | Nothing -> False \n"
        <> " \n"
        <> " \n"
        <> "sig account-sub : acc : Accounts.T \n"
        <> "               -> add : Token.Address \n"
        <> "               -> num : Nat.T {has-n acc add num} \n"
        <> "               -> Accounts.T \n"
        <> "let account-sub accounts add number = \n"
        <> "  case Accounts.select accounts add of \n"
        <> "  | Just balance -> \n"
        <> "     Accounts.put accounts add (balance - number) \n"
        <> " \n"
        <> "sig account-add : Accounts.T -> Token.Address -> Nat.T -> Accounts.T \n"
        <> "let account-add accounts add number = \n"
        <> "  Accounts.update accounts ((+) number) add \n"
        <> " \n"
        <> " \n"
        <> " \n"
        <> "sig transfer-stor : stor  : Token.Storage \n"
        <> "                 -> from  : Token.Address \n"
        <> "                 -> to    : Token.Address \n"
        <> "                 -> num   : Nat.T {has-n stor.accounts from num} \n"
        <> "                 -> Token.Storage \n"
        <> "let transfer-stor stor add_from add_to num = \n"
        <> "  let new-acc = account-add (account-sub stor.accounts add_from) add-to num in \n"
        <> "  { total-supply = stor.total-supply \n"
        <> "  , accounts     = new-acc \n"
        <> "  } \n"
        <> "mod Validation = \n"
        <> "  let T = Token.T -> Transaction.T -> Bool \n"
        <> " \n"
        <> "  let mint token tx = \n"
        <> "    case tx.data of \n"
        <> "    | Transaction.Mint -> \n"
        <> "      token.owner == tx-tx-authorized-account \n"
        <> "    | _ -> \n"
        <> "      false \n"
        <> " \n"
        <> "  let transfer token tx = \n"
        <> "    case tx.data of \n"
        <> "    | Transaction.Transfer {from-account, amount} -> \n"
        <> "      has-n token.storage.accounts from-account amount \n"
        <> "      && tx.authroized-account == from-account \n"
        <> "    | _ -> \n"
        <> "      false \n"
        <> " \n"
        <> "  let Burn token tx = \n"
        <> "    case tx.data of \n"
        <> "    | Transaction.Burn {burn-from-account, burn-ammount} -> \n"
        <> "      has-n token.storage.accounts burn-from-account burn-amount \n"
        <> "      && tx.authroized-account == burn-from-account \n"
        <> "    | _ -> \n"
        <> "      false \n"
        <> "end \n"
        <> " \n"
        <> "  type Error \n"
        <> "    = NotEnoughFunds \n"
        <> "    | NotSameAccount \n"
        <> "    | NotOwnerToken  \n"
        <> "    | NotEnoughTokens \n"
        <> " \n"
        <> "  sig exec : Token.T -> Transaction.T -> Either.T Error Token.T \n"
        <> "  let exec token tx = \n"
        <> "    case tx.data of \n"
        <> "    | Transfer _ -> \n"
        <> "      if | Validation.transfer token tx = Right (transfer token tx) \n"
        <> "         | else                         = Left NotEnoughFunds \n"
        <> "    | Mint _ -> \n"
        <> "      if | Validation.mint token tx = Right (mint token tx) \n"
        <> "         | else                     = Left NotEnoughFunds \n"
    )
