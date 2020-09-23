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
import qualified Juvix.Core.Common.NameSymbol as NameSymbol
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Frontend.Types (Expression, TopLevel)
import qualified Juvix.Frontend.Types as AST
import Juvix.Frontend.Types.Base
import Juvix.Library hiding (show)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (String, error, show)

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
      removeSpaceBefore
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
    [ Function'
        ( Func'
            ( Like'
                { functionLikedName = "foo",
                  functionLikeArgs =
                    [ ConcreteA'
                        ( MatchLogic'
                            { matchLogicContents = MatchName' "a" (),
                              matchLogicNamed = Nothing,
                              annMatchLogic = ()
                            }
                        )
                        (),
                      ConcreteA'
                        ( MatchLogic'
                            { matchLogicContents = MatchName' "b" (),
                              matchLogicNamed = Nothing,
                              annMatchLogic = ()
                            }
                        )
                        (),
                      ConcreteA'
                        ( MatchLogic'
                            { matchLogicContents = MatchName' "c" (),
                              matchLogicNamed = Nothing,
                              annMatchLogic = ()
                            }
                        )
                        ()
                    ],
                  functionLikeBody =
                    Body'
                      ( Application'
                          ( App'
                              { applicationName = Name' ("+" :| []) (),
                                applicationArgs =
                                  Parened'
                                    ( Infix'
                                        ( Inf'
                                            { infixLeft = Name' ("a" :| []) (),
                                              infixOp = "+" :| [],
                                              infixRight = Name' ("b" :| []) (),
                                              annInf = ()
                                            }
                                        )
                                        ()
                                    )
                                    ()
                                    :| [Name' ("c" :| []) ()],
                                annApp = ()
                              }
                          )
                          ()
                      )
                      (),
                  annLike = ()
                }
            )
            ()
        )
        (),
      Function'
        ( Func'
            ( Like'
                { functionLikedName = "bah",
                  functionLikeArgs = [],
                  functionLikeBody = Body' (Application' (App' {applicationName = Name' ("foo" :| []) (), applicationArgs = Constant' (Number' (Integer'' 1 ()) ()) () :| [Constant' (Number' (Integer'' 2 ()) ()) (), Constant' (Number' (Integer'' 3 ()) ()) ()], annApp = ()}) ()) (),
                  annLike = ()
                }
            )
            ()
        )
        (),
      Function'
        ( Func'
            ( Like'
                { functionLikedName = "nah",
                  functionLikeArgs = [],
                  functionLikeBody = Guard' (C' (CondExpression' {condLogicPred = Infix' (Inf' {infixLeft = Name' ("bah" :| []) (), infixOp = "==" :| [], infixRight = Constant' (Number' (Integer'' 5 ()) ()) (), annInf = ()}) (), condLogicBody = Constant' (Number' (Integer'' 7 ()) ()) (), annCondExpression = ()} :| [CondExpression' {condLogicPred = Name' ("else" :| []) (), condLogicBody = Constant' (Number' (Integer'' 11 ()) ()) (), annCondExpression = ()}]) ()) (),
                  annLike = ()
                }
            )
            ()
        )
        (),
      Function'
        ( Func'
            ( Like'
                { functionLikedName = "test",
                  functionLikeArgs = [],
                  functionLikeBody = Body' (Let' (Let''' {letBindings = Like' {functionLikedName = "check", functionLikeArgs = [], functionLikeBody = Body' (Name' ("nah" :| []) ()) (), annLike = ()}, letBody = Match' (Match''' {matchOn = Name' ("check" :| []) (), matchBindigns = MatchL' {matchLPattern = MatchLogic' {matchLogicContents = MatchName' "seven" (), matchLogicNamed = Nothing, annMatchLogic = ()}, matchLBody = Constant' (Number' (Integer'' 11 ()) ()) (), annMatchL = ()} :| [MatchL' {matchLPattern = MatchLogic' {matchLogicContents = MatchName' "eleven" (), matchLogicNamed = Nothing, annMatchLogic = ()}, matchLBody = Constant' (Number' (Integer'' 7 ()) ()) (), annMatchL = ()}, MatchL' {matchLPattern = MatchLogic' {matchLogicContents = MatchName' "f" (), matchLogicNamed = Nothing, annMatchLogic = ()}, matchLBody = OpenExpr' (OpenExpress' {moduleOpenExprModuleN = "Fails" :| [], moduleOpenExprExpr = Do' (Do''' (DoBody' {doBodyName = Nothing, doBodyExpr = Application' (App' {applicationName = Name' ("print" :| []) (), applicationArgs = Name' ("failed" :| []) () :| [], annApp = ()}) (), annDoBody = ()} :| [DoBody' {doBodyName = Nothing, doBodyExpr = Name' ("fail" :| []) (), annDoBody = ()}]) ()) (), annOpenExpress = ()}) (), annMatchL = ()}], annMatch'' = ()}) (), annLet'' = ()}) ()) (),
                  annLike = ()
                }
            )
            ()
        )
        ()
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
    [ Signature'
        ( Sig'
            { signatureName = Sym "foo",
              signatureUsage = Just (Constant' (Number' (Integer'' 0 ()) ()) ()),
              signatureArrowType =
                Infix'
                  ( Inf'
                      { infixLeft = Name' (Sym "Int" :| []) (),
                        infixOp = Sym "->" :| [],
                        infixRight = Name' (Sym "Int" :| []) (),
                        annInf = ()
                      }
                  )
                  (),
              signatureConstraints = [],
              annSig = ()
            }
        )
        ()
    ]

sigTest2 :: T.TestTree
sigTest2 =
  shouldParseAs
    "sigTest2"
    Parser.parse
    "sig foo 0 : i : Int{i > 0} -> Int{i > 1}"
    [ Signature'
        ( Sig'
            { signatureName = Sym "foo",
              signatureUsage = Just (Constant' (Number' (Integer'' 0 ()) ()) ()),
              signatureArrowType =
                Infix'
                  ( Inf'
                      { infixLeft = Name' (Sym "i" :| []) (),
                        infixOp = Sym ":" :| [],
                        infixRight =
                          Infix'
                            ( Inf'
                                { infixLeft =
                                    RefinedE'
                                      ( TypeRefine'
                                          { typeRefineName = Name' (Sym "Int" :| []) (),
                                            typeRefineRefinement =
                                              Infix'
                                                ( Inf'
                                                    { infixLeft = Name' (Sym "i" :| []) (),
                                                      infixOp = Sym ">" :| [],
                                                      infixRight = Constant' (Number' (Integer'' 0 ()) ()) (),
                                                      annInf = ()
                                                    }
                                                )
                                                (),
                                            annTypeRefine = ()
                                          }
                                      )
                                      (),
                                  infixOp = Sym "->" :| [],
                                  infixRight =
                                    RefinedE'
                                      ( TypeRefine'
                                          { typeRefineName = Name' (Sym "Int" :| []) (),
                                            typeRefineRefinement =
                                              Infix'
                                                ( Inf'
                                                    { infixLeft = Name' (Sym "i" :| []) (),
                                                      infixOp = Sym ">" :| [],
                                                      infixRight =
                                                        Constant'
                                                          ( Number'
                                                              (Integer'' 1 ())
                                                              ()
                                                          )
                                                          (),
                                                      annInf = ()
                                                    }
                                                )
                                                (),
                                            annTypeRefine = ()
                                          }
                                      )
                                      (),
                                  annInf = ()
                                }
                            )
                            (),
                        annInf = ()
                      }
                  )
                  (),
              signatureConstraints = [],
              annSig = ()
            }
        )
        ()
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
              |> AST.MatchCon (NameSymbol.fromSymbol "A")
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
          |> AST.CondExpression (AST.Name (NameSymbol.fromSymbol "foo"))
      )
        :| [ AST.Integer' 3
               |> AST.Number
               |> AST.Constant
               |> AST.CondExpression (AST.Name (NameSymbol.fromSymbol "else"))
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
    [ ( AST.Name (NameSymbol.fromSymbol "c")
          |> AST.Inf (AST.Name (NameSymbol.fromSymbol "b")) (NameSymbol.fromSymbol "->")
          |> AST.Infix
          |> AST.Inf (AST.Name (NameSymbol.fromSymbol "a")) (NameSymbol.fromSymbol "->")
          |> AST.Infix
          |> AST.Inf (AST.Name (NameSymbol.fromSymbol "b")) (NameSymbol.fromSymbol ":")
          |> AST.Infix
          |> AST.Arrow
          |> Just
          |> AST.S "A"
      )
        :| [ AST.Name (NameSymbol.fromSymbol "Foo")
               |> AST.Inf (AST.Name (NameSymbol.fromSymbol "d")) (NameSymbol.fromSymbol "->")
               |> AST.Infix
               |> AST.Arrow
               |> Just
               |> AST.S "B",
             --
             AST.NameType' (AST.Name (NameSymbol.fromSymbol "Int")) (AST.Concrete "a")
               :| [AST.NameType' (AST.Name (NameSymbol.fromSymbol "Int")) (AST.Implicit "b")]
               |> flip AST.Record'' Nothing
               |> AST.Record
               |> Just
               |> AST.S "C",
             --
             (AST.Name (NameSymbol.fromSymbol "Int"))
               :| [ (AST.Name (NameSymbol.fromSymbol "Nada"))
                      |> AST.Inf
                        (AST.Name (NameSymbol.fromSymbol "Fooy"))
                        (NameSymbol.fromSymbol "->")
                      |> AST.Infix
                      |> AST.Parened
                  ]
               |> AST.App (AST.Name (NameSymbol.fromSymbol "Foo"))
               |> AST.Application
               |> Just
               |> AST.Record''
                 ( AST.NameType' (AST.Name (NameSymbol.fromSymbol "Int")) (AST.Concrete "a")
                     :| [AST.NameType' (AST.Name (NameSymbol.fromSymbol "Int")) (AST.Implicit "b")]
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
  AST.Name (NameSymbol.fromSymbol "foo")
    |> AST.Inf (AST.Name (NameSymbol.fromSymbol "HAHAHHA")) (NameSymbol.fromSymbol "->")
    |> AST.Infix
    |> AST.Parened
    |> AST.Inf
      ( AST.App
          (AST.Name (NameSymbol.fromSymbol "Bah"))
          (AST.Name (NameSymbol.fromSymbol "a") :| [(AST.Name (NameSymbol.fromSymbol "c"))])
          |> AST.Application
      )
      (NameSymbol.fromSymbol "-o")
    |> AST.Infix
    |> AST.Inf (AST.Name (NameSymbol.fromSymbol "a")) (NameSymbol.fromSymbol ":")
    |> AST.Infix
    |> AST.Inf
      ( AST.App
          (AST.Name (NameSymbol.fromSymbol "Foo"))
          (AST.Name (NameSymbol.fromSymbol "a") :| [(AST.Name (NameSymbol.fromSymbol "b"))])
          |> AST.Application
      )
      (NameSymbol.fromSymbol "->")
    |> AST.Infix
    |> AST.Inf
      ( AST.Name (NameSymbol.fromSymbol "Foo")
          |> AST.Inf (AST.Name (NameSymbol.fromSymbol "B")) (NameSymbol.fromSymbol "-o")
          |> AST.Infix
          |> AST.Inf (AST.Name (NameSymbol.fromSymbol "c")) (NameSymbol.fromSymbol ":")
          |> AST.Infix
          |> AST.Inf (AST.Name (NameSymbol.fromSymbol "Bah")) (NameSymbol.fromSymbol "->")
          |> AST.Infix
          |> AST.Inf (AST.Name (NameSymbol.fromSymbol "b")) (NameSymbol.fromSymbol ":")
          |> AST.Infix
          |> AST.Parened
      )
      (NameSymbol.fromSymbol "->")
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
    [ [ AST.Name (NameSymbol.fromSymbol "nah"),
        AST.Name (NameSymbol.fromSymbol "bah"),
        AST.Name (NameSymbol.fromSymbol "sad")
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
    [ ( AST.Name (NameSymbol.fromSymbol "Int.t")
          |> AST.Body
          |> AST.Like "T" []
          |> AST.Func
          |> AST.Function
      )
        :| [ AST.Inf
               (AST.Name (NameSymbol.fromSymbol "T"))
               (NameSymbol.fromSymbol "->")
               (AST.Name (NameSymbol.fromSymbol "T"))
               |> AST.Infix
               |> flip (AST.Sig "bah" Nothing) []
               |> AST.Signature,
             --
             AST.Inf
               (AST.Name (NameSymbol.fromSymbol "t"))
               (NameSymbol.fromSymbol "+")
               (AST.Constant (AST.Number (AST.Integer' 3)))
               |> AST.Infix
               |> AST.OpenExpress (NameSymbol.fromSymbol "Int")
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
          [ AST.MatchLogic (AST.MatchCon (NameSymbol.fromSymbol "Int") []) Nothing
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
    [ AST.ModuleOpen (AST.Open (NameSymbol.fromSymbol "M"))
        :| [ AST.Sig "bah" Nothing (AST.Name (NameSymbol.fromSymbol "Rec")) []
               |> AST.Signature,
             AST.NonPunned
               (NameSymbol.fromSymbol "a")
               ( AST.Inf
                   (AST.Name (NameSymbol.fromSymbol "t"))
                   (NameSymbol.fromSymbol "+")
                   (AST.Constant (AST.Number (AST.Integer' 3)))
                   |> AST.Infix
               )
               :| [ AST.Name (NameSymbol.fromSymbol "M.N.t") :| []
                      |> AST.App (AST.Name (NameSymbol.fromSymbol "expr"))
                      |> AST.Application
                      |> AST.NonPunned (NameSymbol.fromSymbol "b")
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
          [ AST.MatchLogic (AST.MatchCon (NameSymbol.fromSymbol "M") []) Nothing
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
  AST.Name (NameSymbol.fromSymbol "a")
    :| [ AST.Name (NameSymbol.fromSymbol "b"),
         AST.Name (NameSymbol.fromSymbol "c"),
         AST.Name (NameSymbol.fromSymbol "d")
           |> AST.Inf (AST.Name (NameSymbol.fromSymbol "b")) (NameSymbol.fromSymbol "-o")
           |> AST.Infix
           |> AST.Parened,
         AST.Name (NameSymbol.fromSymbol "a"),
         AST.Name (NameSymbol.fromSymbol "c"),
         AST.Name (NameSymbol.fromSymbol "u")
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
    |> AST.MatchCon (NameSymbol.fromSymbol "Hi")
    |> flip AST.MatchLogic (Just "foo")
    |> shouldParseAs
      "simpleNamedCon"
      (parse Parser.matchLogic)
      "foo@( Hi a b c )"

matchMoreComplex :: T.TestTree
matchMoreComplex =
  [ Nothing
      |> AST.MatchLogic (AST.MatchName "nah")
      |> AST.NonPunned (NameSymbol.fromSymbol "a")
      |> (:| [AST.Punned (NameSymbol.fromSymbol "f")])
      |> AST.MatchRecord
      |> flip AST.MatchLogic (Just "nah"),
    --
    AST.MatchLogic (AST.MatchName "b") Nothing,
    --
    AST.MatchLogic (AST.MatchConst (AST.Number (AST.Integer' 5))) Nothing
  ]
    |> AST.MatchCon (NameSymbol.fromSymbol "Hi")
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
    { condLogicPred = AST.Name (intern "foo" :| []),
      condLogicBody = AST.Name (intern "a" :| [])
    }
    :| [ AST.CondExpression
           { condLogicPred = AST.Name (intern "else" :| []),
             condLogicBody = AST.Name (intern "b" :| [])
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
  AST.Punned (intern "a" :| [])
    :| [ AST.Inf
           { infixLeft = AST.Constant (AST.Number (AST.Integer' 3)),
             infixOp = intern "+" :| [],
             infixRight = AST.Constant (AST.Number (AST.Integer' 5))
           }
           |> AST.Infix
           |> AST.NonPunned (intern "b" :| [])
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
  AST.Punned (intern "a" :| [])
    :| [ AST.Inf
           { infixLeft = AST.Constant (AST.Number (AST.Integer' 3)),
             infixOp = intern "+" :| [],
             infixRight = AST.Constant (AST.Number (AST.Integer' 5))
           }
           |> AST.Infix
           |> AST.NonPunned (intern "b" :| [])
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

--------------------------------------------------------------------------------
-- Spacer tests
--------------------------------------------------------------------------------

spacerSymb :: Bool
spacerSymb =
  case parse (Parser.spacer Parser.prefixSymbol) "Foo   f" of
    Done f s -> f == "f" && s == "Foo"
    _ -> False

--------------------------------------------------------------------------------
-- validPrefixSymbols
--------------------------------------------------------------------------------

vpsDashFrontFail :: Bool
vpsDashFrontFail =
  isLeft (parseOnly Parser.prefixSymbol "-Foo")

vpsDashMiddle :: Bool
vpsDashMiddle =
  isRight (parseOnly Parser.prefixSymbol "Foo-Foo")

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
