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
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Frontend.Types (Expression)
import Juvix.Frontend.Types.Base
import Juvix.Library
  ( ($),
    (&&),
    Alternative (many),
    Bool (False),
    ByteString,
    Either,
    Eq ((==)),
    Maybe (Just, Nothing),
    NonEmpty ((:|)),
    Semigroup ((<>)),
    Show,
    Symbol (Sym),
    Word8,
    isLeft,
    isRight,
    (|>),
  )
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

takeResult :: Result s -> s
takeResult (Done _ x) = x
takeResult (Partial y) = takeResult (y "")
takeResult (Fail _ _ errorMsg) = error errorMsg

shouldParseAs ::
  (Show a, Eq a) => T.TestName -> (ByteString -> Result a) -> ByteString -> a -> T.TestTree
shouldParseAs name parses x y =
  T.testGroup
    "parse Test"
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
    -- to avoid having too many lines some of these lines go over 80
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
    [ Function'
        ( Func'
            ( Like'
                { functionLikedName = Sym "f",
                  functionLikeArgs =
                    [ ConcreteA'
                        ( MatchLogic'
                            { matchLogicContents =
                                MatchCon'
                                  (Sym "A" :| [])
                                  [ MatchLogic'
                                      { matchLogicContents = MatchName' (Sym "b") (),
                                        matchLogicNamed = Nothing,
                                        annMatchLogic = ()
                                      },
                                    MatchLogic'
                                      { matchLogicContents = MatchName' (Sym "c") (),
                                        matchLogicNamed = Nothing,
                                        annMatchLogic = ()
                                      },
                                    MatchLogic'
                                      { matchLogicContents = MatchName' (Sym "d") (),
                                        matchLogicNamed = Nothing,
                                        annMatchLogic = ()
                                      }
                                  ]
                                  (),
                              matchLogicNamed = Just (Sym "foo"),
                              annMatchLogic = ()
                            }
                        )
                        ()
                    ],
                  functionLikeBody =
                    Body'
                      ( Constant'
                          (Number' (Integer'' 3 ()) ())
                          ()
                      )
                      (),
                  annLike = ()
                }
            )
            ()
        )
        ()
    ]

fun2 :: T.TestTree
fun2 =
  shouldParseAs
    "fun2"
    Parser.parse
    "let f foo | foo = 2 | else = 3"
    [ Function'
        ( Func'
            ( Like'
                { functionLikedName = Sym "f",
                  functionLikeArgs =
                    [ ConcreteA'
                        ( MatchLogic'
                            { matchLogicContents = MatchName' (Sym "foo") (),
                              matchLogicNamed = Nothing,
                              annMatchLogic = ()
                            }
                        )
                        ()
                    ],
                  functionLikeBody =
                    Guard'
                      ( C'
                          ( CondExpression'
                              { condLogicPred = Name' (Sym "foo" :| []) (),
                                condLogicBody = Constant' (Number' (Integer'' 2 ()) ()) (),
                                annCondExpression = ()
                              }
                              :| [ CondExpression'
                                     { condLogicPred = Name' (Sym "else" :| []) (),
                                       condLogicBody = Constant' (Number' (Integer'' 3 ()) ()) (),
                                       annCondExpression = ()
                                     }
                                 ]
                          )
                          ()
                      )
                      (),
                  annLike = ()
                }
            )
            ()
        )
        ()
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
    [ Type'
        ( Typ'
            { typeUsage = Nothing,
              typeName' = Sym "Foo",
              typeArgs =
                [Sym "a", Sym "b", Sym "c"],
              typeForm =
                NonArrowed'
                  { dataAdt =
                      Sum'
                        ( S'
                            { sumConstructor = Sym "A",
                              sumValue =
                                Just
                                  ( Arrow'
                                      ( Infix'
                                          ( Inf'
                                              { infixLeft = Name' (Sym "b" :| []) (),
                                                infixOp = Sym ":" :| [],
                                                infixRight =
                                                  Infix'
                                                    ( Inf'
                                                        { infixLeft = Name' (Sym "a" :| []) (),
                                                          infixOp = Sym "->" :| [],
                                                          infixRight =
                                                            Infix'
                                                              ( Inf'
                                                                  { infixLeft =
                                                                      Name'
                                                                        (Sym "b" :| [])
                                                                        (),
                                                                    infixOp = Sym "->" :| [],
                                                                    infixRight = Name' (Sym "c" :| []) (),
                                                                    annInf = ()
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
                                          ()
                                      )
                                      ()
                                  ),
                              annS = ()
                            }
                            :| [ S'
                                   { sumConstructor = Sym "B",
                                     sumValue =
                                       Just
                                         ( Arrow'
                                             ( Infix'
                                                 ( Inf'
                                                     { infixLeft = Name' (Sym "d" :| []) (),
                                                       infixOp = Sym "->" :| [],
                                                       infixRight = Name' (Sym "Foo" :| []) (),
                                                       annInf = ()
                                                     }
                                                 )
                                                 ()
                                             )
                                             ()
                                         ),
                                     annS = ()
                                   },
                                 S'
                                   { sumConstructor = Sym "C",
                                     sumValue =
                                       Just
                                         ( Record'
                                             ( Record'''
                                                 { recordFields =
                                                     NameType''
                                                       { nameTypeSignature = Name' (Sym "Int" :| []) (),
                                                         nameTypeName = Concrete' (Sym "a") (),
                                                         annNameType' = ()
                                                       }
                                                       :| [ NameType''
                                                              { nameTypeSignature = Name' (Sym "Int" :| []) (),
                                                                nameTypeName = Implicit' (Sym "b") (),
                                                                annNameType' = ()
                                                              }
                                                          ],
                                                   recordFamilySignature = Nothing,
                                                   annRecord'' = ()
                                                 }
                                             )
                                             ()
                                         ),
                                     annS = ()
                                   },
                                 S'
                                   { sumConstructor = Sym "D",
                                     sumValue =
                                       Just
                                         ( Record'
                                             ( Record'''
                                                 { recordFields =
                                                     NameType''
                                                       { nameTypeSignature = Name' (Sym "Int" :| []) (),
                                                         nameTypeName = Concrete' (Sym "a") (),
                                                         annNameType' = ()
                                                       }
                                                       :| [ NameType''
                                                              { nameTypeSignature =
                                                                  Name' (Sym "Int" :| []) (),
                                                                nameTypeName = Implicit' (Sym "b") (),
                                                                annNameType' = ()
                                                              }
                                                          ],
                                                   recordFamilySignature =
                                                     Just
                                                       ( Application'
                                                           ( App'
                                                               { applicationName = Name' (Sym "Foo" :| []) (),
                                                                 applicationArgs =
                                                                   Name' (Sym "Int" :| []) ()
                                                                     :| [ Parened'
                                                                            ( Infix'
                                                                                ( Inf'
                                                                                    { infixLeft = Name' (Sym "Fooy" :| []) (),
                                                                                      infixOp = Sym "->" :| [],
                                                                                      infixRight = Name' (Sym "Nada" :| []) (),
                                                                                      annInf = ()
                                                                                    }
                                                                                )
                                                                                ()
                                                                            )
                                                                            ()
                                                                        ],
                                                                 annApp = ()
                                                               }
                                                           )
                                                           ()
                                                       ),
                                                   annRecord'' = ()
                                                 }
                                             )
                                             ()
                                         ),
                                     annS = ()
                                   }
                               ]
                        )
                        (),
                    annNonArrowed = ()
                  },
              annTyp = ()
            }
        )
        ()
    ]

--------------------------------------------------
-- Arrow Testing
--------------------------------------------------

superArrowCase :: T.TestTree
superArrowCase =
  shouldParseAs
    "superArrowCase"
    (parse Parser.expression)
    "( b : Bah ->  c : B -o Foo) -> Foo a b -> a : Bah a c -o ( HAHAHHA -> foo )"
    ( Infix'
        ( Inf'
            { infixLeft =
                Parened'
                  ( Infix'
                      ( Inf'
                          { infixLeft = Name' (Sym "b" :| []) (),
                            infixOp = Sym ":" :| [],
                            infixRight =
                              Infix'
                                ( Inf'
                                    { infixLeft = Name' (Sym "Bah" :| []) (),
                                      infixOp = Sym "->" :| [],
                                      infixRight =
                                        Infix'
                                          ( Inf'
                                              { infixLeft = Name' (Sym "c" :| []) (),
                                                infixOp = Sym ":" :| [],
                                                infixRight =
                                                  Infix'
                                                    ( Inf'
                                                        { infixLeft = Name' (Sym "B" :| []) (),
                                                          infixOp = Sym "-o" :| [],
                                                          infixRight = Name' (Sym "Foo" :| []) (),
                                                          annInf = ()
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
                            annInf = ()
                          }
                      )
                      ()
                  )
                  (),
              infixOp = Sym "->" :| [],
              infixRight =
                Infix'
                  ( Inf'
                      { infixLeft =
                          Application'
                            ( App'
                                { applicationName = Name' (Sym "Foo" :| []) (),
                                  applicationArgs = Name' (Sym "a" :| []) () :| [Name' (Sym "b" :| []) ()],
                                  annApp = ()
                                }
                            )
                            (),
                        infixOp = Sym "->" :| [],
                        infixRight =
                          Infix'
                            ( Inf'
                                { infixLeft = Name' (Sym "a" :| []) (),
                                  infixOp = Sym ":" :| [],
                                  infixRight =
                                    Infix'
                                      ( Inf'
                                          { infixLeft = Application' (App' {applicationName = Name' (Sym "Bah" :| []) (), applicationArgs = Name' (Sym "a" :| []) () :| [Name' (Sym "c" :| []) ()], annApp = ()}) (),
                                            infixOp = Sym "-o" :| [],
                                            infixRight =
                                              Parened'
                                                ( Infix'
                                                    ( Inf'
                                                        { infixLeft = Name' (Sym "HAHAHHA" :| []) (),
                                                          infixOp = Sym "->" :| [],
                                                          infixRight = Name' (Sym "foo" :| []) (),
                                                          annInf = ()
                                                        }
                                                    )
                                                    ()
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
                        annInf = ()
                      }
                  )
                  (),
              annInf = ()
            }
        )
        ()
    )

--------------------------------------------------
-- alias tests
--------------------------------------------------

typeTest :: T.TestTree
typeTest =
  shouldParseAs
    "typeTest"
    Parser.parse
    "type Foo a b c d = | Foo nah bah sad"
    [ Type'
        ( Typ'
            { typeUsage = Nothing,
              typeName' = Sym "Foo",
              typeArgs = [Sym "a", Sym "b", Sym "c", Sym "d"],
              typeForm =
                NonArrowed'
                  { dataAdt =
                      Sum'
                        ( S'
                            { sumConstructor = Sym "Foo",
                              sumValue = Just (ADTLike' [Name' (Sym "nah" :| []) (), Name' (Sym "bah" :| []) (), Name' (Sym "sad" :| []) ()] ()),
                              annS = ()
                            }
                            :| []
                        )
                        (),
                    annNonArrowed = ()
                  },
              annTyp = ()
            }
        )
        ()
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
    [ Module'
        ( Mod'
            ( Like'
                { functionLikedName = Sym "Foo",
                  functionLikeArgs =
                    [ ConcreteA'
                        ( MatchLogic'
                            { matchLogicContents = MatchCon' (Sym "Int" :| []) [] (),
                              matchLogicNamed = Nothing,
                              annMatchLogic = ()
                            }
                        )
                        ()
                    ],
                  functionLikeBody =
                    Body'
                      ( Function'
                          ( Func'
                              (Like' {functionLikedName = Sym "T", functionLikeArgs = [], functionLikeBody = Body' (Name' (Sym "Int" :| [Sym "t"]) ()) (), annLike = ()})
                              ()
                          )
                          ()
                          :| [Signature' (Sig' {signatureName = Sym "bah", signatureUsage = Nothing, signatureArrowType = Infix' (Inf' {infixLeft = Name' (Sym "T" :| []) (), infixOp = Sym "->" :| [], infixRight = Name' (Sym "T" :| []) (), annInf = ()}) (), signatureConstraints = [], annSig = ()}) (), Function' (Func' (Like' {functionLikedName = Sym "bah", functionLikeArgs = [ConcreteA' (MatchLogic' {matchLogicContents = MatchName' (Sym "t") (), matchLogicNamed = Nothing, annMatchLogic = ()}) ()], functionLikeBody = Body' (OpenExpr' (OpenExpress' {moduleOpenExprModuleN = Sym "Int" :| [], moduleOpenExprExpr = Infix' (Inf' {infixLeft = Name' (Sym "t" :| []) (), infixOp = Sym "+" :| [], infixRight = Constant' (Number' (Integer'' 3 ()) ()) (), annInf = ()}) (), annOpenExpress = ()}) ()) (), annLike = ()}) ()) ()]
                      )
                      (),
                  annLike = ()
                }
            )
            ()
        )
        ()
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
    [ Module'
        ( Mod'
            ( Like'
                { functionLikedName = Sym "Bah",
                  functionLikeArgs = [ConcreteA' (MatchLogic' {matchLogicContents = MatchCon' (Sym "M" :| []) [] (), matchLogicNamed = Nothing, annMatchLogic = ()}) ()],
                  functionLikeBody =
                    Body'
                      ( ModuleOpen'
                          ( Open'
                              (Sym "M" :| [])
                              ()
                          )
                          ()
                          :| [Signature' (Sig' {signatureName = Sym "bah", signatureUsage = Nothing, signatureArrowType = Name' (Sym "Rec" :| []) (), signatureConstraints = [], annSig = ()}) (), Function' (Func' (Like' {functionLikedName = Sym "bah", functionLikeArgs = [ConcreteA' (MatchLogic' {matchLogicContents = MatchName' (Sym "t") (), matchLogicNamed = Nothing, annMatchLogic = ()}) ()], functionLikeBody = Body' (ExpRecord' (ExpressionRecord' {expRecordFields = NonPunned' (Sym "a" :| []) (Infix' (Inf' {infixLeft = Name' (Sym "t" :| []) (), infixOp = Sym "+" :| [], infixRight = Constant' (Number' (Integer'' 3 ()) ()) (), annInf = ()}) ()) () :| [NonPunned' (Sym "b" :| []) (Application' (App' {applicationName = Name' (Sym "expr" :| []) (), applicationArgs = Name' (Sym "M" :| [Sym "N", Sym "t"]) () :| [], annApp = ()}) ()) ()], annExpressionRecord = ()}) ()) (), annLike = ()}) ()) ()]
                      )
                      (),
                  annLike = ()
                }
            )
            ()
        )
        ()
    ]

--------------------------------------------------
-- typeName tests
--------------------------------------------------

typeNameNoUniverse :: T.TestTree
typeNameNoUniverse =
  shouldParseAs
    "typeNameNoUniverse"
    (parse Parser.expression)
    "Foo a b c (b -o d) a c u"
    ( Application'
        ( App'
            { applicationName = Name' (Sym "Foo" :| []) (),
              applicationArgs =
                Name'
                  (Sym "a" :| [])
                  ()
                  :| [ Name' (Sym "b" :| []) (),
                       Name' (Sym "c" :| []) (),
                       Parened'
                         ( Infix'
                             ( Inf'
                                 { infixLeft = Name' (Sym "b" :| []) (),
                                   infixOp = Sym "-o" :| [],
                                   infixRight = Name' (Sym "d" :| []) (),
                                   annInf = ()
                                 }
                             )
                             ()
                         )
                         (),
                       Name' (Sym "a" :| []) (),
                       Name' (Sym "c" :| []) (),
                       Name' (Sym "u" :| []) ()
                     ],
              annApp = ()
            }
        )
        ()
    )

--------------------------------------------------------------------------------
-- Match tests
--------------------------------------------------------------------------------

simpleNamedCon :: T.TestTree
simpleNamedCon =
  shouldParseAs
    "simpleNamedCon"
    (parse Parser.matchLogic)
    "foo@( Hi a b c )"
    ( MatchLogic'
        { matchLogicContents =
            MatchCon'
              (Sym "Hi" :| [])
              [ MatchLogic'
                  { matchLogicContents = MatchName' (Sym "a") (),
                    matchLogicNamed = Nothing,
                    annMatchLogic = ()
                  },
                MatchLogic'
                  { matchLogicContents = MatchName' (Sym "b") (),
                    matchLogicNamed = Nothing,
                    annMatchLogic = ()
                  },
                MatchLogic'
                  { matchLogicContents = MatchName' (Sym "c") (),
                    matchLogicNamed = Nothing,
                    annMatchLogic = ()
                  }
              ]
              (),
          matchLogicNamed = Just (Sym "foo"),
          annMatchLogic = ()
        }
    )

matchMoreComplex :: T.TestTree
matchMoreComplex =
  shouldParseAs
    "matchMoreComplex"
    (parse Parser.matchLogic)
    "foo@( Hi nah@{ a = nah , f } b 5 )"
    ( MatchLogic'
        { matchLogicContents =
            MatchCon'
              (Sym "Hi" :| [])
              [ MatchLogic'
                  { matchLogicContents =
                      MatchRecord'
                        ( NonPunned'
                            (Sym "a" :| [])
                            ( MatchLogic'
                                { matchLogicContents = MatchName' (Sym "nah") (),
                                  matchLogicNamed = Nothing,
                                  annMatchLogic = ()
                                }
                            )
                            ()
                            :| [Punned' (Sym "f" :| []) ()]
                        )
                        (),
                    matchLogicNamed = Just (Sym "nah"),
                    annMatchLogic = ()
                  },
                MatchLogic'
                  { matchLogicContents = MatchName' (Sym "b") (),
                    matchLogicNamed = Nothing,
                    annMatchLogic = ()
                  },
                MatchLogic'
                  { matchLogicContents =
                      MatchConst'
                        ( Number'
                            (Integer'' 5 ())
                            ()
                        )
                        (),
                    matchLogicNamed = Nothing,
                    annMatchLogic = ()
                  }
              ]
              (),
          matchLogicNamed = Just (Sym "foo"),
          annMatchLogic = ()
        }
    )

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

condTest1 :: T.TestTree
condTest1 =
  shouldParseAs
    "condTest1"
    (parse Parser.cond)
    ( ""
        <> "if  | foo  = a\n"
        <> "    | else = b "
    )
    ( C'
        ( CondExpression'
            { condLogicPred = Name' (Sym "foo" :| []) (),
              condLogicBody = Name' (Sym "a" :| []) (),
              annCondExpression = ()
            }
            :| [ CondExpression'
                   { condLogicPred = Name' (Sym "else" :| []) (),
                     condLogicBody = Name' (Sym "b" :| []) (),
                     annCondExpression = ()
                   }
               ]
        )
        ()
    )

--------------------------------------------------
-- Record
--------------------------------------------------

record1 :: T.TestTree
record1 =
  shouldParseAs
    "record1"
    (parse Parser.expression)
    "{a, b = 3+5}"
    ( ExpRecord'
        ( ExpressionRecord'
            { expRecordFields =
                Punned' (Sym "a" :| []) ()
                  :| [ NonPunned'
                         (Sym "b" :| [])
                         ( Infix'
                             ( Inf'
                                 { infixLeft = Constant' (Number' (Integer'' 3 ()) ()) (),
                                   infixOp = Sym "+" :| [],
                                   infixRight = Constant' (Number' (Integer'' 5 ()) ()) (),
                                   annInf = ()
                                 }
                             )
                             ()
                         )
                         ()
                     ],
              annExpressionRecord = ()
            }
        )
        ()
    )

--------------------------------------------------
-- parens
--------------------------------------------------

parens1 :: T.TestTree
parens1 =
  shouldParseAs
    "parens1"
    (parse Parser.expression)
    "(       ( (({a, b = 3+5}))))"
    ( Parened'
        ( Parened'
            ( Parened'
                ( Parened'
                    ( ExpRecord'
                        ( ExpressionRecord'
                            { expRecordFields =
                                Punned' (Sym "a" :| []) ()
                                  :| [ NonPunned'
                                         (Sym "b" :| [])
                                         ( Infix'
                                             ( Inf'
                                                 { infixLeft = Constant' (Number' (Integer'' 3 ()) ()) (),
                                                   infixOp = Sym "+" :| [],
                                                   infixRight = Constant' (Number' (Integer'' 5 ()) ()) (),
                                                   annInf = ()
                                                 }
                                             )
                                             ()
                                         )
                                         ()
                                     ],
                              annExpressionRecord = ()
                            }
                        )
                        ()
                    )
                    ()
                )
                ()
            )
            ()
        )
        ()
    )

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
