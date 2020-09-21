module Frontend.Desugar where

import Data.Attoparsec.ByteString (parseOnly)
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Frontend.Types.Base
import Juvix.FrontendDesugar (desugar)
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Desugared
import Juvix.Library
  ( (<>),
    ByteString,
    Either (Right),
    Maybe (..),
    NonEmpty (..),
    fmap,
    many,
    show,
  )
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

allDesugar :: T.TestTree
allDesugar =
  T.testGroup
    "desugar Tests"
    [guardTest]

shouldDesugar ::
  T.TestName -> ByteString -> [Desugared.TopLevel] -> T.TestTree
shouldDesugar name x y =
  T.testGroup
    "Desugar tests"
    [ T.testCase
        ("desugar: " <> name <> " " <> show x <> " should desugar to " <> show y)
        (fmap desugar (parseOnly (many Parser.topLevelSN) x) T.@=? Right y)
    ]

guardTest :: T.TestTree
guardTest =
  shouldDesugar
    "guardTest"
    "let foo | x == 3 = 3 | else = 2"
    [ Function'
        ( FunctionX
            ( "foo",
              FunctionLikeX
                { extFunctionLike =
                    ( [],
                      Match'
                        ( Match'''
                            { matchOn =
                                Infix'
                                  ( Inf'
                                      { infixLeft = Name' ("x" :| []) (),
                                        infixOp = "==" :| [],
                                        infixRight = Constant' (Number' (Integer'' 3 ()) ()) (),
                                        annInf = ()
                                      }
                                  )
                                  (),
                              matchBindigns =
                                MatchL'
                                  { matchLPattern =
                                      MatchLogic'
                                        { matchLogicContents = MatchCon' ("True" :| []) [] (),
                                          matchLogicNamed = Nothing,
                                          annMatchLogic = ()
                                        },
                                    matchLBody = Constant' (Number' (Integer'' 3 ()) ()) (),
                                    annMatchL = ()
                                  }
                                  :| [ MatchL'
                                         { matchLPattern =
                                             MatchLogic'
                                               { matchLogicContents = MatchCon' ("False" :| []) [] (),
                                                 matchLogicNamed = Nothing,
                                                 annMatchLogic = ()
                                               },
                                           matchLBody =
                                             Match'
                                               ( Match'''
                                                   { matchOn = Name' ("else" :| []) (),
                                                     matchBindigns = MatchL' {matchLPattern = MatchLogic' {matchLogicContents = MatchCon' ("True" :| []) [] (), matchLogicNamed = Nothing, annMatchLogic = ()}, matchLBody = Constant' (Number' (Integer'' 2 ()) ()) (), annMatchL = ()} :| [MatchL' {matchLPattern = MatchLogic' {matchLogicContents = MatchCon' ("False" :| []) [] (), matchLogicNamed = Nothing, annMatchLogic = ()}, matchLBody = Match' (Match''' {matchOn = Name' ("else" :| []) (), matchBindigns = MatchL' {matchLPattern = MatchLogic' {matchLogicContents = MatchCon' ("True" :| []) [] (), matchLogicNamed = Nothing, annMatchLogic = ()}, matchLBody = Constant' (Number' (Integer'' 2 ()) ()) (), annMatchL = ()} :| [], annMatch'' = ()}) (), annMatchL = ()}],
                                                     annMatch'' = ()
                                                   }
                                               )
                                               (),
                                           annMatchL = ()
                                         }
                                     ],
                              annMatch'' = ()
                            }
                        )
                        ()
                    )
                }
                :| [],
              Nothing
            )
        )
        ()
    ]
