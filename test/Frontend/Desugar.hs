module Frontend.Desugar where

import Data.Attoparsec.ByteString (parse)
import Frontend.Parser (shouldParseAs)
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Frontend.Types (TopLevel)
import Juvix.Frontend.Types.Base
  ( Cond' (C'),
    CondLogic'
      ( CondExpression',
        annCondExpression,
        condLogicBody,
        condLogicPred
      ),
    Constant' (Number'),
    Expression' (Constant', Infix', Name'),
    Function' (Func'),
    FunctionLike'
      ( Like',
        annLike,
        functionLikeArgs,
        functionLikeBody,
        functionLikedName
      ),
    GuardBody' (Guard'),
    Infix' (Inf', annInf, infixLeft, infixOp, infixRight),
    Numb' (Integer''),
    TopLevel' (Function'),
  )
import Juvix.Library
  ( Alternative (many),
    ByteString,
    NonEmpty ((:|)),
    Symbol (Sym),
  )
import qualified Test.Tasty as T

allDesugar :: T.TestTree
allDesugar =
  T.testGroup
    "desugar Tests"
    [guardTest]

shouldDesugar ::
  T.TestName -> ByteString -> [TopLevel] -> T.TestTree
shouldDesugar name =
  shouldParseAs name (parse (many Parser.topLevelSN))

guardTest :: T.TestTree
guardTest =
  shouldDesugar
    "guardTest"
    "let foo | x == 3 = 3 | else = 2"
    [ Function'
        ( Func'
            ( Like'
                { functionLikedName = Sym "foo",
                  functionLikeArgs = [],
                  functionLikeBody =
                    Guard'
                      ( C'
                          ( CondExpression'
                              { condLogicPred =
                                  Infix'
                                    ( Inf'
                                        { infixLeft = Name' (Sym "x" :| []) (),
                                          infixOp = Sym "==" :| [],
                                          infixRight =
                                            Constant'
                                              (Number' (Integer'' 3 ()) ())
                                              (),
                                          annInf = ()
                                        }
                                    )
                                    (),
                                condLogicBody = Constant' (Number' (Integer'' 3 ()) ()) (),
                                annCondExpression = ()
                              }
                              :| [ CondExpression'
                                     { condLogicPred = Name' (Sym "else" :| []) (),
                                       condLogicBody =
                                         Constant'
                                           ( Number'
                                               (Integer'' 2 ())
                                               ()
                                           )
                                           (),
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
