module Frontend.Desugar where

import qualified Data.Attoparsec.ByteString as Parsec
import qualified Juvix.Core.Common.NameSymbol as NameSym
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.FrontendDesugar as Desugar
import qualified Juvix.FrontendDesugar.RemoveDo.Types as AST
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Desugared
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

allDesugar :: T.TestTree
allDesugar =
  T.testGroup
    "desugar Tests"
    [guardTest]

shouldDesugar :: T.TestName -> ByteString -> [Desugared.TopLevel] -> T.TestTree
shouldDesugar name x y =
  T.testGroup
    "Desugar tests"
    [ T.testCase
        ("desugar: " <> name <> " " <> show x <> " should desugar to " <> show y)
        ( fmap Desugar.f (Parsec.parseOnly (many Parser.topLevelSN) x)
            T.@=? Right y
        )
    ]

guardTest :: T.TestTree
guardTest =
  shouldDesugar
    "guardTest"
    "let foo | x == 3 = 3 | else = 2"
    [ ( AST.MatchL
          { matchLPattern =
              AST.MatchCon (NameSym.fromSymbol "True") []
                |> flip AST.MatchLogic Nothing,
            matchLBody =
              AST.Constant (AST.Number (AST.Integer' 3))
          }
          :| [ ( AST.MatchL
                   { matchLPattern =
                       AST.MatchCon (NameSym.fromSymbol "True") []
                         |> flip AST.MatchLogic Nothing,
                     matchLBody =
                       AST.Constant (AST.Number (AST.Integer' 2))
                   }
                   :| []
               )
                 |> AST.Match'' (AST.Name (NameSym.fromSymbol "else"))
                 |> AST.Match
                 |> AST.MatchL
                   (AST.MatchLogic (AST.MatchCon (NameSym.fromSymbol "False") []) Nothing)
             ]
      )
        |> AST.Match''
          ( AST.Integer' 3
              |> AST.Number
              |> AST.Constant
              |> AST.Inf (AST.Name (NameSym.fromSymbol "x")) (NameSym.fromSymbol "==")
              |> AST.Infix
          )
        |> AST.Match
        |> AST.Like []
        |> (\x -> AST.Func "foo" (pure x) Nothing)
        |> AST.Function
    ]
