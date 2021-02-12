module Sexp where

import Juvix.Library
import qualified Juvix.Library.LineNum as LineNum
import qualified Juvix.Library.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (String, error)

top :: T.TestTree
top =
  T.testGroup
    "sexp tests:"
    [condWorksAsExpected]

condTransform :: Sexp.T -> Sexp.T
condTransform xs = Sexp.foldPred xs (== "cond") condToIf
  where
    condToIf atom cdr =
      Sexp.foldr
        ( \name acc ->
            case name of
              Sexp.Cons condition body ->
                generateIf condition (Sexp.car body) acc
              _ ->
                error "malformed cond"
        )
        Sexp.Nil
        cdr
        |> Sexp.addMetaToCar atom
    generateIf p t e =
      Sexp.ofList [Sexp.Atom (Sexp.A "if" Nothing), p, t, e]

condWorksAsExpected :: T.TestTree
condWorksAsExpected =
  T.testCase
    "cond properly desguars cond"
    (expected T.@=? show (condTransform testData))
  where
    expected :: String
    expected =
      "(\"if\" (\"g\" \"x\")\
      \ \"true\"\
      \ (\"if\" (\"p\" \"x\")\
      \ \"true\"\
      \ (\"if\" \"else\" \"false\" ))))"

-- TODO ∷ add a sexp parser, to make this less annoying
testData :: Sexp.T
testData =
  Sexp.ofList
    [ Sexp.Atom $ Sexp.A "cond" (Just (LineNum.T 2 3)),
      Sexp.ofList
        [ Sexp.ofList
            [ Sexp.Atom $ Sexp.A "g" Nothing,
              Sexp.Atom $ Sexp.A "x" Nothing
            ],
          Sexp.Atom $ Sexp.A "true" Nothing
        ],
      Sexp.ofList
        [ Sexp.ofList
            [ Sexp.Atom $ Sexp.A "p" Nothing,
              Sexp.Atom $ Sexp.A "x" Nothing
            ],
          Sexp.Atom $ Sexp.A "true" Nothing
        ],
      Sexp.ofList
        [ Sexp.Atom $ Sexp.A "else" Nothing,
          Sexp.Atom $ Sexp.A "false" Nothing
        ]
    ]
