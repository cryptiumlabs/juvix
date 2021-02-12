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
    [condWorksAsExpected, ifWorksAsExpected]

condTransform :: Sexp.T -> Sexp.T
condTransform xs = Sexp.foldPred xs (== "cond") condToIf
  where
    condToIf atom cdr =
      let acc =
            generation (Sexp.last cdr) Sexp.Nil
              |> Sexp.butLast
       in Sexp.foldr generation acc (Sexp.butLast cdr)
            |> Sexp.addMetaToCar atom
    --
    generation (Sexp.Cons condition body) acc =
      Sexp.list [Sexp.atom "if", condition, (Sexp.car body), acc]
    generation _ _ =
      error "malformed cond"

ifTransform :: Sexp.T -> Sexp.T
ifTransform xs = Sexp.foldPred xs (== "if") ifToCase
  where
    ifToCase atom cdr =
      case cdr of
        Sexp.Cons pred (Sexp.Cons then' (Sexp.Cons else' Sexp.Nil)) ->
          Sexp.list (caseListElse pred then' else')
        Sexp.Cons pred (Sexp.Cons then' Sexp.Nil) ->
          Sexp.list (caseList pred then')
        _ ->
          error "malformed if"
            |> Sexp.addMetaToCar atom
    caseList pred then' =
      [Sexp.atom "case", pred, Sexp.list [Sexp.atom "true", then']]
    caseListElse pred then' else' =
      caseList pred then' <> [Sexp.list [Sexp.atom "false", else']]

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
      \ (\"if\" \"else\" \"false\")))"

ifWorksAsExpected :: T.TestTree
ifWorksAsExpected =
  T.testCase
    "if expansion to match works properly"
    (expected T.@=? show (ifTransform (condTransform testData)))
  where
    expected :: String
    expected =
      "(\"case\" (\"g\" \"x\")\
      \ (\"true\" \"true\")\
      \ (\"false\"\
      \ (\"case\" (\"p\" \"x\")\
      \ (\"true\" \"true\")\
      \ (\"false\" (\"case\" \"else\" (\"true\" \"false\"))))))"

-- TODO ∷ add a sexp parser, to make this less annoying
testData :: Sexp.T
testData =
  Sexp.list
    [ Sexp.Atom $ Sexp.A "cond" (Just (LineNum.T 2 3)),
      Sexp.list
        [ Sexp.list
            [Sexp.atom "g", Sexp.atom "x"],
          Sexp.atom "true"
        ],
      Sexp.list
        [ Sexp.list
            [Sexp.atom "p", Sexp.atom "x"],
          Sexp.atom "true"
        ],
      Sexp.list
        [Sexp.atom "else", Sexp.atom "false"]
    ]
