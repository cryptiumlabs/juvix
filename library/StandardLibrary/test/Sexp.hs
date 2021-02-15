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
      Sexp.list [Sexp.atom "if", condition, Sexp.car body, acc]
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

-- This one and sig combining are odd mans out, as they happen on a
-- list of transforms
multipleTransDefun :: [Sexp.T] -> [Sexp.T]
multipleTransDefun = search
  where
    combineMultiple name xs =
      Sexp.list ([Sexp.atom "defun-match", Sexp.atom name] <> (Sexp.cdr . Sexp.cdr <$> xs))
    sameName name (Sexp.Cons defun1 (Sexp.Cons name1 _))
      | Sexp.isAtomNamed defun1 "defun" && Sexp.isAtomNamed name1 name =
        True
    sameName _ _ =
      False
    grabSimilar _nam [] = ([], [])
    grabSimilar name (defn : xs)
      | sameName name defn =
        let (same, rest) = grabSimilar name xs
         in (defn : same, rest)
      | otherwise =
        ([], defn : xs)
    search (defun@(Sexp.Cons defun1 (Sexp.Cons name1@(Sexp.Atom a) _)) : xs)
      | Sexp.isAtomNamed defun1 "defun",
        Just name <- Sexp.nameFromT name1 =
        let (sameDefun, toSearch) = grabSimilar name xs
         in combineMultiple name (defun : sameDefun)
              |> Sexp.addMetaToCar a
              |> (: search toSearch)
    search (x : xs) = x : search xs
    search [] = []

multipleTransLet :: Sexp.T -> Sexp.T
multipleTransLet xs = Sexp.foldPred xs (== "let") letToLetMatch
  where
    letToLetMatch atom cdr = undefined

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

testDefun :: [Sexp.T]
testDefun =
  [ Sexp.list
      [ Sexp.atom "defun",
        Sexp.atom "f",
        Sexp.list
          [Sexp.list [Sexp.atom "Cons", Sexp.atom "a", Sexp.atom "as"], Sexp.atom "b"],
        Sexp.atom "b1"
      ],
    Sexp.list
      [ Sexp.atom "defun",
        Sexp.atom "f",
        Sexp.list [Sexp.atom "Nil", Sexp.atom "b"],
        Sexp.atom "b2"
      ],
    Sexp.list
      [ Sexp.atom "defun",
        Sexp.atom "g",
        Sexp.list [Sexp.atom "a", Sexp.atom "b"],
        Sexp.atom "new-b"
      ],
    Sexp.list
      [ Sexp.atom "sig",
        Sexp.atom "g"
      ]
  ]
