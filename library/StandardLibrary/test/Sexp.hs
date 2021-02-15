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
    [ condWorksAsExpected,
      ifWorksAsExpected,
      letWorksAsExpected,
      doWorksAsExpected,
      recordWorksAsExpected
    ]

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
        Sexp.List [pred, then', else'] ->
          Sexp.list (caseListElse pred then' else')
        Sexp.List [pred, then'] ->
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
    sameName name (Sexp.List (defun1 : name1 : _))
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
    search (defun@(Sexp.List (defun1 : name1@(Sexp.Atom a) : _)) : xs)
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
    letToLetMatch atom (Sexp.List [a@(Sexp.Atom (Sexp.A name _)), bindingsBody, rest]) =
      let (grabbed, notMatched) = grabSimilar name rest
       in Sexp.list
            [ Sexp.atom "let-match",
              a,
              putTogetherSplices (bindingsBody : grabbed),
              notMatched
            ]
            |> Sexp.addMetaToCar atom
    letToLetMatch _atom _ =
      error "malformed let"
    --
    grabSimilar name (Sexp.List [let1, name1, bindingsBody, rest])
      | Sexp.isAtomNamed let1 "let" && Sexp.isAtomNamed name1 name =
        grabSimilar name rest
          |> first (bindingsBody :)
    grabSimilar _name xs = ([], xs)
    --
    putTogetherSplices =
      foldr spliceBindingBody Sexp.Nil
    --
    spliceBindingBody (Sexp.List [bindings, body]) acc =
      Sexp.Cons bindings (Sexp.Cons body acc)
    spliceBindingBody _ _ =
      error "doesn't happen"

translateDo :: Sexp.T -> Sexp.T
translateDo xs = Sexp.foldPred xs (== "do") doToBind
  where
    doToBind atom sexp =
      Sexp.foldr generation acc (Sexp.butLast sexp)
        |> Sexp.addMetaToCar atom
      where
        acc =
          case Sexp.last sexp of
            -- toss away last %<-... we should likely throw a warning for this
            Sexp.List [Sexp.Atom (Sexp.A "%<-" _), _name, body] -> body
            xs -> xs
        generation body acc =
          case body of
            Sexp.List [Sexp.Atom (Sexp.A "%<-" _), name, body] ->
              Sexp.list
                [ Sexp.atom "Prelude.>>=",
                  body,
                  Sexp.list [Sexp.atom "lambda", Sexp.list [name], acc]
                ]
            notBinding ->
              Sexp.list [Sexp.atom "Prelude.>>", notBinding, acc]

removePunnedRecords :: Sexp.T -> Sexp.T
removePunnedRecords xs = Sexp.foldPred xs (== "record") removePunned
  where
    removePunned atom sexp =
      Sexp.listStar
        [ Sexp.atom "record-no-pun",
          Sexp.foldr f Sexp.Nil sexp
        ]
        |> Sexp.addMetaToCar atom
      where
        f (Sexp.List [field, bind]) acc =
          field Sexp.:> bind Sexp.:> acc
        f (Sexp.List [pun]) acc =
          pun Sexp.:> pun Sexp.:> acc
        f _ _ = error "malformed record"

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

letWorksAsExpected :: T.TestTree
letWorksAsExpected =
  T.testCase
    "let expansion to match works properly"
    (expected T.@=? show (multipleTransLet testLet))
  where
    expected :: String
    expected =
      "(\"let-match\" \"foo\" ((\"Nil\" \"b\") \"body-1\"\
      \ ((\"Cons\" \"a\" \"xs\") \"b\") \"body-2\")\
      \ (\"let-match\" \"bar\" (((\"Cons\" \"a\" \"xs\") \"b\") \"body-2\")\
      \ \"&rest\"))"

doWorksAsExpected :: T.TestTree
doWorksAsExpected =
  T.testCase
    "do expansion works propelry"
    (expected T.@=? show (translateDo testDo))
  where
    expected :: String
    expected =
      "(\"Prelude.>>=\" \"computation\"\
      \ (\"lambda\" (\"x\") (\"Prelude.>>\" \"computation\"\
      \ (\"Prelude.>>=\" \"more-comp\"\
      \ (\"lambda\" (\"y\") (\"Prelude.return\" (\"+\" \"x\" \"y\")))))))"

recordWorksAsExpected :: T.TestTree
recordWorksAsExpected =
  T.testCase
    "removing punned names works as expected"
    (expected T.@=? show (removePunnedRecords testRecord))
  where
    expected :: String
    expected =
      "(\"record-no-pun\"\
      \ \"name\" \"value\"\
      \ \"field-pun\" \"field-pun\"\
      \ \"name2\" \"value2\")"

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

testLet :: Sexp.T
testLet =
  Sexp.list
    [ Sexp.atom "let",
      Sexp.atom "foo",
      Sexp.list
        [Sexp.list [Sexp.atom "Nil", Sexp.atom "b"], Sexp.atom "body-1"],
      Sexp.list
        [ Sexp.atom "let",
          Sexp.atom "foo",
          Sexp.list
            [ Sexp.list
                [ Sexp.list [Sexp.atom "Cons", Sexp.atom "a", Sexp.atom "xs"],
                  Sexp.atom "b"
                ],
              Sexp.atom "body-2"
            ],
          Sexp.list
            [ Sexp.atom "let",
              Sexp.atom "bar",
              Sexp.list
                [ Sexp.list
                    [ Sexp.list [Sexp.atom "Cons", Sexp.atom "a", Sexp.atom "xs"],
                      Sexp.atom "b"
                    ],
                  Sexp.atom "body-2"
                ],
              Sexp.atom "&rest"
            ]
        ]
    ]

testDo :: Sexp.T
testDo =
  Sexp.list
    [ Sexp.atom "do",
      Sexp.list [Sexp.atom "%<-", Sexp.atom "x", Sexp.atom "computation"],
      Sexp.atom "computation",
      Sexp.list [Sexp.atom "%<-", Sexp.atom "y", Sexp.atom "more-comp"],
      Sexp.list [Sexp.atom "Prelude.return", Sexp.list [Sexp.atom "+", Sexp.atom "x", Sexp.atom "y"]]
    ]

testRecord :: Sexp.T
testRecord =
  Sexp.list
    [ Sexp.atom "record",
      Sexp.list [Sexp.atom "name", Sexp.atom "value"],
      Sexp.list [Sexp.atom "field-pun"],
      Sexp.list [Sexp.atom "name2", Sexp.atom "value2"]
    ]
