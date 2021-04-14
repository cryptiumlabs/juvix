module Sexp (top) where

import qualified Data.Set as Set
import Juvix.Library
import Juvix.Library.Sexp as Sexp (Atom)
import qualified Juvix.Library.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (error)

top :: T.TestTree
top =
  T.testGroup
    "sexp pass tests:"
    [ lastWorksAsExpected,
      foldrWorksAsExpted,
      foldPredWorksAsExpted,
      listWorksAsExpected,
      listStarWorksAsExpected
    ]

--------------------------------------------------------------------------------
-- Tests not exported
--------------------------------------------------------------------------------

lastWorksAsExpected :: T.TestTree
lastWorksAsExpected =
  T.testGroup
    "last works as expected"
    [ T.testCase
        "last on Cons"
        (fmap Sexp.last (Sexp.parse "(1 2 3 4)") T.@=? Sexp.parse "4"),
      T.testCase
        "last on atom"
        (fmap Sexp.last (Sexp.parse "4") T.@=? Sexp.parse "4"),
      T.testCase
        "last on Nil"
        (fmap Sexp.last (Sexp.parse "()") T.@=? Sexp.parse "()")
    ]

foldrWorksAsExpted :: T.TestTree
foldrWorksAsExpted =
  T.testGroup
    "foldr works as epxected"
    [ T.testCase
        "foldr on (1 2 3 4 5) properly adds"
        (Sexp.foldr (\(Sexp.Atom (Sexp.N n Nothing)) acc -> n + acc) 0 ns T.@=? 15)
    ]
  where
    Right ns = Sexp.parse "(1 2 3 4 5)"

foldPredWorksAsExpted :: T.TestTree
foldPredWorksAsExpted =
  T.testGroup
    "foldPred works as Exptected"
    [ T.testCase
        "foldPred properly recurses"
        ( Sexp.foldPred nest (== "if") (\_ sexp -> Sexp.Cons (Sexp.atom ":if") sexp)
            T.@=? expectedNest
        )
    ]
  where
    Right nest =
      Sexp.parse "(if x y (if z l))"
    Right expectedNest =
      Sexp.parse "(:if x y (:if z l))"

listWorksAsExpected :: T.TestTree
listWorksAsExpected =
  T.testGroup
    "list works as epctected"
    [ T.testCase
        "list on a term is correct"
        (Sexp.parse "(1 2 3)" T.@=? Right manualList)
    ]
  where
    manualList =
      Sexp.list [Sexp.number 1, Sexp.number 2, Sexp.number 3]

listStarWorksAsExpected :: T.TestTree
listStarWorksAsExpected =
  T.testGroup
    "list* works as epctected"
    [ T.testCase
        "list* on a list properly removes the last list"
        (Sexp.parse "(1 2 3)" T.@=? Right manualList)
    ]
  where
    manualList =
      Sexp.listStar [Sexp.number 1, Sexp.number 2, Sexp.list [Sexp.number 3]]
