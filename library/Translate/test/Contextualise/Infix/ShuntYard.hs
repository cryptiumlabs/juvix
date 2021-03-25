module Contextualise.Infix.ShuntYard where

import qualified Juvix.Contextify.InfixPrecedence.ShuntYard as Shunt
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

times, div, add, carrot, carrotL, equality :: Shunt.PredOrEle Symbol a
div = Shunt.Precedence (Shunt.Pred "/" Shunt.Left' 7)
add = Shunt.Precedence (Shunt.Pred "+" Shunt.Left' 6)
times = Shunt.Precedence (Shunt.Pred "*" Shunt.Left' 7)
carrot = Shunt.Precedence (Shunt.Pred "^" Shunt.Right' 8)
carrotL = Shunt.Precedence (Shunt.Pred "^l" Shunt.Left' 8)
equality = Shunt.Precedence (Shunt.Pred "==" Shunt.NonAssoc 4)

allInfixTests :: T.TestTree
allInfixTests =
  T.testGroup
    "Infix Tests"
    [ infixlTest,
      infixrTest,
      mixFailTest,
      nonAssocFailTest
    ]

infixlTest :: T.TestTree
infixlTest =
  Shunt.Single 5
    |> Shunt.App "*" (Shunt.Single 4)
    |> flip (Shunt.App "*") (Shunt.Single 6)
    |> Shunt.App "+" (Shunt.Single 3)
    |> (T.@=? app)
    |> T.testCase "test infixl: 3 + 4 * 5 * 6 ≡ 3 + ((4 * 5) * 6)"
  where
    app :: Shunt.Application Symbol Integer
    Right app =
      Shunt.shunt $
        Shunt.Ele 3
          :| [add, Shunt.Ele 4, times, Shunt.Ele 5, times, Shunt.Ele 6]

infixrTest :: T.TestTree
infixrTest =
  Shunt.Single 6
    |> Shunt.App "^" (Shunt.Single 5)
    |> Shunt.App "^" (Shunt.Single 4)
    |> Shunt.App "+" (Shunt.Single 3)
    |> (T.@=? app)
    |> T.testCase "test infixr: 3 + 4 ^ 5 ^ 6 ≡ 3 + (4 ^ (5 ^ 6))"
  where
    app :: Shunt.Application Symbol Integer
    Right app =
      Shunt.shunt $
        Shunt.Ele 3
          :| [add, Shunt.Ele 4, carrot, Shunt.Ele 5, carrot, Shunt.Ele 6]

mixFailTest :: T.TestTree
mixFailTest =
  Shunt.Pred "^" Shunt.Right' 8
    |> Shunt.Clash (Shunt.Pred "^l" Shunt.Left' 8)
    |> Left
    |> (T.@=? app)
    |> T.testCase "test infixFail: 3 + 4 ^ 5 ^l 6 ≡ Error l ^l: mixing precedents"
  where
    app :: Either (Shunt.Error Symbol) (Shunt.Application Symbol Integer)
    app =
      Shunt.shunt $
        Shunt.Ele 3
          :| [add, Shunt.Ele 4, carrot, Shunt.Ele 5, carrotL, Shunt.Ele 6]

nonAssocFailTest :: T.TestTree
nonAssocFailTest =
  Shunt.Pred "==" Shunt.NonAssoc 4
    |> Shunt.Clash (Shunt.Pred "==" Shunt.NonAssoc 4)
    |> Left
    |> (T.@=? app)
    |> T.testCase ("test infixr: 3 + 4 == 5 == 6 ≡ Error: making non assocs assoc")
  where
    app :: Either (Shunt.Error Symbol) (Shunt.Application Symbol Integer)
    app =
      Shunt.shunt $
        Shunt.Ele 3
          :| [add, Shunt.Ele 4, equality, Shunt.Ele 5, equality, Shunt.Ele 6]
