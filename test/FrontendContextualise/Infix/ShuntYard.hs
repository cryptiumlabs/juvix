module FrontendContextualise.Infix.ShuntYard where

import qualified Juvix.FrontendContextualise.InfixPrecedence.ShuntYard as Shunt
import Juvix.Library
  ( ($),
    Either (Right),
    Integer,
    NonEmpty ((:|)),
    Symbol,
    show,
    (|>),
  )
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (String)

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
  ( "App \"+\" (Single 3) (App \"*\" (App \"*\" (Single 4) (Single 5)) (Single 6))"
      T.@=? (show app :: String)
  )
    |> T.testCase ("test infixl: 3 + 4 * 5 * 6 ≡ 3 + ((4 * 5) * 6)")
  where
    app :: Shunt.Application Symbol Integer
    Right app =
      Shunt.shunt $
        Shunt.Ele 3
          :| [add, Shunt.Ele 4, times, Shunt.Ele 5, times, Shunt.Ele 6]

infixrTest :: T.TestTree
infixrTest =
  ( "App \"+\" (Single 3) (App \"^\" (Single 4) (App \"^\" (Single 5) (Single 6)))"
      T.@=? (show app :: String)
  )
    |> T.testCase ("test infixr: 3 + 4 ^ 5 ^ 6 ≡ 3 + (4 ^ (5 ^ 6))")
  where
    app :: Shunt.Application Symbol Integer
    Right app =
      Shunt.shunt $
        Shunt.Ele 3
          :| [add, Shunt.Ele 4, carrot, Shunt.Ele 5, carrot, Shunt.Ele 6]

mixFailTest :: T.TestTree
mixFailTest =
  ( "Left (Clash (Pred \"^l\" Left' 8) (Pred \"^\" Right' 8))"
      T.@=? (show app :: String)
  )
    |> T.testCase ("test infixFail: 3 + 4 ^ 5 ^l 6 ≡ Error l ^l: mixing precedents")
  where
    app :: Either (Shunt.Error Symbol) (Shunt.Application Symbol Integer)
    app =
      Shunt.shunt $
        Shunt.Ele 3
          :| [add, Shunt.Ele 4, carrot, Shunt.Ele 5, carrotL, Shunt.Ele 6]

nonAssocFailTest :: T.TestTree
nonAssocFailTest =
  ( "Left (Clash (Pred \"==\" NonAssoc 4) (Pred \"==\" NonAssoc 4))"
      T.@=? (show app :: String)
  )
    |> T.testCase ("test infixr: 3 + 4 == 5 == 6 ≡ Error: making non assocs assoc")
  where
    app :: Either (Shunt.Error Symbol) (Shunt.Application Symbol Integer)
    app =
      Shunt.shunt $
        Shunt.Ele 3
          :| [add, Shunt.Ele 4, equality, Shunt.Ele 5, equality, Shunt.Ele 6]
