module FrontendContextualise.Infix.ShuntYard where

import qualified Juvix.FrontendContextualise.InfixPrecedence.ShuntYard as Shunt
import Juvix.Library hiding ((^^))
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (String)

times, div, add, carrot, carrotL :: Shunt.PredOrEle a
div = Shunt.Precedence (Shunt.Pred "/" Shunt.Left 7)
add = Shunt.Precedence (Shunt.Pred "+" Shunt.Left 6)
times = Shunt.Precedence (Shunt.Pred "*" Shunt.Left 7)
carrot = Shunt.Precedence (Shunt.Pred "^" Shunt.Right 8)
carrotL = Shunt.Precedence (Shunt.Pred "^l" Shunt.Left 8)

allInfixTests :: T.TestTree
allInfixTests =
  T.testGroup
    "Infix Tests"
    [infixlTest]

infixlTest :: T.TestTree
infixlTest =
  ( "App + (Single 3) (App * (App * (Single 4) (Single 5)) (Single 6))"
      T.@=? (show app :: String)
  )
    |> T.testCase ("test infixl: 3 + 4 * 5 * 6 ≡ 3 + ((4 * 5) * 6)")
  where
    app :: Shunt.Application Integer
    app =
      Shunt.shunt $
        Shunt.Ele 3
          :| [add, Shunt.Ele 4, times, Shunt.Ele 5, times, Shunt.Ele 6]

(^^) = (^)

infixl 8 ^^

(&&&) = (&&)

infixl 4 &&&
