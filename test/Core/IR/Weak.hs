-- | Tests that weak works as expected
module Core.IR.Weak where

import Juvix.Library
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Types as IR
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Test.Tasty.QuickCheck as T

--------------------------------------------------------------------------------
-- Top Level Test
--------------------------------------------------------------------------------

top :: T.TestTree
top =
  T.testGroup
    "Weakening tests:"
    [ weakensFree,
      weaken1DoesNotEffect0
    ]

--------------------------------------------------------------------------------
-- Generic Terms
--------------------------------------------------------------------------------
ident :: IR.Term a b
ident =
  IR.Lam (IR.Elim (IR.Bound 0))


freeVal :: Natural -> IR.Term a b
freeVal x =
  IR.Bound x
    |> IR.Elim


--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

weakensFree :: T.TestTree
weakensFree =
  (\x -> ((Eval.weakBy x (freeVal 0) :: IR.Term Int Int) T.=== freeVal x))
    |> forAllNats
    |> T.testProperty "Promoting a bound at 0 by x is the same as having bound x"


weaken1DoesNotEffect0 :: T.TestTree
weaken1DoesNotEffect0 =
  let t :: IR.Term Int Int
      t = freeVal 0
      f x y =
        Eval.weakBy' x (succ y) t T.=== t
  in
      forAllNats (forAllNats . f)
      |> T.testProperty "promoting terms greater than 0 does not change the value"


--------------------------------------------------------------------------------
-- property Helpers
--------------------------------------------------------------------------------
forAllNats :: (Show a, Integral a, T.Testable prop) => (a -> prop) -> T.Property
forAllNats =
  T.forAll T.arbitrarySizedNatural
