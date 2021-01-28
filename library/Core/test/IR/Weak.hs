-- | Tests that weak works as expected
module IR.Weak where

import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Types as IR
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as T

--------------------------------------------------------------------------------
-- Top Level Test
--------------------------------------------------------------------------------

top :: T.TestTree
top =
  T.testGroup
    "Weakening tests:"
    [ weakensFree,
      weaken1DoesNotEffect0,
      letsNonRecursive,
      weakOnlyShiftsFree,
      piBindsItself
    ]

data A = A deriving (Eq, Show)
instance Eval.HasWeak A where weakBy' _ _ A = A

--------------------------------------------------------------------------------
-- Generic Terms
--------------------------------------------------------------------------------
ident :: IR.Term a b
ident =
  IR.Lam (IR.Elim (IR.Bound 0))

dollarSign :: Natural -> IR.Term a b
dollarSign x =
  IR.Lam (IR.Elim (IR.App (IR.Bound 0) (IR.Elim (IR.Bound x))))

freeVal :: Natural -> IR.Term a b
freeVal x =
  IR.Bound x
    |> IR.Elim

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

weakensFree :: T.TestTree
weakensFree =
  (\x -> ((Eval.weakBy x (freeVal 0) :: IR.Term A A) T.=== freeVal x))
    |> forAllNats
    |> T.testProperty "Promoting a bound at 0 by x is the same as having bound x"

weaken1DoesNotEffect0 :: T.TestTree
weaken1DoesNotEffect0 =
  let t :: IR.Term A A
      t = freeVal 0
      f x y =
        Eval.weakBy' x (succ y) t T.=== t
   in forAllNats (forAllNats . f)
        |> T.testProperty "promoting terms greater than 0 does not change the value"

letsNonRecursive :: T.TestTree
letsNonRecursive =
  let body = IR.Elim (IR.Bound 0)
      bound = IR.Bound 0
      --
      t :: IR.Term A A
      t = IR.Let one bound body
      --
      relation x =
        Eval.weakBy x t
          T.=== IR.Let one (Eval.weakBy x bound) (Eval.weakBy' x 1 body)
   in forAllNats relation
        |> T.testProperty "lets are non recursive, and bind in the body"

weakOnlyShiftsFree :: T.TestTree
weakOnlyShiftsFree =
  let t :: IR.Term A A
      t = dollarSign 1
   in (\x -> Eval.weakBy x t T.=== dollarSign (succ x))
        |> forAllNats
        |> T.testProperty "weakby only weakens the free variables"

piBindsItself :: T.TestTree
piBindsItself =
  let body = IR.Elim (IR.Bound 0)
      --
      t :: IR.Term A A
      t = IR.Pi one (freeVal 0) body
      --
      relation x =
        let IR.Pi _ _ newBody = Eval.weakBy x t
         in newBody T.=== Eval.weakBy' x 1 body
   in forAllNats relation
        |> T.testProperty "pi binds itself in the body"

--------------------------------------------------------------------------------
-- property Helpers
--------------------------------------------------------------------------------
forAllNats :: (Show a, Integral a, T.Testable prop) => (a -> prop) -> T.Property
forAllNats =
  T.forAll T.arbitrarySizedNatural
