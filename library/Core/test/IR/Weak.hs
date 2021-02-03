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
    "Weakening tests"
    [ weakensFree,
      weaken1DoesNotEffect0,
      letsNonRecursive,
      weakOnlyShiftsFree,
      piBindsItself
    ]

data A = A deriving (Eq, Show)

instance Eval.HasWeak A where weakBy' _ _ A = A

type ATerm = IR.Term A A

type AElim = IR.Elim A A

--------------------------------------------------------------------------------
-- Generic Terms
--------------------------------------------------------------------------------
ident :: ATerm
ident =
  IR.Lam (IR.Elim (IR.Bound 0))

dollarSign :: Natural -> ATerm
dollarSign x =
  IR.Lam (IR.Elim (IR.App (IR.Bound 0) (IR.Elim (IR.Bound x))))

boundVar :: Natural -> ATerm
boundVar x =
  IR.Bound x
    |> IR.Elim

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

weakensFree :: T.TestTree
weakensFree =
  T.testProperty "weakBy b 0 = b" $
    forAllNats \b -> Eval.weakBy b (boundVar 0) T.=== boundVar b

weaken1DoesNotEffect0 :: T.TestTree
weaken1DoesNotEffect0 =
  T.testProperty "x < i ==> weak' i x = x" $
    forAllNats \b -> forAllNats \i -> forAllNats \x -> test b i x
  where
    test b i x =
      x < i
        T.==> let t = boundVar x in Eval.weakBy' b i t T.=== t

letsNonRecursive :: T.TestTree
letsNonRecursive =
  let body = IR.Elim (IR.Bound 0) :: ATerm
      bound = IR.Bound 0
      --
      relation x =
        Eval.weakBy x (IR.Let one bound body)
          T.=== IR.Let one (Eval.weakBy x bound) (Eval.weakBy' x 1 body)
   in forAllNats relation
        |> T.testProperty "'let' binds in its body"

weakOnlyShiftsFree :: T.TestTree
weakOnlyShiftsFree =
  let t :: IR.Term A A
      t = dollarSign 1
   in (\x -> Eval.weakBy x t T.=== dollarSign (succ x))
        |> forAllNats
        |> T.testProperty "weakBy only weakens outer variables"

piBindsItself :: T.TestTree
piBindsItself =
  let body = IR.Elim (IR.Bound 0) :: ATerm
      --
      relation x =
        let IR.Pi _ _ newBody = Eval.weakBy x $ IR.Pi one (boundVar 0) body
         in newBody T.=== Eval.weakBy' x 1 body
   in forAllNats relation
        |> T.testProperty "'Π' binds in its body"

--------------------------------------------------------------------------------
-- property Helpers
--------------------------------------------------------------------------------
forAllNats :: (Show a, Integral a, T.Testable prop) => (a -> prop) -> T.Property
forAllNats =
  T.forAll T.arbitrarySizedNatural
