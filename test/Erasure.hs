module Erasure where

import qualified Juvix.Core.Erased as Erased
import qualified Juvix.Core.Erasure as Erasure
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.Parameterisations.Unit as Unit
import qualified Juvix.Core.Types as Core
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (identity)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

shouldEraseTo ∷
  ∀ primTy primVal.
  (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒
  Core.Parameterisation primTy primVal →
  (HR.Term primTy primVal, Usage.T, HR.Term primTy primVal) →
  Erased.Term primVal →
  T.TestTree
shouldEraseTo parameterisation (term, usage, ty) erased =
  T.testCase
    (show (term, usage, ty) <> " should erase to " <> show erased)
    ( Right erased
        T.@=? ((fst . fst) |<< Erasure.erase parameterisation term usage ty)
    )

erasureTests ∷ T.TestTree
erasureTests =
  T.testGroup
    "Erasure"
    [ shouldEraseTo Unit.t (HR.Elim (HR.Prim Unit.Val), one, HR.PrimTy Unit.Ty) (Erased.Prim Unit.Val),
      shouldEraseTo Unit.t (constTerm, one, constTy) (Erased.Lam "y" (Erased.Var "y")),
      usedArg,
      appUnusedArg,
      unusedFunction
    ]

usedArg ∷ T.TestTree
usedArg =
  shouldEraseTo
    Unit.t
    (appTerm, one, appTy)
    (Erased.Lam "f" (Erased.Lam "x" (Erased.App (Erased.Var "f") (Erased.Var "x"))))

appUnusedArg ∷ T.TestTree
appUnusedArg =
  shouldEraseTo
    Unit.t
    ( HR.Elim
        ( HR.App
            (HR.Ann one constTerm constTy)
            (HR.Elim (HR.Prim Unit.Val))
        ),
      one,
      identityTy
    )
    (Erased.Lam "y" (Erased.Var "y"))

unusedFunction ∷ T.TestTree
unusedFunction =
  shouldEraseTo
    Unit.t
    (HR.Elim (HR.App (HR.Ann one constTerm constTy2) identityTerm), one, identityTy)
    (Erased.Lam "y" (Erased.Var "y"))

identityTerm ∷ HR.Term Unit.Ty Unit.Val
identityTerm = HR.Lam "y" (HR.Elim (HR.Var "y"))

identityTy ∷ HR.Term Unit.Ty Unit.Val
identityTy = HR.Pi one unitTy unitTy

appTerm ∷ HR.Term Unit.Ty Unit.Val
appTerm = HR.Lam "f" (HR.Lam "x" (HR.Elim (HR.App (HR.Var "f") (HR.Elim (HR.Var "x")))))

appTy ∷ HR.Term Unit.Ty Unit.Val
appTy = HR.Pi one identityTy (HR.Pi one unitTy unitTy)

constTerm ∷ HR.Term Unit.Ty Unit.Val
constTerm = HR.Lam "x" identityTerm

constTy ∷ HR.Term Unit.Ty Unit.Val
constTy = HR.Pi mempty unitTy identityTy

constTy2 ∷ HR.Term Unit.Ty Unit.Val
constTy2 = HR.Pi mempty identityTy identityTy

unitTerm ∷ HR.Term Unit.Ty Unit.Val
unitTerm = HR.Elim (HR.Prim Unit.Val)

unitTy ∷ HR.Term Unit.Ty Unit.Val
unitTy = HR.PrimTy Unit.Ty
