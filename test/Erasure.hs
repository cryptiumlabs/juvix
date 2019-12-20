module Erasure where

import qualified Juvix.Core.Erased as Erased
import qualified Juvix.Core.Erasure as Erasure
import qualified Juvix.Core.HR as HR
import Juvix.Core.Parameterisations.Unit
import qualified Juvix.Core.Types as Core
import Juvix.Core.Usage
import Juvix.Library hiding (identity)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

shouldEraseTo ∷
  ∀ primTy primVal.
  (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒
  Core.Parameterisation primTy primVal →
  (HR.Term primTy primVal, Usage, HR.Term primTy primVal) →
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
    [ shouldEraseTo unit (HR.Elim (HR.Prim Unit), SNat 1, HR.PrimTy TUnit) (Erased.Prim Unit),
      shouldEraseTo unit (constTerm, SNat 1, constTy) (Erased.Lam "y" (Erased.Var "y")),
      usedArg,
      appUnusedArg,
      unusedFunction
    ]

usedArg ∷ T.TestTree
usedArg =
  shouldEraseTo
    unit
    (appTerm, SNat 1, appTy)
    (Erased.Lam "f" (Erased.Lam "x" (Erased.App (Erased.Var "f") (Erased.Var "x"))))

appUnusedArg ∷ T.TestTree
appUnusedArg =
  shouldEraseTo
    unit
    ( HR.Elim
        ( HR.App
            (HR.Ann (SNat 1) constTerm constTy)
            (HR.Elim (HR.Prim Unit))
        ),
      SNat 1,
      identityTy
    )
    (Erased.Lam "y" (Erased.Var "y"))

unusedFunction ∷ T.TestTree
unusedFunction =
  shouldEraseTo
    unit
    (HR.Elim (HR.App (HR.Ann (SNat 1) constTerm constTy2) identityTerm), SNat 1, identityTy)
    (Erased.Lam "y" (Erased.Var "y"))

identityTerm ∷ HR.Term UnitTy UnitVal
identityTerm = HR.Lam "y" (HR.Elim (HR.Var "y"))

identityTy ∷ HR.Term UnitTy UnitVal
identityTy = HR.Pi (SNat 1) unitTy unitTy

appTerm ∷ HR.Term UnitTy UnitVal
appTerm = HR.Lam "f" (HR.Lam "x" (HR.Elim (HR.App (HR.Var "f") (HR.Elim (HR.Var "x")))))

appTy ∷ HR.Term UnitTy UnitVal
appTy = HR.Pi (SNat 1) identityTy (HR.Pi (SNat 1) unitTy unitTy)

constTerm ∷ HR.Term UnitTy UnitVal
constTerm = HR.Lam "x" identityTerm

constTy ∷ HR.Term UnitTy UnitVal
constTy = HR.Pi (SNat 0) unitTy identityTy

constTy2 ∷ HR.Term UnitTy UnitVal
constTy2 = HR.Pi (SNat 0) identityTy identityTy

unitTerm ∷ HR.Term UnitTy UnitVal
unitTerm = HR.Elim (HR.Prim Unit)

unitTy ∷ HR.Term UnitTy UnitVal
unitTy = HR.PrimTy TUnit
