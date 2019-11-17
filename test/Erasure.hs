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
        T.@=? ((fst |<< Erasure.erase parameterisation term usage ty))
    )

test_trivial_unit ∷ T.TestTree
test_trivial_unit = shouldEraseTo unit (HR.Elim (HR.Prim Unit), SNat 1, HR.PrimTy TUnit) (Erased.Prim Unit)

test_unused_arg ∷ T.TestTree
test_unused_arg = shouldEraseTo unit (constTerm, SNat 1, constTy) (Erased.Lam "y" (Erased.Var "y"))

test_app_unused_arg ∷ T.TestTree
test_app_unused_arg = shouldEraseTo unit (HR.Elim (HR.App (HR.Ann (SNat 1) constTerm constTy) (HR.Elim (HR.Prim Unit))), SNat 1, identityTy) (Erased.Lam "y" (Erased.Var "y"))

identityTerm ∷ HR.Term UnitTy UnitVal
identityTerm = HR.Lam "y" (HR.Elim (HR.Var "y"))

identityTy ∷ HR.Term UnitTy UnitVal
identityTy = HR.Pi (SNat 1) unitTy (HR.Lam "_" unitTy)

constTerm ∷ HR.Term UnitTy UnitVal
constTerm = HR.Lam "x" identityTerm

constTy ∷ HR.Term UnitTy UnitVal
constTy = HR.Pi (SNat 0) unitTy (HR.Lam "_" identityTy)

unitTerm ∷ HR.Term UnitTy UnitVal
unitTerm = HR.Elim (HR.Prim Unit)

unitTy ∷ HR.Term UnitTy UnitVal
unitTy = HR.PrimTy TUnit
