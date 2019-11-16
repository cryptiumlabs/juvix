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
    ((fst |<< Erasure.erase parameterisation term usage ty) T.@=? Right erased)

test_trivial_unit ∷ T.TestTree
test_trivial_unit = shouldEraseTo unit (HR.Elim (HR.Prim Unit), SNat 1, HR.PrimTy TUnit) (Erased.Prim Unit)
