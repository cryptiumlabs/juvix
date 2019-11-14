-- | Tests for the type checker and evaluator in Core/IR/Typechecker.hs
module CoreTypechecker where

import qualified Juvix.Core.IR as IR
import Juvix.Core.Parameterisations.All
import Juvix.Core.Parameterisations.Naturals
import Juvix.Core.Parameterisations.Unit
import Juvix.Core.Types
import Juvix.Core.Usage
import Juvix.Library hiding (identity)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

type NatTerm = IR.Term NatTy NatVal

type NatElim = IR.Elim NatTy NatVal

type NatValue = IR.Value NatTy NatVal

type NatAnnotation = IR.Annotation NatTy NatVal

type UnitTerm = IR.Term UnitTy UnitVal

type UnitElim = IR.Elim UnitTy UnitVal

type UnitValue = IR.Value UnitTy UnitVal

type UnitAnnotation = IR.Annotation UnitTy UnitVal

identity ∷ ∀ primTy primVal. IR.Term primTy primVal
identity = IR.Lam (IR.Elim (IR.Bound 0))

identityNatCompTy ∷ NatAnnotation
identityNatCompTy =
  (SNat 1, IR.VPi (SNat 1) (IR.VPrimTy Nat) (const (IR.VPrimTy Nat)))

identityUnitCompTy ∷ UnitAnnotation
identityUnitCompTy =
  (SNat 1, IR.VPi (SNat 1) (IR.VPrimTy TUnit) (const (IR.VPrimTy TUnit)))

identityNatContTy ∷ NatAnnotation
identityNatContTy =
  (SNat 0, IR.VPi (SNat 0) (IR.VPrimTy Nat) (const (IR.VPrimTy Nat)))

identityApplication ∷ NatTerm
identityApplication =
  IR.Elim
    ( IR.App
        ( IR.Ann
            (SNat 1)
            identity
            (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat))
        )
        (IR.Elim (IR.Prim (Natural 1)))
    )

natTy ∷ NatAnnotation
natTy = (SNat 1, IR.VPrimTy Nat)

-- (I:(Nat->Nat)->(Nat->Nat) I:(Nat->Nat)) I:Nat type checked to NatTy
identityAppINat1 ∷ NatElim
identityAppINat1 =
  IR.App
    ( IR.App
        ( IR.Ann
            (SNat 1)
            identity
            ( IR.Pi
                (SNat 1)
                (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat))
                (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat))
            )
        )
        ( IR.Elim
            ( IR.Ann
                (SNat 1)
                identity
                (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat))
            )
        )
    )
    (IR.Elim (IR.Prim (Natural 1)))

-- I:(Nat->Nat)->(Nat->Nat) I:(Nat->Nat) type checked to (Nat->Nat)
-- I:(Nat->Nat) I:(Nat->Nat) correctly does not type checked
identityAppI ∷ NatElim
identityAppI =
  IR.App
    ( IR.Ann
        (SNat 1)
        identity
        ( IR.Pi
            (SNat 1)
            (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat))
            (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat))
        )
    )
    ( IR.Elim
        ( IR.Ann
            (SNat 1)
            identity
            (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat))
        )
    )

kcombinator ∷ ∀ primTy primVal. IR.Term primTy primVal -- K = \x.\y.x
kcombinator = IR.Lam (IR.Lam (IR.Elim (IR.Bound 1)))

-- Nat -> (Nat -> Nat)
kCompTy ∷ NatAnnotation
kCompTy =
  ( SNat 1,
    IR.VPi
      (SNat 1)
      (IR.VPrimTy Nat)
      (const (IR.VPi (SNat 0) (IR.VPrimTy Nat) (const (IR.VPrimTy Nat))))
  )

-- I:(Nat->Nat->Nat)->(Nat->Nat->Nat) K:(Nat->Nat->Nat) should type check to (Nat->Nat->Nat)
identityAppK ∷ NatElim
identityAppK =
  IR.App
    ( IR.Ann
        (SNat 1)
        identity
        ( IR.Pi
            (SNat 1)
            ( IR.Pi
                (SNat 1)
                (IR.PrimTy Nat)
                (IR.Pi (SNat 0) (IR.PrimTy Nat) (IR.PrimTy Nat))
            )
            ( IR.Pi
                (SNat 1)
                (IR.PrimTy Nat)
                (IR.Pi (SNat 0) (IR.PrimTy Nat) (IR.PrimTy Nat))
            )
        )
    )
    ( IR.Elim
        ( IR.Ann
            (SNat 1)
            kcombinator
            ( IR.Pi
                (SNat 1)
                (IR.PrimTy Nat)
                (IR.Pi (SNat 0) (IR.PrimTy Nat) (IR.PrimTy Nat))
            )
        )
    )

test_identity_computational ∷ T.TestTree
test_identity_computational = shouldCheck nat identity identityNatCompTy

test_identity_unit_computational ∷ T.TestTree
test_identity_unit_computational = shouldCheck unit identity identityUnitCompTy

test_identity_contemplation ∷ T.TestTree
test_identity_contemplation = shouldCheck nat identity identityNatContTy

test_identity_application ∷ T.TestTree
test_identity_application = shouldCheck nat identityApplication natTy

test_identity_app_I_Nat1 ∷ T.TestTree
test_identity_app_I_Nat1 = shouldInfer nat identityAppINat1 natTy

test_identity_app_I ∷ T.TestTree
test_identity_app_I = shouldInfer nat identityAppI identityNatCompTy

test_kcombinator_computational ∷ T.TestTree
test_kcombinator_computational = shouldCheck nat kcombinator kCompTy

test_identity_app_k ∷ T.TestTree
test_identity_app_k = shouldInfer nat identityAppK kCompTy

test_nats_type_star0 ∷ T.TestTree
test_nats_type_star0 = shouldCheck nat (IR.PrimTy Nat) (SNat 0, IR.VStar 0)

test_nat1 ∷ T.TestTree
test_nat1 = shouldInfer nat (IR.Prim (Natural 1)) (Omega, IR.VPrimTy Nat)

test_add_nat ∷ T.TestTree
test_add_nat =
  shouldInfer
    nat
    ( IR.App
        (IR.App (IR.Prim Add) (IR.Elim (IR.Prim (Natural 1))))
        (IR.Elim (IR.Prim (Natural 2)))
    )
    (Omega, IR.VPrimTy Nat)

test_eval_add ∷ T.TestTree
test_eval_add =
  shouldEval
    nat
    ( IR.Elim
        ( IR.App
            (IR.App (IR.Prim Add) (IR.Elim (IR.Prim (Natural 1))))
            (IR.Elim (IR.Prim (Natural 2)))
        )
    )
    (IR.VPrim (Natural 3))

test_eval_sub ∷ T.TestTree
test_eval_sub =
  shouldEval
    nat
    ( IR.Elim
        ( IR.App
            (IR.App (IR.Prim Sub) (IR.Elim (IR.Prim (Natural 5))))
            (IR.Elim (IR.Prim (Natural 2)))
        )
    )
    (IR.VPrim (Natural 3))

--unit tests for cType
shouldCheck ∷
  ∀ primTy primVal.
  (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒
  Parameterisation primTy primVal →
  IR.Term primTy primVal →
  IR.Annotation primTy primVal →
  T.TestTree
shouldCheck param term ann =
  T.testCase (show term <> " should check as type " <> show ann) $
    IR.cType param 0 [] term ann T.@=? Right ()

--unit tests for iType
shouldInfer ∷
  ∀ primTy primVal.
  (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒
  Parameterisation primTy primVal →
  IR.Elim primTy primVal →
  IR.Annotation primTy primVal →
  T.TestTree
shouldInfer param term ann =
  T.testCase (show term <> " should infer to type " <> show ann) $
    IR.iType0 param [] term T.@=? Right ann

shouldEval ∷
  ∀ primTy primVal.
  (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒
  Parameterisation primTy primVal →
  IR.Term primTy primVal →
  IR.Value primTy primVal →
  T.TestTree
shouldEval param term res =
  T.testCase (show term <> " should evaluate to " <> show res) $
    IR.cEval param term IR.initEnv T.@=? res

one ∷ ∀ primTy primVal. IR.Term primTy primVal
one = IR.Lam $ IR.Lam $ IR.Elim $ IR.App (IR.Bound 1) (IR.Elim (IR.Bound 0))

oneCompTy ∷ NatAnnotation
oneCompTy =
  ( SNat 1,
    IR.VPi
      (SNat 1)
      (IR.VPi (SNat 1) (IR.VPrimTy Nat) (const (IR.VPrimTy Nat)))
      (const (IR.VPi (SNat 1) (IR.VPrimTy Nat) (const (IR.VPrimTy Nat))))
  )

two ∷ ∀ primTy primVal. IR.Term primTy primVal
two =
  IR.Lam
    $ IR.Lam
    $ IR.Elim
    $ IR.App (IR.Bound 1) (IR.Elim (IR.App (IR.Bound 1) (IR.Elim (IR.Bound 0))))

twoCompTy ∷ NatAnnotation
twoCompTy =
  ( SNat 1,
    IR.VPi
      (SNat 2)
      (IR.VPi (SNat 1) (IR.VPrimTy Nat) (const (IR.VPrimTy Nat)))
      (const (IR.VPi (SNat 1) (IR.VPrimTy Nat) (const (IR.VPrimTy Nat))))
  )
