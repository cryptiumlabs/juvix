-- | Tests for the type checker and evaluator in Core/IR/Typechecker.hs
module CoreTypechecker where

import qualified Juvix.Core.IR as IR
import Juvix.Core.Parameterisations.All as All
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

type AllTerm = IR.Term AllTy AllVal

type AllElim = IR.Elim AllTy AllVal

type AllValue = IR.Value AllTy AllVal

type AllAnnotation = IR.Annotation AllTy AllVal

-- \x. x
identity ∷ ∀ primTy primVal. IR.Term primTy primVal
identity = IR.Lam (IR.Elim (IR.Bound 0))

-- annotation of identity: (1, 1 Nat -> Nat)
identityNatCompTy ∷ NatAnnotation
identityNatCompTy =
  (SNat 1, IR.VPi (SNat 1) (IR.VPrimTy Nat) (const (IR.VPrimTy Nat)))

-- annotation of identity: (1, 1 Unit -> Unit)
identityUnitCompTy ∷ UnitAnnotation
identityUnitCompTy =
  (SNat 1, IR.VPi (SNat 1) (IR.VPrimTy TUnit) (const (IR.VPrimTy TUnit)))

-- contemplating annotation of identity: (0, 0 Nat -> Nat)
identityNatContTy ∷ NatAnnotation
identityNatContTy =
  (SNat 0, IR.VPi (SNat 0) (IR.VPrimTy Nat) (const (IR.VPrimTy Nat)))

-- dependent identity function, \t.\x.x 0: t
depIdentity ∷ ∀ primTy primVal. IR.Term primTy primVal
depIdentity =
  IR.Lam -- first input \t.
    ( IR.Lam -- second input \x.
        ( IR.Elim -- output
            ( IR.Ann -- annotation is of
                (SNat 1) -- 1 usage
                (IR.Elim (IR.Bound 0)) -- x is the output, which is
                (IR.Elim (IR.Bound 1)) -- of type t
            )
        )
    )

-- dependent identity annotation (0, 0 * -> 1 t -> t)
depIdentityCompTy ∷ ∀ primTy primVal. IR.Annotation primTy primVal
depIdentityCompTy =
  ( SNat 1, -- the usage of the dependent identity function
    IR.VPi -- the first input, t
      (SNat 0) -- t's usage
      (IR.VStar 0) -- first input's type, is type
      ( const
          ( IR.VPi -- the second input, x
              (SNat 1) -- x's usage
              (IR.VNeutral (IR.NFree (IR.Local 0))) -- x is of type of the first input, i.e., t
              (const (IR.VNeutral (IR.NFree (IR.Local 0)))) -- the output is of the type that's the same as the second input of this annotation. I.e., t
          )
      )
  )

-- \x.x 1
identityApplication ∷ NatTerm
identityApplication =
  IR.Elim
    ( IR.App -- Applying
        ( IR.Ann -- the function that has annotation of
            (SNat 1) -- usage 1
            identity -- the identity function
            (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- type of 1 Nat -> Nat
        )
        (IR.Elim (IR.Prim (Natural 1))) -- applies to 1
    )

-- annotation (1, Nat)
natTy ∷ NatAnnotation
natTy = (SNat 1, IR.VPrimTy Nat)

-- (I:(Nat->Nat)->(Nat->Nat) I:(Nat->Nat)) 1 type checked to NatTy
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

-- Nat -> () -> Nat
kCompTyWithUnit ∷ AllAnnotation
kCompTyWithUnit =
  ( SNat 1,
    IR.VPi
      (SNat 1)
      (IR.VPrimTy (All.NatTy Nat))
      (const (IR.VPi (SNat 0) (IR.VPrimTy (All.UnitTy TUnit)) (const (IR.VPrimTy (All.NatTy Nat)))))
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

-- (K: Nat -> Nat -> Nat 1) should type check to Nat -> Nat
kApp1 ∷ NatElim
kApp1 =
  IR.App
    ( IR.Ann
        (SNat 1)
        kcombinator
        ( IR.Pi
            (SNat 1)
            (IR.PrimTy Nat)
            (IR.Pi (SNat 0) (IR.PrimTy Nat) (IR.PrimTy Nat))
        )
    )
    (IR.Elim (IR.Prim (Natural 1)))

natToNatTy ∷ NatAnnotation
natToNatTy =
  ( SNat 1,
    IR.VPi (SNat 0) (IR.VPrimTy Nat) (const (IR.VPrimTy Nat))
  )

-- ((K: Nat -> (Nat -> Nat) -> Nat) 1) should type check to (Nat -> Nat) -> Nat
kFunApp1 ∷ NatElim
kFunApp1 =
  IR.App
    ( IR.Ann
        (SNat 1)
        kcombinator
        ( IR.Pi
            (SNat 1)
            (IR.PrimTy Nat)
            ( IR.Pi
                (SNat 0)
                (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat))
                (IR.PrimTy Nat)
            )
        )
    )
    (IR.Elim (IR.Prim (Natural 1)))

kFunApp1CompTy ∷ NatAnnotation
kFunApp1CompTy =
  ( SNat 1,
    IR.VPi
      (SNat 0)
      (IR.VPi (SNat 1) (IR.VPrimTy Nat) (const (IR.VPrimTy Nat)))
      (const (IR.VPrimTy Nat))
  )

--K: (Nat -> Nat) -> Nat -> (Nat -> Nat) I:Nat -> Nat type checks to Nat -> (Nat -> Nat)
kAppI ∷ NatElim
kAppI =
  IR.App
    ( IR.Ann
        (SNat 1)
        kcombinator
        ( IR.Pi
            (SNat 1)
            (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat))
            ( IR.Pi
                (SNat 0)
                (IR.PrimTy Nat)
                (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat))
            )
        )
    )
    ( IR.Elim
        ( IR.Ann
            (SNat 1)
            identity
            (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat))
        )
    )

--K: (Nat -> Nat) -> Nat -> (Nat -> Nat) I:Nat -> Nat type checks to Nat -> (Nat -> Nat)
kAppINotAnnotated ∷ NatElim
kAppINotAnnotated =
  IR.App
    ( IR.Ann
        (SNat 1)
        kcombinator
        ( IR.Pi
            (SNat 1)
            (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat))
            ( IR.Pi
                (SNat 0)
                (IR.PrimTy Nat)
                (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat))
            )
        )
    )
    identity

kAppICompTy ∷ NatAnnotation
kAppICompTy =
  ( SNat 1,
    IR.VPi
      (SNat 0)
      (IR.VPrimTy Nat)
      (const (IR.VPi (SNat 1) (IR.VPrimTy Nat) (const (IR.VPrimTy Nat))))
  )

-- S combinator: Sxyz = xz(yz)
-- Because S returns functions, it's not general because of the annotations.
-- For example, S KSK = KK (SK) = K:Nat-> Nat-> Nat
-- this S takes in KSK, and has x and y annotated as follows:
-- (x = K that takes inputs
--     (1) K, with type signature of z, and
--     (2) SK, the S takes in K and 2 Nats, and has the signature (Nat -> Nat -> Nat) -> Nat -> Nat -> Nat,
--             the K has the type signature of z. So SK has the signature of Nat -> Nat -> Nat
-- so x has the signature of (Nat -> Nat -> Nat) -> (Nat -> Nat -> Nat) -> (Nat -> Nat -> Nat)
-- (y = S that takes in K and 2 Nats and returns a Nat:) (Nat -> Nat-> Nat) -> Nat -> Nat -> Nat
-- (z = K:) Nat -> Nat -> Nat
-- (returns z) -> Nat -> Nat -> Nat
-- To sum, type signature of S in this example is:
-- ((Nat -> Nat -> Nat) -> (Nat -> Nat -> Nat) -> (Nat -> Nat -> Nat)) ->
-- ((Nat -> Nat -> Nat) -> Nat -> Nat -> Nat)
-- (Nat -> Nat -> Nat)
-- this example is too long, not doing this atm

-- example of s combinator with the following signature:
-- the first has type signature of 1 Nat -> 0 Nat -> Nat
-- the second input has type signature 1 Nat -> Nat
-- the third input is Nat
-- type signature of this S is (1 Nat -> 0 Nat -> Nat) -> (1 Nat -> Nat) -> Nat -> Nat
scombinator ∷ NatTerm -- S = \x.\y.\z. (xz) (yz)
scombinator =
  IR.Lam --x/first input (Bound 2, counting from output)
    ( IR.Lam --y/second input (Bound 1, counting from output)
        ( IR.Lam --z/third input (Bound 0, counting from output)
            ( IR.Elim
                ( IR.App -- xz applies to yz
                    ( IR.Ann
                        (SNat 1)
                        ( IR.Elim
                            ( IR.App -- x applies to z
                                ( IR.Ann
                                    (SNat 1)
                                    (IR.Elim (IR.Bound 2)) -- x
                                    ( IR.Pi
                                        (SNat 1)
                                        (IR.PrimTy Nat)
                                        (IR.Pi (SNat 0) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- Annotation of x: (Nat -> Nat -> Nat)
                                    )
                                )
                                (IR.Elim (IR.Bound 0)) -- z
                            )
                        )
                        ( IR.Pi -- Annotation of xz: Nat -> Nat
                            (SNat 0)
                            (IR.PrimTy Nat)
                            (IR.PrimTy Nat)
                        )
                    )
                    ( IR.Elim
                        ( IR.App -- y applies to z
                            ( IR.Ann
                                (SNat 1)
                                (IR.Elim (IR.Bound 1)) -- y
                                (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- Annotation of y
                            )
                            (IR.Elim (IR.Bound 0)) -- z
                        )
                    )
                )
            )
        )
    )

scombinatorCompNatTy ∷ NatAnnotation
scombinatorCompNatTy =
  ( SNat 1,
    IR.VPi
      (SNat 1)
      (IR.VPrimTy Nat) -- (1 Nat ->
      ( const
          ( IR.VPi
              (SNat 0)
              (IR.VPrimTy Nat) -- 0 Nat ->
              ( const
                  ( IR.VPi
                      (SNat 1)
                      ( IR.VPi
                          (SNat 1)
                          (IR.VPrimTy Nat) -- Nat) ->
                          (const (IR.VPrimTy Nat)) --(1 Nat ->
                      )
                      ( const
                          ( IR.VPi
                              (SNat 1)
                              (IR.VPrimTy Nat) -- 1 Nat ->
                              (const (IR.VPrimTy Nat)) -- Nat
                          )
                      )
                  )
              )
          )
      )
  )

-- K 1 (I 1) = 1, so should type checked to Nat
ski1CompNatTy ∷ NatAnnotation
ski1CompNatTy =
  ( SNat 1,
    IR.VPrimTy Nat
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

test_kcombinatorUnit_computational ∷ T.TestTree
test_kcombinatorUnit_computational = shouldCheck All.all kcombinator kCompTyWithUnit

test_identity_app_k ∷ T.TestTree
test_identity_app_k = shouldInfer nat identityAppK kCompTy

test_k_app_I ∷ T.TestTree
test_k_app_I = shouldCheck nat (IR.Elim kAppI) kAppICompTy

test_k_app_I_not_ann ∷ T.TestTree
test_k_app_I_not_ann = shouldCheck nat (IR.Elim kAppINotAnnotated) kAppICompTy

test_k_app_1 ∷ T.TestTree
test_k_app_1 = shouldInfer nat kApp1 natToNatTy

test_kFun_app_1 ∷ T.TestTree
test_kFun_app_1 = shouldInfer nat kFunApp1 kFunApp1CompTy

test_depIdentity ∷ T.TestTree
test_depIdentity = shouldCheck All.all depIdentity depIdentityCompTy

test_nats_type_star0 ∷ T.TestTree
test_nats_type_star0 = shouldCheck nat (IR.PrimTy Nat) (SNat 0, IR.VStar 0)

test_nat1 ∷ T.TestTree
test_nat1 = shouldInfer nat (IR.Prim (Natural 1)) (Omega, IR.VPrimTy Nat)

test_scombinator ∷ T.TestTree
test_scombinator = shouldCheck nat scombinator scombinatorCompNatTy

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
