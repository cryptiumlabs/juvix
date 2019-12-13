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

type NatAnnotation = IR.Annotation NatTy NatVal (IR.EnvTypecheck NatTy NatVal)

type UnitTerm = IR.Term UnitTy UnitVal

type UnitElim = IR.Elim UnitTy UnitVal

type UnitValue = IR.Value UnitTy UnitVal

type UnitAnnotation = IR.Annotation UnitTy UnitVal (IR.EnvTypecheck UnitTy UnitVal)

type AllTerm = IR.Term AllTy AllVal

type AllElim = IR.Elim AllTy AllVal

type AllValue = IR.Value AllTy AllVal

type AllAnnotation = IR.Annotation AllTy AllVal (IR.EnvTypecheck AllTy AllVal)

--unit test generator for typeTerm
shouldCheck ∷
  ∀ primTy primVal.
  (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒
  Parameterisation primTy primVal →
  IR.Term primTy primVal →
  IR.Annotation primTy primVal (IR.EnvTypecheck primTy primVal) →
  T.TestTree
shouldCheck param term ann =
  T.testCase (show term <> " should check as type " <> show ann) $
    fst (IR.exec (IR.typeTerm param 0 [] term ann)) T.@=? Right ()

--unit test generator for typeElim
shouldInfer ∷
  ∀ primTy primVal.
  (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒
  Parameterisation primTy primVal →
  IR.Elim primTy primVal →
  IR.Annotation primTy primVal (IR.EnvTypecheck primTy primVal) →
  T.TestTree
shouldInfer param term ann =
  T.testCase (show term <> " should infer to type " <> show ann) $
    fst (IR.exec (IR.typeElim0 param [] term)) T.@=? Right ann

--unit test generator for evalTerm
shouldEval ∷
  ∀ primTy primVal.
  (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒
  Parameterisation primTy primVal →
  IR.Term primTy primVal →
  IR.Value primTy primVal (IR.EnvTypecheck primTy primVal) →
  T.TestTree
shouldEval param term res =
  T.testCase (show term <> " should evaluate to " <> show res) $
    fst (IR.exec (IR.evalTerm param term IR.initEnv)) T.@=? Right res

test_core ∷ T.TestTree
test_core =
  --TODO after moving away from discover, can rename to coreTests
  T.testGroup
    "Core type checker and evaluator tests"
    [ skiComp,
      natComp,
      dependentFunctionComp,
      evaluations,
      skiCont
    ]

skiComp ∷ T.TestTree
skiComp =
  T.testGroup
    "SKI combinators Computational typing"
    [ shouldCheck nat identity identityNatCompTy,
      shouldCheck unit identity identityUnitCompTy,
      shouldCheck nat identityApplication natTy,
      shouldInfer nat identityAppINat1 natTy,
      shouldInfer nat identityAppI identityNatCompTy,
      shouldCheck nat kcombinator kCompTy,
      shouldCheck All.all kcombinator kCompTyWithUnit,
      shouldInfer nat identityAppK kCompTy,
      shouldCheck nat (IR.Elim kAppI) kAppICompTy,
      shouldCheck nat (IR.Elim kAppINotAnnotated) kAppICompTy,
      shouldInfer nat kApp1 natToNatTy,
      shouldInfer nat kFunApp1 kFunApp1CompTy
      --, shouldCheck nat scombinator scombinatorCompNatTy
    ]

natComp ∷ T.TestTree
natComp =
  T.testGroup
    "Nat Computational typing"
    [ shouldCheck nat (IR.PrimTy Nat) (SNat 0, IR.VStar 0),
      shouldInfer nat (IR.Prim (Natural 1)) (Omega, IR.VPrimTy Nat),
      shouldInfer nat add12 (Omega, IR.VPrimTy Nat)
    ]

dependentFunctionComp ∷ T.TestTree
dependentFunctionComp =
  T.testGroup
    "Dependent Functions Computational typing"
    [ shouldCheck All.all depIdentity depIdentityCompTy
    ]

evaluations ∷ T.TestTree
evaluations =
  T.testGroup
    "Evaluations"
    [ shouldEval nat (IR.Elim add12) (IR.VPrim (Natural 3)),
      shouldEval nat sub52 (IR.VPrim (Natural 3))
    ]

skiCont ∷ T.TestTree
skiCont =
  T.testGroup
    "SKI combinators contemplational typing"
    [ shouldCheck nat identity identityNatContTy
    ]

-- \x. x
identity ∷ ∀ primTy primVal. IR.Term primTy primVal
identity = IR.Lam (IR.Elim (IR.Bound 0))

-- computation annotation of identity: (1, 1 Nat -> Nat)
identityNatCompTy ∷ NatAnnotation
identityNatCompTy =
  (SNat 1, IR.VPi (SNat 1) (IR.VPrimTy Nat) (const (pure (IR.VPrimTy Nat))))

-- computation annotation of identity: (1, 1 Unit -> Unit)
identityUnitCompTy ∷ UnitAnnotation
identityUnitCompTy =
  (SNat 1, IR.VPi (SNat 1) (IR.VPrimTy TUnit) (const (pure (IR.VPrimTy TUnit))))

-- contemplation annotation of identity: (0, 0 Nat -> Nat)
identityNatContTy ∷ NatAnnotation
identityNatContTy =
  (SNat 0, IR.VPi (SNat 0) (IR.VPrimTy Nat) (const (pure (IR.VPrimTy Nat))))

-- dependent identity function, \t.\x.x 1: t
depIdentity ∷ ∀ primTy primVal. IR.Term primTy primVal
depIdentity =
  IR.Lam -- first input \t.
    ( IR.Lam -- second input \x.
        ( IR.Elim -- output
            ( IR.Ann -- annotation is of
                (SNat 1) -- 1 usage
                (IR.Elim (IR.Bound 0)) -- x is the output, which has annotation (1, t)
                (IR.Elim (IR.Bound 1)) -- of type t
            )
        )
    )

-- computation dependent identity annotation (1, 0 * -> 1 t -> t)
depIdentityCompTy ∷ AllAnnotation
depIdentityCompTy =
  ( SNat 1, -- the sig usage of the dependent identity function
    IR.VPi -- the first input, t
      (SNat 0) -- t's usage
      (IR.VStar 0) -- first input's type, is type
      ( const
          ( pure
              ( IR.VPi -- the second input, x
                  (SNat 1) -- x's usage
                  (IR.VNeutral (IR.NFree (IR.Local 0))) -- x is of type of the first input, i.e., t
                  (const (pure (IR.VNeutral (IR.NFree (IR.Local 0)))))
                -- the output is of the type that's the same as the second input of this annotation. I.e., t
                -- TODO: is this right? Chris thinks `t` should be at index 1 and the return type should be t
              )
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
            identity -- the identity function, which has annotation (1, 1 Nat -> Nat)
            (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- type of 1 Nat -> Nat
        )
        (IR.Elim (IR.Prim (Natural 1))) -- applies to 1
    )

-- computation annotation (1, Nat)
natTy ∷ NatAnnotation
natTy = (SNat 1, IR.VPrimTy Nat)

-- (I:1 (1 Nat->Nat) -> (1 Nat->Nat) I:(1 Nat->Nat) ) 1 type checked to NatTy
identityAppINat1 ∷ NatElim
identityAppINat1 =
  IR.App -- applying (identity to identity) to 1
    ( IR.App -- applying identity to identity
        ( IR.Ann
            (SNat 1) -- sig usage, the first 1 in the annotation
            identity -- has annotation ( 1, 1 ((1 Nat -> Nat)) -> (1 Nat -> Nat) )
            ( IR.Pi
                (SNat 1) -- the second 1 in the annotation
                (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- the third 1 in the annotation, (1 Nat -> Nat)
                (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- the forth 1 in the annotation, (1 Nat -> Nat)
            )
        )
        ( IR.Elim
            ( IR.Ann
                (SNat 1) -- sig usage, the first 1 in the annotation
                identity -- has annotation (1, 1 Nat -> Nat)
                ( IR.Pi
                    (SNat 1) -- the second 1 in the annotation
                    (IR.PrimTy Nat) -- 1 Nat ->
                    (IR.PrimTy Nat) -- Nat
                )
            )
        )
    )
    (IR.Elim (IR.Prim (Natural 1)))

-- I:(Nat->Nat)->(Nat->Nat) I:(Nat->Nat) type checked to (Nat->Nat)
-- I:(Nat->Nat) I:(Nat->Nat) correctly does not type checked
identityAppI ∷ NatElim
identityAppI =
  IR.App -- applying identity to identity
    ( IR.Ann
        (SNat 1) -- sig usage, the first 1 in the annotation
        identity -- has annotation (1, (1, (1 Nat -> Nat) ) -> (1 Nat -> Nat) ) )
        ( IR.Pi
            (SNat 1) -- the second 1 in the annotation
            (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- the third 1 in the annotation 1 Nat -> Nat
            (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- the forth 1 in the annotation 1 Nat -> Nat
        )
    )
    ( IR.Elim
        ( IR.Ann
            (SNat 1) -- sig usage, the first 1 of the annotation
            identity -- annotation (1, 1 Nat -> Nat)
            (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- the second 1 of the annotation, 1 Nat -> Nat
        )
    )

kcombinator ∷ ∀ primTy primVal. IR.Term primTy primVal -- K = \x.\y.x
kcombinator = IR.Lam (IR.Lam (IR.Elim (IR.Bound 1)))

-- K has annotation (1, 1 Nat -> 0 Nat -> Nat )
kCompTy ∷ NatAnnotation
kCompTy =
  ( SNat 1, -- the sig usage of k
    IR.VPi -- first input, 1 Nat
      (SNat 1) -- is used once in the output
      (IR.VPrimTy Nat) -- of type Nat
      ( const
          ( pure
              ( IR.VPi -- second input, 0 Nat
                  (SNat 0) -- is not used in the output
                  (IR.VPrimTy Nat) -- of type Nat
                  (const (pure (IR.VPrimTy Nat))) -- the output is of type Nat
              )
          )
      )
  )

-- K computation annotation (1, 1 Nat -> 0 () -> Nat)
kCompTyWithUnit ∷ AllAnnotation
kCompTyWithUnit =
  ( SNat 1, -- sig usage of k
    IR.VPi -- first input, 1 Nat
      (SNat 1) -- is used once in the output
      (IR.VPrimTy (All.NatTy Nat)) -- of type Nat
      ( const
          ( pure
              ( IR.VPi -- second input, 0 Unit
                  (SNat 0) -- is not used in the output
                  (IR.VPrimTy (All.UnitTy TUnit)) -- of type Unit
                  (const (pure (IR.VPrimTy (All.NatTy Nat)))) -- the output is of type Nat
              )
          )
      )
  )

-- I K computation annotation (1, 1 (1 Nat -> 0 Nat -> Nat) -> ( 1 Nat -> 0 Nat -> Nat) -> (1 Nat -> 0 Nat-> Nat) )
identityAppK ∷ NatElim
identityAppK =
  IR.App -- applying I to K
    ( IR.Ann -- I
        (SNat 1) -- sig usage, the first 1 in the annotation
        identity -- annotation (1, (1 Nat -> 0 Nat -> Nat) -> ( 1 Nat -> 0 Nat -> Nat) )
        ( IR.Pi
            (SNat 1) -- sig usage, the first 1 in the annotation
            ( IR.Pi
                (SNat 1) -- the second 1 in the annotation
                (IR.PrimTy Nat) -- (1 Nat ->
                (IR.Pi (SNat 0) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- 0 Nat -> Nat)
                    -- ->
            )
            ( IR.Pi
                (SNat 1)
                (IR.PrimTy Nat) -- (1 Nat ->
                (IR.Pi (SNat 0) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- 0 Nat -> Nat)
            )
        )
    ) -- K
    ( IR.Elim
        ( IR.Ann
            (SNat 1) -- sig usage
            kcombinator -- annotation (1, (1 Nat -> 0 Nat-> Nat))
            ( IR.Pi
                (SNat 1)
                (IR.PrimTy Nat) -- (1 Nat ->
                (IR.Pi (SNat 0) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- 0 Nat -> Nat)
            )
        )
    )

-- (K: Nat -> Nat -> Nat 1) should type check to Nat -> Nat
kApp1 ∷ NatElim
kApp1 =
  IR.App -- applying K to 1
    ( IR.Ann -- K
        (SNat 1) -- sig usage
        kcombinator -- annotation (1, (1 Nat -> 0 Nat -> Nat))
        ( IR.Pi
            (SNat 1)
            (IR.PrimTy Nat) -- (1 Nat ->
            (IR.Pi (SNat 0) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- 0 Nat -> Nat)
        )
    ) -- 1
    (IR.Elim (IR.Prim (Natural 1)))

-- computation annotation (0 Nat -> Nat)
natToNatTy ∷ NatAnnotation
natToNatTy =
  ( SNat 1, -- sig usage
    IR.VPi (SNat 0) (IR.VPrimTy Nat) (const (pure (IR.VPrimTy Nat))) -- 0 Nat -> Nat
  )

-- (1 (K: 1 Nat -> 0 (1 Nat -> Nat) -> Nat) 1) should type check to 0 (1 Nat -> Nat) -> Nat
kFunApp1 ∷ NatElim
kFunApp1 =
  IR.App -- applying K to 1
    ( IR.Ann
        (SNat 1) -- sig usage
        kcombinator -- annotation (1, (1 Nat -> 0 (1 Nat -> Nat) -> Nat))
        ( IR.Pi
            (SNat 1)
            (IR.PrimTy Nat) -- 1 Nat ->
            ( IR.Pi
                (SNat 0) -- usage of (1 Nat -> Nat )
                (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- (1 Nat -> Nat ) ->
                (IR.PrimTy Nat) -- Nat
            )
        )
    )
    (IR.Elim (IR.Prim (Natural 1))) -- 1
          -- computation annotation (1, 0 (1 Nat -> Nat) -> Nat)

kFunApp1CompTy ∷ NatAnnotation
kFunApp1CompTy =
  ( SNat 1,
    IR.VPi
      (SNat 0)
      (IR.VPi (SNat 1) (IR.VPrimTy Nat) (const (pure (IR.VPrimTy Nat))))
      (const (pure (IR.VPrimTy Nat)))
  )

--1 K: 1 (1 Nat -> Nat) -> 0 Nat -> (1 Nat -> Nat) 1 I: 1 Nat -> Nat type checks to 0 Nat -> (1 Nat -> Nat)
kAppI ∷ NatElim
kAppI =
  IR.App -- applying K to I
    ( IR.Ann
        (SNat 1) -- sig usage
        kcombinator
        ( IR.Pi
            (SNat 1) -- usage of (1 Nat -> Nat)
            (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- (1 Nat -> Nat) ->
            ( IR.Pi
                (SNat 0)
                (IR.PrimTy Nat) -- 0 Nat ->
                (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- (1 Nat -> Nat)
            )
        )
    )
    ( IR.Elim
        ( IR.Ann -- I
            (SNat 1) -- usage of identity
            identity
            (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- 1 Nat -> Nat
        )
    )

--1 K: 1 (1 Nat -> Nat) -> 0 Nat -> (1 Nat -> Nat) I type checks to 0 Nat -> (1 Nat -> Nat)
kAppINotAnnotated ∷ NatElim
kAppINotAnnotated =
  IR.App
    ( IR.Ann
        (SNat 1) -- sig usage
        kcombinator
        ( IR.Pi
            (SNat 1) -- usage of (1 Nat -> Nat)
            (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- (1 Nat -> Nat) ->
            ( IR.Pi
                (SNat 0)
                (IR.PrimTy Nat) -- 0 Nat ->
                (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- (1 Nat -> Nat)
            )
        )
    )
    identity

-- computation annotation (1, 0 Nat -> (1 Nat -> Nat))
kAppICompTy ∷ NatAnnotation
kAppICompTy =
  ( SNat 1, --sig usage
    IR.VPi
      (SNat 0)
      (IR.VPrimTy Nat) -- 0 Nat ->
      (const (pure (IR.VPi (SNat 1) (IR.VPrimTy Nat) (const (pure (IR.VPrimTy Nat)))))) -- (1 Nat -> Nat)
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
-- type signature of this S is 1 (1 Nat -> 0 Nat -> Nat) -> 1 (1 Nat -> Nat) -> 2 Nat -> Nat
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
                                    (SNat 1) -- usage of x
                                    (IR.Elim (IR.Bound 2)) -- x
                                    ( IR.Pi
                                        (SNat 1)
                                        (IR.PrimTy Nat)
                                        (IR.Pi (SNat 0) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- Annotation of x: (1 Nat -> 0 Nat -> Nat)
                                    )
                                )
                                (IR.Elim (IR.Bound 0)) -- z
                            )
                        )
                        ( IR.Pi -- Annotation of xz: 0 Nat -> Nat
                            (SNat 0)
                            (IR.PrimTy Nat)
                            (IR.PrimTy Nat)
                        )
                    )
                    ( IR.Elim
                        ( IR.App -- y applies to z
                            ( IR.Ann
                                (SNat 1) -- usage of y
                                (IR.Elim (IR.Bound 1)) -- y
                                (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat)) -- Annotation of y, (1 Nat -> Nat)
                            )
                            (IR.Elim (IR.Bound 0)) -- z
                        )
                    )
                )
            )
        )
    )

-- S xyz = (xz) (yz)
-- computation annotation of S (1, 1 (1 Nat -> 0 Nat -> Nat) -> 1 (1 Nat -> Nat) -> 2 Nat -> Nat )
scombinatorCompNatTy ∷ NatAnnotation
scombinatorCompNatTy =
  ( SNat 1, -- sig usage of S
    IR.VPi --
      (SNat 1) -- usage of 1 (1 Nat -> 0 Nat -> Nat)
      ( IR.VPi
          (SNat 1)
          (IR.VPrimTy Nat) -- (1 Nat ->
          ( const
              ( pure
                  ( IR.VPi
                      (SNat 0)
                      (IR.VPrimTy Nat) -- 0 Nat ->
                      (const (pure (IR.VPrimTy Nat))) -- Nat) ->
                  )
              )
          )
      )
      ( const
          ( pure
              ( IR.VPi -- second input, (1 Nat -> Nat)
                  (SNat 1) -- usage of (1 Nat -> Nat)
                  ( IR.VPi
                      (SNat 1)
                      (IR.VPrimTy Nat) --(1 Nat ->
                      (const (pure (IR.VPrimTy Nat))) -- Nat) ->
                  )
                  ( const
                      ( pure
                          ( IR.VPi
                              (SNat 2)
                              (IR.VPrimTy Nat) -- 2 Nat ->
                              (const (pure (IR.VPrimTy Nat))) -- Nat
                          )
                      )
                  )
              )
          )
      )
  )

-- K 1 (I 1) = 1, so should type checked to (1, Nat)
ski1CompNatTy ∷ NatAnnotation
ski1CompNatTy =
  ( SNat 1,
    IR.VPrimTy Nat
  )

add12 ∷ NatElim
add12 =
  IR.App
    (IR.App (IR.Prim Add) (IR.Elim (IR.Prim (Natural 1))))
    (IR.Elim (IR.Prim (Natural 2)))

sub52 ∷ NatTerm
sub52 =
  IR.Elim
    ( IR.App
        (IR.App (IR.Prim Sub) (IR.Elim (IR.Prim (Natural 5))))
        (IR.Elim (IR.Prim (Natural 2)))
    )

one ∷ ∀ primTy primVal. IR.Term primTy primVal
one = IR.Lam $ IR.Lam $ IR.Elim $ IR.App (IR.Bound 1) (IR.Elim (IR.Bound 0))

oneCompTy ∷ NatAnnotation
oneCompTy =
  ( SNat 1,
    IR.VPi
      (SNat 1)
      (IR.VPi (SNat 1) (IR.VPrimTy Nat) (const (pure (IR.VPrimTy Nat))))
      (const (pure (IR.VPi (SNat 1) (IR.VPrimTy Nat) (const (pure (IR.VPrimTy Nat))))))
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
      (IR.VPi (SNat 1) (IR.VPrimTy Nat) (const (pure (IR.VPrimTy Nat))))
      (const (pure (IR.VPi (SNat 1) (IR.VPrimTy Nat) (const (pure (IR.VPrimTy Nat))))))
  )
