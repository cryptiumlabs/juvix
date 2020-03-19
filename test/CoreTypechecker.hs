-- | Tests for the type checker and evaluator in Core/IR/Typechecker.hs
module CoreTypechecker where

import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Parameterisations.All as All
import qualified Juvix.Core.Parameterisations.Naturals as Nat
import qualified Juvix.Core.Parameterisations.Unit as Unit
import Juvix.Core.Types
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (identity)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

type NatTerm = IR.Term Nat.Ty Nat.Val

type NatElim = IR.Elim Nat.Ty Nat.Val

type Value = IR.Value Nat.Ty Nat.Val

type NatAnnotation = IR.Annotation Nat.Ty Nat.Val (IR.EnvTypecheck Nat.Ty Nat.Val)

type UnitTerm = IR.Term Unit.Ty Unit.Val

type UnitElim = IR.Elim Unit.Ty Unit.Val

type UnitValue = IR.Value Unit.Ty Unit.Val

type UnitAnnotation = IR.Annotation Unit.Ty Unit.Val (IR.EnvTypecheck Unit.Ty Unit.Val)

type AllTerm = IR.Term All.Ty All.Val

type AllElim = IR.Elim All.Ty All.Val

type AllValue = IR.Value All.Ty All.Val

type AllAnnotation = IR.Annotation All.Ty All.Val (IR.EnvTypecheck All.Ty All.Val)

-- unit test generator for typeTerm
shouldCheck ∷
  ∀ primTy primVal.
  (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒
  Parameterisation primTy primVal →
  IR.Term primTy primVal →
  IR.Annotation primTy primVal (IR.EnvTypecheck primTy primVal) →
  T.TestTree
shouldCheck param term ann =
  -- TODO: take out the logs and put them in an IO monad.
  let logs =
        intercalate "\n"
          $ map IR.msg
          $ IR.typecheckerLog
          $ snd
          $ IR.exec
          $ IR.typeTerm param 0 [] term ann
   in T.testCase
        ( show term
            <> " should check as type "
            <> show ann
            <> show logs
        )
        $ fst (IR.exec (IR.typeTerm param 0 [] term ann)) T.@=? Right ()

-- unit test generator for typeElim
shouldInfer ∷
  ∀ primTy primVal.
  (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒
  Parameterisation primTy primVal →
  IR.Elim primTy primVal →
  IR.Annotation primTy primVal (IR.EnvTypecheck primTy primVal) →
  T.TestTree
shouldInfer param term ann =
  let logs = concatMap IR.msg (IR.typecheckerLog $ snd (IR.exec (IR.typeElim0 param [] term)))
   in T.testCase (show term <> " should infer to type " <> show ann <> show logs) $
        fst (IR.exec (IR.typeElim0 param [] term)) T.@=? Right ann

-- unit test generator for evalTerm
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

coreCheckerEval ∷ T.TestTree
coreCheckerEval =
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
    [ shouldCheck Nat.t identity identityNatCompTy,
      shouldCheck Unit.t identity identityUnitCompTy,
      shouldCheck Nat.t identityApplication natTy,
      shouldInfer Nat.t identityAppINat1 natTy,
      shouldInfer Nat.t identityAppI identityNatCompTy,
      shouldCheck Nat.t kcombinator kCompTy,
      shouldCheck All.t kcombinator kCompTyWithUnit,
      shouldInfer Nat.t identityAppK kCompTy,
      shouldCheck Nat.t (IR.Elim kAppI) kAppICompTy,
      shouldCheck Nat.t (IR.Elim kAppINotAnnotated) kAppICompTy,
      shouldInfer Nat.t kApp1 natToNatTy,
      shouldInfer
        Nat.t
        kFunApp1
        kFunApp1CompTy {- ,
        shouldCheck
          nat
          scombinator
          scombinatorCompNat.Ty -}
    ]

natComp ∷ T.TestTree
natComp =
  T.testGroup
    "Nat Computational typing"
    [ shouldCheck Nat.t (IR.PrimTy Nat.Ty) (IR.Annotated mempty (IR.VStar 0)),
      shouldInfer Nat.t (IR.Prim (Nat.Val 1)) (IR.Annotated Usage.Omega (IR.VPrimTy Nat.Ty)),
      shouldInfer Nat.t add12 (IR.Annotated Usage.Omega (IR.VPrimTy Nat.Ty))
    ]

dependentFunctionComp ∷ T.TestTree
dependentFunctionComp =
  T.testGroup
    "Dependent Functions Computational typing"
    [ shouldCheck
        All.t
        depIdentity
        depIdentityCompTy,
      shouldCheck
        All.t
        depIdentity
        depIdentityCompTyOmega,
      shouldCheck
        All.t
        depK
        depKCompTy
    ]

evaluations ∷ T.TestTree
evaluations =
  T.testGroup
    "Evaluations"
    [ shouldEval Nat.t (IR.Elim add12) (IR.VPrim (Nat.Val 3)),
      shouldEval Nat.t sub52 (IR.VPrim (Nat.Val 3))
    ]

skiCont ∷ T.TestTree
skiCont =
  T.testGroup
    "SKI combinators contemplational typing"
    [ shouldCheck Nat.t identity identityNatContTy
    ]

-- \x. x
identity ∷ ∀ primTy primVal. IR.Term primTy primVal
identity = IR.Lam (IR.Elim (IR.Bound 0))

-- computation annotation of identity: (1, 1 Nat -> Nat)
identityNatCompTy ∷ NatAnnotation
identityNatCompTy =
  IR.Annotated one (IR.VPi one (IR.VPrimTy Nat.Ty) (const (pure (IR.VPrimTy Nat.Ty))))

-- computation annotation of identity: (1, 1 Unit -> Unit)
identityUnitCompTy ∷ UnitAnnotation
identityUnitCompTy =
  IR.Annotated one (IR.VPi one (IR.VPrimTy Unit.Ty) (const (pure (IR.VPrimTy Unit.Ty))))

-- contemplation annotation of identity: (0, 0 Nat -> Nat)
identityNatContTy ∷ NatAnnotation
identityNatContTy =
  IR.Annotated mempty (IR.VPi mempty (IR.VPrimTy Nat.Ty) (const (pure (IR.VPrimTy Nat.Ty))))

-- dependent identity function, \t.\x.x 1: t
depIdentity ∷ ∀ primTy primVal. IR.Term primTy primVal
depIdentity =
  IR.Lam -- first input \t.
    ( IR.Lam -- second input \x.
        ( IR.Elim -- output
            ( IR.Ann -- annotation is of
                one -- 1 usage
                (IR.Elim (IR.Bound 0)) -- x is the output, which has annotation (1, t)
                (IR.Elim (IR.Bound 1)) -- of type t
            )
        )
    )

-- computation dependent identity annotation (1, 0 * -> 1 t -> t)
depIdentityCompTy ∷ AllAnnotation
depIdentityCompTy =
  IR.Annotated
    one -- the sig usage of the dependent identity function
    ( IR.VPi -- the first input, t
        mempty -- t's usage
        (IR.VStar 0) -- first input's type, is type
        ( const
            ( pure
                ( IR.VPi -- the second input, x
                    one -- x's usage
                    (IR.VNeutral (IR.NFree (IR.Local 0))) -- Local 0 is the first input, i.e. t. x is of type t
                    (const (pure (IR.VNeutral (IR.NFree (IR.Local 0))))) -- the return type is t
                )
            )
        )
    )

-- computation dependent identity annotation (1, 0 * -> w t -> t)
depIdentityCompTyOmega ∷ AllAnnotation
depIdentityCompTyOmega =
  IR.Annotated
    one -- the sig usage of the dependent identity function
    ( IR.VPi -- the first input, t
        mempty -- t's usage
        (IR.VStar 0) -- first input's type, is type
        ( const
            ( pure
                ( IR.VPi -- the second input, x
                    Usage.Omega -- x's usage
                    (IR.VNeutral (IR.NFree (IR.Local 0))) -- Local 0 is the first input, i.e. t. x is of type t
                    (const (pure (IR.VNeutral (IR.NFree (IR.Local 0))))) -- the return type is t
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
            one -- usage 1
            identity -- the identity function, which has annotation (1, 1 Nat -> Nat)
            (IR.Pi one (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- type of 1 Nat -> Nat
        )
        (IR.Elim (IR.Prim (Nat.Val 1))) -- applies to 1
    )

-- computation annotation (1, Nat)
natTy ∷ NatAnnotation
natTy = IR.Annotated one (IR.VPrimTy Nat.Ty)

-- (I:1 (1 Nat->Nat) -> (1 Nat->Nat) I:(1 Nat->Nat) ) 1 type checked to Nat.Ty
identityAppINat1 ∷ NatElim
identityAppINat1 =
  IR.App -- applying (identity to identity) to 1
    ( IR.App -- applying identity to identity
        ( IR.Ann
            one -- sig usage, the first 1 in the annotation
            identity -- has annotation ( 1, 1 ((1 Nat -> Nat)) -> (1 Nat -> Nat) )
            ( IR.Pi
                one -- the second 1 in the annotation
                (IR.Pi one (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- the third 1 in the annotation, (1 Nat -> Nat)
                (IR.Pi one (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- the forth 1 in the annotation, (1 Nat -> Nat)
            )
        )
        ( IR.Elim
            ( IR.Ann
                one -- sig usage, the first 1 in the annotation
                identity -- has annotation (1, 1 Nat -> Nat)
                ( IR.Pi
                    one -- the second 1 in the annotation
                    (IR.PrimTy Nat.Ty) -- 1 Nat ->
                    (IR.PrimTy Nat.Ty) -- Nat
                )
            )
        )
    )
    (IR.Elim (IR.Prim (Nat.Val 1)))

-- I:(Nat->Nat)->(Nat->Nat) I:(Nat->Nat) type checked to (Nat->Nat)
-- I:(Nat->Nat) I:(Nat->Nat) correctly does not type checked
identityAppI ∷ NatElim
identityAppI =
  IR.App -- applying identity to identity
    ( IR.Ann
        one -- sig usage, the first 1 in the annotation
        identity -- has annotation (1, (1, (1 Nat -> Nat) ) -> (1 Nat -> Nat) ) )
        ( IR.Pi
            one -- the second 1 in the annotation
            (IR.Pi one (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- the third 1 in the annotation 1 Nat -> Nat
            (IR.Pi one (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- the forth 1 in the annotation 1 Nat -> Nat
        )
    )
    ( IR.Elim
        ( IR.Ann
            one -- sig usage, the first 1 of the annotation
            identity -- annotation (1, 1 Nat -> Nat)
            (IR.Pi one (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- the second 1 of the annotation, 1 Nat -> Nat
        )
    )

kcombinator ∷ ∀ primTy primVal. IR.Term primTy primVal -- K = \x.\y.x
kcombinator = IR.Lam (IR.Lam (IR.Elim (IR.Bound 1)))

-- K has annotation (1, 1 Nat -> 0 Nat -> Nat )
kCompTy ∷ NatAnnotation
kCompTy =
  IR.Annotated
    one -- the sig usage of k
    ( IR.VPi -- first input, 1 Nat
        one -- is used once in the output
        (IR.VPrimTy Nat.Ty) -- of type Nat
        ( const
            ( pure
                ( IR.VPi -- second input, 0 Nat
                    mempty -- is not used in the output
                    (IR.VPrimTy Nat.Ty) -- of type Nat
                    (const (pure (IR.VPrimTy Nat.Ty))) -- the output is of type Nat
                )
            )
        )
    )

-- K computation annotation (1, 1 Nat -> 0 () -> Nat)
kCompTyWithUnit ∷ AllAnnotation
kCompTyWithUnit =
  IR.Annotated
    one -- sig usage of k
    ( IR.VPi -- first input, 1 Nat
        one -- is used once in the output
        (IR.VPrimTy (All.NatTy Nat.Ty)) -- of type Nat
        ( const
            ( pure
                ( IR.VPi -- second input, 0 Unit
                    mempty -- is not used in the output
                    (IR.VPrimTy (All.UnitTy Unit.Ty)) -- of type Unit
                    (const (pure (IR.VPrimTy (All.NatTy Nat.Ty)))) -- the output is of type Nat
                )
            )
        )
    )

-- I K computation annotation (1, 1 (1 Nat -> 0 Nat -> Nat) -> ( 1 Nat -> 0 Nat -> Nat) -> (1 Nat -> 0 Nat-> Nat) )
identityAppK ∷ NatElim
identityAppK =
  IR.App -- applying I to K
    ( IR.Ann -- I
        one -- sig usage, the first 1 in the annotation
        identity -- annotation (1, (1 Nat -> 0 Nat -> Nat) -> ( 1 Nat -> 0 Nat -> Nat) )
        ( IR.Pi
            one -- sig usage, the first 1 in the annotation
            ( IR.Pi
                one -- the second 1 in the annotation
                (IR.PrimTy Nat.Ty) -- (1 Nat ->
                (IR.Pi mempty (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- 0 Nat -> Nat)
                    -- ->
            )
            ( IR.Pi
                one
                (IR.PrimTy Nat.Ty) -- (1 Nat ->
                (IR.Pi mempty (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- 0 Nat -> Nat)
            )
        )
    ) -- K
    ( IR.Elim
        ( IR.Ann
            one -- sig usage
            kcombinator -- annotation (1, (1 Nat -> 0 Nat-> Nat))
            ( IR.Pi
                one
                (IR.PrimTy Nat.Ty) -- (1 Nat ->
                (IR.Pi mempty (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- 0 Nat -> Nat)
            )
        )
    )

-- (K: Nat -> Nat -> Nat 1) should type check to Nat -> Nat
kApp1 ∷ NatElim
kApp1 =
  IR.App -- applying K to 1
    ( IR.Ann -- K
        one -- sig usage
        kcombinator -- annotation (1, (1 Nat -> 0 Nat -> Nat))
        ( IR.Pi
            one
            (IR.PrimTy Nat.Ty) -- (1 Nat ->
            (IR.Pi mempty (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- 0 Nat -> Nat)
        )
    ) -- 1
    (IR.Elim (IR.Prim (Nat.Val 1)))

-- computation annotation (0 Nat -> Nat)
natToNatTy ∷ NatAnnotation
natToNatTy =
  IR.Annotated
    one -- sig usage
    (IR.VPi mempty (IR.VPrimTy Nat.Ty) (const (pure (IR.VPrimTy Nat.Ty)))) -- 0 Nat -> Nat

-- (1 (K: 1 Nat -> 0 (1 Nat -> Nat) -> Nat) 1) should type check to 0 (1 Nat -> Nat) -> Nat
kFunApp1 ∷ NatElim
kFunApp1 =
  IR.App -- applying K to 1
    ( IR.Ann
        one -- sig usage
        kcombinator -- annotation (1, (1 Nat -> 0 (1 Nat -> Nat) -> Nat))
        ( IR.Pi
            one
            (IR.PrimTy Nat.Ty) -- 1 Nat ->
            ( IR.Pi
                mempty -- usage of (1 Nat -> Nat )
                (IR.Pi one (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- (1 Nat -> Nat ) ->
                (IR.PrimTy Nat.Ty) -- Nat
            )
        )
    )
    (IR.Elim (IR.Prim (Nat.Val 1))) -- 1
          -- computation annotation (1, 0 (1 Nat -> Nat) -> Nat)

kFunApp1CompTy ∷ NatAnnotation
kFunApp1CompTy =
  IR.Annotated
    one
    ( IR.VPi
        mempty
        (IR.VPi one (IR.VPrimTy Nat.Ty) (const (pure (IR.VPrimTy Nat.Ty))))
        (const (pure (IR.VPrimTy Nat.Ty)))
    )

--1 K: 1 (1 Nat -> Nat) -> 0 Nat -> (1 Nat -> Nat) 1 I: 1 Nat -> Nat type checks to 0 Nat -> (1 Nat -> Nat)
kAppI ∷ NatElim
kAppI =
  IR.App -- applying K to I
    ( IR.Ann
        one -- sig usage
        kcombinator
        ( IR.Pi
            one -- usage of (1 Nat -> Nat)
            (IR.Pi one (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- (1 Nat -> Nat) ->
            ( IR.Pi
                mempty
                (IR.PrimTy Nat.Ty) -- 0 Nat ->
                (IR.Pi one (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- (1 Nat -> Nat)
            )
        )
    )
    ( IR.Elim
        ( IR.Ann -- I
            one -- usage of identity
            identity
            (IR.Pi one (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- 1 Nat -> Nat
        )
    )

--1 K: 1 (1 Nat -> Nat) -> 0 Nat -> (1 Nat -> Nat) I type checks to 0 Nat -> (1 Nat -> Nat)
kAppINotAnnotated ∷ NatElim
kAppINotAnnotated =
  IR.App
    ( IR.Ann
        one -- sig usage
        kcombinator
        ( IR.Pi
            one -- usage of (1 Nat -> Nat)
            (IR.Pi one (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- (1 Nat -> Nat) ->
            ( IR.Pi
                mempty
                (IR.PrimTy Nat.Ty) -- 0 Nat ->
                (IR.Pi one (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- (1 Nat -> Nat)
            )
        )
    )
    identity

-- computation annotation (1, 0 Nat -> (1 Nat -> Nat))
kAppICompTy ∷ NatAnnotation
kAppICompTy =
  IR.Annotated
    one -- sig usage
    ( IR.VPi
        mempty
        (IR.VPrimTy Nat.Ty) -- 0 Nat ->
        (const (pure (IR.VPi one (IR.VPrimTy Nat.Ty) (const (pure (IR.VPrimTy Nat.Ty)))))) -- (1 Nat -> Nat)
    )

-- dependent k, \t1.\t2.\x:t1.\y:t2.x 1: t1
depK ∷ ∀ primTy primVal. IR.Term primTy primVal
depK =
  IR.Lam -- first input t1, Bound 3 counting from output
    ( IR.Lam -- second input t2, Bound 2 counting from output
        ( IR.Lam -- third input x, Bound 1 counting from output
            ( IR.Lam -- forth input y, Bound 0 counting from output
                ( IR.Elim -- output
                    ( IR.Ann -- annotation is of
                        one -- 1 usage
                        (IR.Elim (IR.Bound 1)) -- x is the output, which has annotation (1, t)
                        (IR.Elim (IR.Bound 3)) -- of type t1
                    )
                )
            )
        )
    )

-- computation dependent k annotation
-- \t1.\t2.\x.\y.x 1: (t1 0: *0) -> (t2 0: *0) -> (x 1: t1) -> (y 0: t2) -> t1
depKCompTy ∷ AllAnnotation
depKCompTy =
  IR.Annotated
    one -- the sig usage of the dependent identity function
    ( IR.VPi -- the first input, t1
        mempty -- t1's usage
        (IR.VStar 0) -- t1's type
        ( const
            ( pure
                ( IR.VPi -- the second input, t2
                    mempty -- t2's usage
                    (IR.VStar 0) -- t2's type
                    ( const
                        ( pure
                            ( IR.VPi -- the third input, x
                                one -- x's usage
                                (IR.VNeutral (IR.NFree (IR.Local 0))) -- x's type, Local 0 is the first input (t1)
                                ( const
                                    ( pure
                                        ( IR.VPi -- the forth input, y
                                            mempty -- y's usage
                                            (IR.VNeutral (IR.NFree (IR.Local 1))) -- y's type, Local 1 is the second input (t2)
                                            ( const
                                                ( pure
                                                    (IR.VNeutral (IR.NFree (IR.Local 0))) -- output type is t1
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
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
                        one
                        ( IR.Elim
                            ( IR.App -- x applies to z
                                ( IR.Ann
                                    one -- usage of x
                                    (IR.Elim (IR.Bound 2)) -- x
                                    ( IR.Pi
                                        one
                                        (IR.PrimTy Nat.Ty)
                                        (IR.Pi mempty (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- Annotation of x: (1 Nat -> 0 Nat -> Nat)
                                    )
                                )
                                (IR.Elim (IR.Bound 0)) -- z
                            )
                        )
                        ( IR.Pi -- Annotation of xz: 0 Nat -> Nat
                            mempty
                            (IR.PrimTy Nat.Ty)
                            (IR.PrimTy Nat.Ty)
                        )
                    )
                    ( IR.Elim
                        ( IR.App -- y applies to z
                            ( IR.Ann
                                one -- usage of y
                                (IR.Elim (IR.Bound 1)) -- y
                                (IR.Pi one (IR.PrimTy Nat.Ty) (IR.PrimTy Nat.Ty)) -- Annotation of y, (1 Nat -> Nat)
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
  IR.Annotated
    one -- sig usage of S
    ( IR.VPi --
        one -- usage of 1 (1 Nat -> 0 Nat -> Nat)
        ( IR.VPi
            one
            (IR.VPrimTy Nat.Ty) -- (1 Nat ->
            ( const
                ( pure
                    ( IR.VPi
                        mempty
                        (IR.VPrimTy Nat.Ty) -- 0 Nat ->
                        (const (pure (IR.VPrimTy Nat.Ty))) -- Nat) ->
                    )
                )
            )
        )
        ( const
            ( pure
                ( IR.VPi -- second input, (1 Nat -> Nat)
                    one -- usage of (1 Nat -> Nat)
                    ( IR.VPi
                        one
                        (IR.VPrimTy Nat.Ty) --(1 Nat ->
                        (const (pure (IR.VPrimTy Nat.Ty))) -- Nat) ->
                    )
                    ( const
                        ( pure
                            ( IR.VPi
                                Usage.Omega
                                (IR.VPrimTy Nat.Ty) -- w Nat ->
                                (const (pure (IR.VPrimTy Nat.Ty))) -- Nat
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
  IR.Annotated one (IR.VPrimTy Nat.Ty)

add12 ∷ NatElim
add12 =
  IR.App
    (IR.App (IR.Prim Nat.Add) (IR.Elim (IR.Prim (Nat.Val 1))))
    (IR.Elim (IR.Prim (Nat.Val 2)))

sub52 ∷ NatTerm
sub52 =
  IR.Elim
    ( IR.App
        (IR.App (IR.Prim Nat.Sub) (IR.Elim (IR.Prim (Nat.Val 5))))
        (IR.Elim (IR.Prim (Nat.Val 2)))
    )

one' ∷ ∀ primTy primVal. IR.Term primTy primVal
one' = IR.Lam $ IR.Lam $ IR.Elim $ IR.App (IR.Bound 1) (IR.Elim (IR.Bound 0))

oneCompTy ∷ NatAnnotation
oneCompTy =
  IR.Annotated
    one
    ( IR.VPi
        one
        (IR.VPi one (IR.VPrimTy Nat.Ty) (const (pure (IR.VPrimTy Nat.Ty))))
        (const (pure (IR.VPi one (IR.VPrimTy Nat.Ty) (const (pure (IR.VPrimTy Nat.Ty))))))
    )

two ∷ ∀ primTy primVal. IR.Term primTy primVal
two =
  IR.Lam
    $ IR.Lam
    $ IR.Elim
    $ IR.App (IR.Bound 1) (IR.Elim (IR.App (IR.Bound 1) (IR.Elim (IR.Bound 0))))

twoCompTy ∷ NatAnnotation
twoCompTy =
  IR.Annotated
    one
    ( IR.VPi
        (Usage.SNat 2)
        (IR.VPi one (IR.VPrimTy Nat.Ty) (const (pure (IR.VPrimTy Nat.Ty))))
        (const (pure (IR.VPi one (IR.VPrimTy Nat.Ty) (const (pure (IR.VPrimTy Nat.Ty))))))
    )
