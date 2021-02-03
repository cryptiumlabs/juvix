-- | Tests for the type checker in Core/IR/Typechecker.hs
module Typechecker where

import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Typechecker as TC
import qualified Juvix.Core.Parameterisations.All as All
import qualified Juvix.Core.Parameterisations.Naturals as Nat
import qualified Juvix.Core.Parameterisations.Unit as Unit
import Juvix.Core.Types
import Juvix.Library hiding (identity)
import qualified Juvix.Library.Usage as Usage
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Typechecker.Terms
import Typechecker.Types

assertIsRight :: (HasCallStack, Show a) => Either a b -> T.Assertion
assertIsRight (Right _) = pure ()
assertIsRight (Left l) =
  T.assertFailure $
    "expected a Right, got\n\t"
      ++ "Left ("
      ++ show l
      ++ ")"

type TestCheck primTy primVal =
  ( HasCallStack,
    TestCheckPrim primTy,
    TestCheckPrimBase primVal,
    TestCheckPrimExt (TypedPrim primTy primVal),
    TC.PrimSubstValue primTy primVal,
    TC.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal
  )

type TestCheckPrimBase a = (Eq a, Show a)

type TestCheckPrimExt a =
  ( CanApply a,
    TestCheckPrimBase (Arg a),
    TestCheckPrimBase (ApplyErrorExtra a)
  )

type TestCheckPrim a = (TestCheckPrimBase a, TestCheckPrimExt a)

shouldCheckWith' ::
  TestCheck primTy primVal =>
  T.TestName ->
  Parameterisation primTy primVal ->
  IR.GlobalsT primTy primVal ->
  IR.Context primTy primVal ->
  IR.Term primTy primVal ->
  IR.AnnotationT primTy primVal ->
  T.TestTree
shouldCheckWith' msg param globals ctx term ann =
  -- TODO: take out the logs and put them in an IO monad.
  let (res, _) = TC.exec globals $ TC.typeTermWith param mempty ctx term ann
   in T.testCase msg $ assertIsRight res

shouldCheckWith ::
  TestCheck primTy primVal =>
  T.TestName ->
  Parameterisation primTy primVal ->
  IR.GlobalsT primTy primVal ->
  IR.Context primTy primVal ->
  IR.Term primTy primVal ->
  IR.AnnotationT primTy primVal ->
  T.TestTree
shouldCheckWith name = shouldCheckWith' $ name <> " type-checks"

shouldCheck ::
  TestCheck primTy primVal =>
  T.TestName ->
  Parameterisation primTy primVal ->
  IR.Term primTy primVal ->
  IR.AnnotationT primTy primVal ->
  T.TestTree
shouldCheck name param = shouldCheckWith name param mempty []

shouldInferWith' ::
  TestCheck primTy primVal =>
  T.TestName ->
  Parameterisation primTy primVal ->
  IR.GlobalsT primTy primVal ->
  IR.Context primTy primVal ->
  IR.Elim primTy primVal ->
  IR.AnnotationT primTy primVal ->
  T.TestTree
shouldInferWith' msg param globals ctx elim ann@(IR.Annotation {annUsage = σ}) =
  let (res, _) = TC.exec globals $ TC.typeElimWith param mempty ctx elim σ
      resTy = TC.getElimAnn . TC.loValue <$> res
   in T.testCase msg $ resTy T.@?= Right ann

shouldInferWith ::
  TestCheck primTy primVal =>
  T.TestName ->
  Parameterisation primTy primVal ->
  IR.GlobalsT primTy primVal ->
  IR.Context primTy primVal ->
  IR.Elim primTy primVal ->
  IR.AnnotationT primTy primVal ->
  T.TestTree
shouldInferWith name = shouldInferWith' $ name <> " can be inferred"

shouldInfer ::
  TestCheck primTy primVal =>
  T.TestName ->
  Parameterisation primTy primVal ->
  IR.Elim primTy primVal ->
  IR.AnnotationT primTy primVal ->
  T.TestTree
shouldInfer name param = shouldInferWith name param mempty []

typecheckerTests :: T.TestTree
typecheckerTests =
  T.testGroup
    "Core type checker tests"
    [ skiComp,
      natComp,
      dependentFunctionComp,
      dependentPairComp,
      letComp,
      skiCont,
      subtype
    ]

skiComp :: T.TestTree
skiComp =
  T.testGroup
    "SKI combinators Computational typing"
    [ shouldCheck "I [Nat]" Nat.t identity identityNatCompTy,
      shouldCheck "I [Unit]" Unit.t identity identityUnitCompTy,
      shouldCheck "I 1" Nat.t identityApplication natTy,
      shouldInfer "I I 1" Nat.t identityAppINat1 natTy,
      shouldInfer "I I" Nat.t identityAppI identityNatCompTy,
      shouldCheck "K [Nat]" Nat.t kcombinator kCompTy,
      shouldCheck "K [All]" All.t kcombinator kCompTyWithUnit,
      shouldInfer "I K" Nat.t identityAppK kCompTy,
      shouldCheck "K I: …" Nat.t (IR.Elim kAppI) kAppICompTy,
      shouldCheck "K I" Nat.t (IR.Elim kAppINotAnnotated) kAppICompTy,
      shouldInfer "K 1" Nat.t kApp1 natToNatTy,
      shouldInfer "K' 1" Nat.t kFunApp1 kFunApp1CompTy
    ]

natComp :: T.TestTree
natComp =
  T.testGroup
    "Nat Computational typing"
    [ shouldCheck "Nat" Nat.t natT' (mempty `ann` IR.VStar 0),
      shouldCheck "1" Nat.t (nat 1) (Usage.Omega `ann` natT),
      shouldCheck "plus" Nat.t (IR.Prim Nat.Add) (Usage.Omega `ann` addTy)
    ]

dependentFunctionComp :: T.TestTree
dependentFunctionComp =
  T.testGroup
    "Dependent Functions Computational typing"
    [ shouldCheck "λA x. x" All.t depIdentity depIdentityCompTy,
      shouldCheck "λA x. x [with ω]" All.t depIdentity depIdentityCompTyOmega,
      shouldCheck "λA B x y. x" All.t depK depKCompTy
    ]

letComp :: T.TestTree
letComp =
  T.testGroup
    "'let' Computational typing"
    [ shouldCheck
        "let 0 x = 0 in 0"
        Nat.t
        (IR.Let mempty nzero (IR.Elim nzero))
        (Usage.Omega `ann` natT),
      shouldCheck
        "let ω x = 0 in x"
        Nat.t
        (IR.Let Usage.Omega nzero (IR.Elim (IR.Bound 0)))
        (Usage.Omega `ann` natT),
      shouldCheck
        "λx. let 0 y = 0 in x"
        Nat.t
        (IR.Lam (IR.Let mempty nzero (IR.Elim (IR.Bound 1))))
        (natToNatTy' one)
    ]
  where
    nzero = IR.Ann Usage.Omega (nat 0) natT' 0

skiCont :: T.TestTree
skiCont =
  T.testGroup
    "SKI combinators contemplational typing"
    [ shouldCheck "I" Nat.t identity identityNatContTy
    ]

subtype :: T.TestTree
subtype =
  T.testGroup
    "Subtyping"
    [ shouldSub "*₀" "*₀" aTerm (IR.VStar 0),
      shouldSub "*₀" "*₁" aTerm (IR.VStar 1),
      shouldSub "(*₁ → *₁)" "(*₁ → *₁)" fTerm (1 ~~> 1),
      shouldSub "(*₁ → *₁)" "(*₀ → *₁)" fTerm (0 ~~> 1),
      shouldSub "(*₁ → *₁)" "(*₁ → *₂)" fTerm (1 ~~> 2),
      shouldSubI "*₀" "*₁" faElim (IR.VStar 1)
    ]
  where
    i ~~> j = IR.VPi mempty (IR.VStar i) (IR.VStar j)
    ty = ann mempty
    shouldSub a b s t =
      shouldCheckWith' (a <> " <: " <> b) Unit.t typGlobals [] s $ ty t
    shouldSubI a b s t =
      shouldInferWith' (a <> " <: " <> b) Unit.t typGlobals [] s $ ty t

dependentPairComp :: T.TestTree
dependentPairComp =
  T.testGroup "Dependent pair typing" $
    [shouldCheck "Nat, 1 : Σ(A: *₀). A" Nat.t boxNat boxNatAnn]
