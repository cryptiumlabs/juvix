module Erasure where

import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Erased as Erased
import qualified Juvix.Core.Erasure as Erasure
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Typechecker as Typed
import qualified Juvix.Core.Parameterisations.Unit as Unit
import qualified Juvix.Core.Types as Core
import Juvix.Library hiding (identity)
import qualified Juvix.Library.Usage as Usage
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (String)

type ErasureType primTy primVal =
  Either (Erasure.Error primTy primVal) (Erasure.Term primTy primVal)

shouldEraseTo ::
  forall primTy primVal.
  ( Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    Eq (Core.ApplyErrorExtra (Typed.TypedPrim primTy primVal)),
    Show (Core.ApplyErrorExtra (Typed.TypedPrim primTy primVal))
  ) =>
  String ->
  Core.Parameterisation primTy primVal ->
  (Typed.Term primTy primVal, Usage.T) ->
  Erased.TermT primTy primVal ->
  T.TestTree
shouldEraseTo name _ (term, usage) erased =
  T.testCase
    (name <> ": " <> show (term, usage) <> " should erase to " <> show erased)
    ( Right erased
        T.@=? (Erasure.eraseAnn |<< Erasure.erase term usage)
    )

infix 0 `ann`

ann :: Usage.T -> IR.Value primTy primVal -> Typed.Annotation primTy primVal
ann = Typed.Annotation

bann ::
  Typed.Annotation primTy primVal ->
  Typed.Annotation primTy primVal ->
  Typed.BindAnnotation primTy primVal
bann = Typed.BindAnnotation

omega :: Usage.T
omega = Usage.Omega

omegaAnn :: IR.Value primTy primVal -> Typed.Annotation primTy primVal
omegaAnn = Typed.Annotation omega

zeroAnn :: IR.Value primTy primVal -> Typed.Annotation primTy primVal
zeroAnn = Typed.Annotation mempty

unitAnn :: Typed.AnnotationT Unit.Ty Unit.Val
unitAnn = omegaAnn unitTy

unitAnn0 :: Typed.AnnotationT Unit.Ty Unit.Val
unitAnn0 = zeroAnn unitTy

unitTy :: IR.ValueT Unit.Ty Unit.Val
unitTy = IR.VPrimTy Unit.Ty

unitTyT :: Typed.Term Unit.Ty Unit.Val
unitTyT = Typed.PrimTy Unit.Ty (zeroAnn $ IR.VStar 0)

erasureTests :: T.TestTree
erasureTests =
  T.testGroup
    "Erasure"
    [ identityUnit,
      constUnit,
      usedFst,
      unusedFst,
      usedArg,
      appUnusedArg,
      unusedFunction
    ]

identityUnit :: T.TestTree
identityUnit =
  shouldEraseTo
    "identityUnit"
    Unit.t
    (Typed.Prim unitVal' unitAnn, one)
    (Erased.Prim unitVal')

constUnit :: T.TestTree
constUnit =
  shouldEraseTo
    "constUnit"
    Unit.t
    ( Typed.Lam
        ( Typed.Lam
            (Typed.Elim (Typed.Bound 0 unitAnn) unitAnn)
            (bann unitAnn identityAnn)
        )
        (bann unitAnn0 (omegaAnn constTy)),
      one
    )
    (Erased.Lam "1" (Erased.Var "1"))

unusedFst :: T.TestTree
unusedFst =
  shouldEraseTo
    "unusedFst"
    Unit.t
    (Typed.Pair unitTerm unitTerm (one `ann` unitPairTy0), one)
    unitTermE

usedFst :: T.TestTree
usedFst =
  shouldEraseTo
    "usedFst"
    Unit.t
    (Typed.Pair unitTerm unitTerm (one `ann` unitPairTy1), one)
    (unitTermE `Erased.Pair` unitTermE)

usedArg :: T.TestTree
usedArg =
  shouldEraseTo
    "usedArg"
    Unit.t
    (appTerm, one)
    (Erased.Lam "0" (Erased.Lam "1" (Erased.App (Erased.Var "0") (Erased.Var "1"))))

appUnusedArg :: T.TestTree
appUnusedArg =
  shouldEraseTo
    "appUnusedArg"
    Unit.t
    ( Typed.Elim
        ( Typed.App
            ( Typed.Ann
                one
                constTerm
                constTyT
                0
                (omegaAnn constTy)
            )
            unitTerm
            identityAnn
        )
        identityAnn,
      one
    )
    (Erased.Lam "1" (Erased.Var "1"))

unusedFunction :: T.TestTree
unusedFunction =
  shouldEraseTo
    "unusedFunction"
    Unit.t
    ( Typed.Elim
        ( Typed.App
            ( Typed.Ann
                one
                constTerm
                constTy2T
                0
                (omegaAnn constTy2)
            )
            identityTerm
            identityAnn
        )
        identityAnn,
      one
    )
    (Erased.Lam "1" (Erased.Var "1"))

identityTerm :: Typed.Term Unit.Ty Unit.Val
identityTerm =
  Typed.Lam
    (Typed.Elim (Typed.Bound 0 unitAnn) unitAnn)
    (bann unitAnn identityAnn)

identityTy :: IR.ValueT Unit.Ty Unit.Val
identityTy = IR.VPi one unitTy unitTy

identityTyT :: Typed.Term Unit.Ty Unit.Val
identityTyT = Typed.Pi one unitTyT unitTyT (zeroAnn $ IR.VStar 0)

identityAnn :: Typed.AnnotationT Unit.Ty Unit.Val
identityAnn = omegaAnn identityTy

identityTy2 :: IR.ValueT Unit.Ty Unit.Val
identityTy2 = IR.VPi one identityTy identityTy

identityAnn2 :: Typed.AnnotationT Unit.Ty Unit.Val
identityAnn2 = omegaAnn identityTy2

appTerm :: Typed.Term Unit.Ty Unit.Val
appTerm =
  Typed.Lam
    ( Typed.Lam
        ( Typed.Elim
            ( Typed.App
                (Typed.Bound 1 identityAnn)
                ( Typed.Elim
                    (Typed.Bound 0 unitAnn)
                    unitAnn
                )
                unitAnn
            )
            unitAnn
        )
        ( bann
            unitAnn
            identityAnn
        )
    )
    ( bann
        unitAnn
        identityAnn2
    )

appTy :: IR.ValueT Unit.Ty Unit.Val
appTy = IR.VPi one identityTy identityTy

constTerm :: Typed.Term Unit.Ty Unit.Val
constTerm =
  Typed.Lam
    identityTerm
    ( bann
        unitAnn0
        (omegaAnn constTy)
    )

constTy :: IR.ValueT Unit.Ty Unit.Val
constTy = IR.VPi mempty unitTy identityTy

constTyT :: Typed.Term Unit.Ty Unit.Val
constTyT = Typed.Pi mempty unitTyT identityTyT (zeroAnn $ IR.VStar 0)

constTy2 :: IR.ValueT Unit.Ty Unit.Val
constTy2 = IR.VPi mempty identityTy identityTy

constTy2T :: Typed.Term Unit.Ty Unit.Val
constTy2T = Typed.Pi mempty identityTyT identityTyT (zeroAnn $ IR.VStar 0)

unitTerm :: Typed.Term Unit.Ty Unit.Val
unitTerm = Typed.Prim unitVal' unitAnn

unitElim :: Typed.Elim Unit.Ty Unit.Val
unitElim = Typed.Ann Usage.Omega unitTerm unitTyT 0 unitAnn

unitTermE :: Erased.TermT Unit.Ty Unit.Val
unitTermE = Erased.Prim unitVal'

unitPairTy0 :: IR.ValueT Unit.Ty Unit.Val
unitPairTy0 = IR.VSig mempty unitTy unitTy

unitPairTy1 :: IR.ValueT Unit.Ty Unit.Val
unitPairTy1 = IR.VSig one unitTy unitTy

unitVal' :: Typed.TypedPrim Unit.Ty Unit.Val
unitVal' = App.Return {retType = Unit.Ty :| [], retTerm = Unit.Val}
