-- | Tests for the evaluator in Core/IR/Evaluator.hs
module Evaluator where

import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.TransformExt as TransformExt
import qualified Juvix.Core.IR.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.Parameterisations.Naturals as Nat
import Juvix.Core.Types
import Juvix.Library hiding (identity)
import Juvix.Library.HashMap as Map
import qualified Juvix.Library.Usage as Usage
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Typechecker.Terms
import Typechecker.Types

type TestEval primTy primVal =
  ( HasCallStack,
    TestEvalPrim primTy primVal primTy,
    TestEvalPrim primTy primVal primVal,
    Eq (Eval.Error IR.NoExt IR.NoExt primTy primVal),
    Show (Eval.Error IR.NoExt IR.NoExt primTy primVal)
  )

type TestEvalPrim primTy primVal a =
  ( Eq a,
    Show a,
    CanApply a,
    Eval.HasPatSubstTerm (OnlyExts.T IR.NoExt) primTy primVal a,
    Eval.HasSubstValue IR.NoExt primTy primVal a
  )

-- unit test generator for evalTerm
shouldEval' ::
  TestEval primTy primVal =>
  T.TestName ->
  IR.Globals primTy primVal ->
  IR.Term primTy primVal ->
  IR.Value primTy primVal ->
  T.TestTree
shouldEval' name g term res =
  let look x = Map.lookup x g
   in T.testCase name $ IR.evalTerm look term T.@=? Right res

shouldEval ::
  TestEval primTy primVal =>
  T.TestName ->
  IR.Term primTy primVal ->
  IR.Value primTy primVal ->
  T.TestTree
shouldEval name = shouldEval' name mempty

evaluatorTests :: T.TestTree
evaluatorTests =
  T.testGroup
    "Evaluator tests"
    [ evalTests,
      toLambdaTests
    ]

evalTests :: T.TestTree
evalTests =
  T.testGroup
    "'eval' tests"
    [ shouldEval "1 + 2" add12 (natV 3),
      shouldEval "5 - 2" sub52 (natV 3),
      shouldEval "I 1" identityApplication (natV 1),
      shouldEval "I I 1" (IR.Elim identityAppINat1) (natV 1),
      shouldEval "I I" (IR.Elim identityAppI) videntity,
      shouldEval "K 1 2" (IR.Elim kApp1_2) (natV 1),
      shouldEval' "ty" typGlobals (name "ty") (IR.VStar 0),
      shouldEval' "tz" typGlobals (name "tz") (vname "tz"),
      shouldEval' "B" typGlobals (name "B") (vname "A"),
      shouldEval' "C" typGlobals (name "C") (vname "A")
    ]
  where
    add12 = IR.Elim $ add `IR.App` nat 1 `IR.App` nat 2
    sub52 = IR.Elim $ sub `IR.App` nat 5 `IR.App` nat 2
    sub = IR.Ann Usage.Omega (IR.Prim Nat.Sub) addTyT 0
    videntity = IR.VLam $ IR.VBound 0
    name = IR.Elim . IR.Free . IR.Global
    vname = IR.VFree . IR.Global

toLambda' ::
  Eval.EvalPatSubst IR.NoExt primTy primVal =>
  IR.Global primTy primVal ->
  Maybe (IR.Elim primTy primVal)
toLambda' g =
  TransformExt.extForgetE <$> Eval.toLambda @IR.NoExt g

type TestToLambda primTy primVal =
  ( Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Eval.EvalPatSubst IR.NoExt primTy primVal
  )

shouldFailToLambda ::
  TestToLambda primTy primVal =>
  T.TestName ->
  IR.Global primTy primVal ->
  T.TestTree
shouldFailToLambda name g = T.testCase msg $ maybe (pure ()) err $ toLambda' g
  where
    msg = "fails on '" <> name <> "'"
    err res = T.assertFailure $ "produced:\n" <> show res

shouldFailToLambdaN :: T.TestName -> NatGlobal -> T.TestTree
shouldFailToLambdaN = shouldFailToLambda

shouldSucceedToLambda ::
  TestToLambda primTy primVal =>
  T.TestName ->
  IR.Function primTy primVal ->
  IR.Elim primTy primVal ->
  T.TestTree
shouldSucceedToLambda name g res =
  T.testCase msg $ toLambda' (IR.GFunction g) T.@?= Just res
  where
    msg = "succeeds on '" <> name <> "'"

shouldSucceedToLambdaN :: T.TestName -> NatFunction -> NatElim -> T.TestTree
shouldSucceedToLambdaN = shouldSucceedToLambda

fun1 ::
  IR.GlobalName ->
  IR.Value primTy primVal ->
  [IR.Pattern primTy primVal] ->
  IR.Term primTy primVal ->
  IR.Function primTy primVal
fun1 name ty pats rhs =
  IR.Function name IR.GOmega ty (IR.FunClause pats rhs :| [])

funT ::
  IR.Term primTy primVal -> -- type
  IR.Term primTy primVal -> -- term
  IR.Elim primTy primVal
funT ty tm = IR.Ann Usage.Omega tm ty 0

pat :: IR.PatternVar -> IR.Term primTy primVal
pat = IR.Elim . IR.Free . IR.Pattern

bv :: IR.BoundVar -> IR.Term primTy primVal
bv = IR.Elim . IR.Bound

toLambdaTests :: T.TestTree
toLambdaTests =
  T.testGroup
    "toLambda"
    [ shouldFailToLambdaN "[abstract]" $ IR.GAbstract $
        IR.Abstract "blah" IR.GOmega (IR.VStar 0),
      shouldFailToLambdaN "[datatype]" $ IR.GDatatype $
        IR.Datatype "void" [] 0 [],
      shouldFailToLambdaN "[constructor]" $ IR.GDataCon $
        IR.DataCon "askfjlad" (IR.VStar 0),
      shouldSucceedToLambdaN
        "let const x y = x"
        ( fun1
            "const"
            (natT --> natT --> natT)
            [IR.PVar 0, IR.PVar 1]
            (pat 0)
        )
        ( funT
            (natT' ~~> natT' ~~> natT')
            (IR.Lam $ IR.Lam $ bv 1)
        ),
      shouldSucceedToLambdaN
        "let outer x y = {0}"
        ( fun1
            "outer"
            (natT --> natT --> natT)
            [IR.PVar 0, IR.PVar 1]
            (bv 0)
        )
        ( funT
            (natT' ~~> natT' ~~> natT')
            (IR.Lam $ IR.Lam $ bv 2)
        ),
      shouldSucceedToLambdaN
        "let lam x y = \\z -> x + z"
        ( fun1
            "outer"
            (natT --> natT --> natT --> natT)
            [IR.PVar 0, IR.PVar 1]
            (IR.Lam $ IR.Elim $ add @@ pat 0 @@ bv 0)
        )
        ( funT
            (natT' ~~> natT' ~~> natT' ~~> natT')
            (IR.Lam $ IR.Lam $ IR.Lam $ IR.Elim $ add @@ bv 2 @@ bv 0)
        ),
      shouldFailToLambdaN "let f x = x; let f x = 0" $ IR.GFunction
        $ IR.Function "f" IR.GOmega (natT --> natT)
        $ IR.FunClause [IR.PVar 0] (pat 0)
          :| IR.FunClause [IR.PVar 0] (nat 0) : [],
      shouldFailToLambdaN "let fst (pair x y) = x" $ IR.GFunction $
        -- this isn't the right type but that doesn't matter
        fun1
          "fst"
          (natT --> natT)
          [IR.PCon "pair" [IR.PVar 0, IR.PVar 1]]
          (pat 0)
    ]
