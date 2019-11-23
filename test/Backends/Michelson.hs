module Backends.Michelson where

import Juvix.Backends.Michelson.Compilation
import Juvix.Backends.Michelson.Optimisation
import Juvix.Backends.Michelson.Parameterisation
import qualified Juvix.Core.Erased.Types as J
import Juvix.Library hiding (Type)
import qualified Michelson.TypeCheck as M
import qualified Michelson.Typed as MT
import qualified Michelson.Untyped as M
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

shouldCompile ∷ Term → Type → Text → T.TestTree
shouldCompile term ty contract =
  T.testCase
    (show term <> " :: " <> show ty <> " should compile to " <> show contract)
    ((contractToSource |<< fst (compile term ty)) T.@=? Right contract)

shouldOptimise ∷ MT.Instr a b → MT.Instr a b → T.TestTree
shouldOptimise instr opt =
  T.testCase
    (show instr <> " should optimise to " <> show opt)
    (optimise' instr T.@=? Just opt)

test_optimise_dup_drop ∷ T.TestTree
test_optimise_dup_drop = shouldOptimise (MT.Seq MT.DUP MT.DROP) MT.Nop

test_identity ∷ T.TestTree
test_identity =
  shouldCompile
    identityTerm
    identityType
    "parameter unit;storage unit;code {{DUP; {DIP {{}}; {CAR; {NIL operation; {PAIR % %; {DIP {{DROP}}; {}}}}}}}};"

test_identity_app ∷ T.TestTree
test_identity_app = shouldCompile identityAppTerm identityType "parameter unit;storage unit;code {{DUP; {DIP {{}}; {CAR; {NIL operation; {PAIR % %; {DIP {{DROP}}; {}}}}}}}};"

--(show (fst (compile term ty)) T.@=? ((show (Right contract :: Either () M.SomeContract)) :: Text))
{-
identityContract :: M.SomeContract
identityContract = M.SomeContract (MT.Seq MT.DUP MT.DROP)
  (MT.STPair (MT.STList MT.STOperation) MT.STUnit M.-:& M.SNil)
  (MT.STPair (MT.STList MT.STOperation) MT.STUnit M.-:& M.SNil)
-}

identityTerm ∷ Term
identityTerm =
  J.Lam
    "x"
    ( J.App
        ( J.App
            (J.Prim PrimPair)
            (J.Prim (PrimConst M.ValueNil))
        )
        (J.App (J.Prim PrimFst) (J.Var "x"))
    )

identityAppTerm ∷ Term
identityAppTerm = J.Lam "x" (J.App (J.Lam "f" (J.App (J.App (J.Var "f") (J.Prim (PrimConst M.ValueNil))) (J.App (J.Prim PrimFst) (J.Var "x")))) (J.Prim PrimPair))

identityType ∷ Type
identityType = J.Pi (J.PrimTy (PrimTy (M.Type (M.TPair "" "" unit unit) ""))) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" opl unit) "")))

opl ∷ M.Type
opl = M.Type (M.TList (M.Type M.TOperation "")) ""

unit ∷ M.Type
unit = M.Type M.TUnit ""
