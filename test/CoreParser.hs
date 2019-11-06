module CoreParser where

import Juvix.Core.HR
import Juvix.Core.Parameterisations.Naturals
import Juvix.Core.Parameterisations.Unit
import Juvix.Core.Types
import Juvix.Core.Usage
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (String)

-- | Term parser unit test generator. TODO: does it make sense for it to be NatTy?
shouldParse ∷ String → Term NatTy NatVal → T.TestTree
shouldParse str parsed =
  T.testCase (show str <> " should parse as " <> show parsed) $
    Just parsed T.@=? parseString str

parseString ∷ String → Maybe (Term NatTy NatVal)
parseString = generateParser nat

-- | Primitive parser unit test generator.
shouldParsePrim ∷
  ∀ primTy primVal.
  (Show primTy, Eq primTy, Show primVal, Eq primVal) ⇒
  Parameterisation primTy primVal →
  String →
  Term primTy primVal →
  T.TestTree
shouldParsePrim param str parsed =
  T.testCase (show str <> " should parse as " <> show parsed) $
    Just parsed T.@=? parseStringPrim param str

parseStringPrim ∷
  ∀ primTy primVal.
  Parameterisation primTy primVal →
  String →
  Maybe (Term primTy primVal)
parseStringPrim = generateParser

test_star_n ∷ T.TestTree
test_star_n = shouldParse "* 0" (Star 0)

test_star_n_parens ∷ T.TestTree
test_star_n_parens = shouldParse "(* 1)" (Star 1)

test_primitive_type_Nats ∷ T.TestTree
test_primitive_type_Nats = shouldParsePrim nat "Nat" (PrimTy Nat)

test_primitive_type_unit ∷ T.TestTree
test_primitive_type_unit = shouldParsePrim unit "Unit" (PrimTy TUnit)

test_primitive_val_unit ∷ T.TestTree
test_primitive_val_unit = shouldParsePrim unit "()" (Elim (Prim Unit))

test_dependent_fun ∷ T.TestTree
test_dependent_fun = shouldParse "[Π] 1 * 0 * 0" (Pi (SNat 1) (Star 0) (Star 0))

test_lam_identity ∷ T.TestTree
test_lam_identity = shouldParse "\\x -> x" (Lam "x" (Elim (Var "x")))

test_lam_basic ∷ T.TestTree
test_lam_basic = shouldParse "\\x -> y" (Lam "x" (Elim (Var "y")))

test_lam_nested ∷ T.TestTree
test_lam_nested =
  shouldParse "\\x -> \\y -> x" (Lam "x" (Lam "y" (Elim (Var "x"))))

test_lam_nested_app ∷ T.TestTree
test_lam_nested_app =
  shouldParse
    "\\x -> \\y -> x y"
    (Lam "x" (Lam "y" (Elim (App (Var "x") (Elim (Var "y"))))))

test_parse_nat_lit ∷ T.TestTree
test_parse_nat_lit = shouldParse "3" (Elim (Prim (Natural 3)))

-- TODO: Fix this; currently only applications of eliminations can be parsed.
-- test_parse_app ∷ T.TestTree
-- test_parse_app = shouldParse "(\\x -> x) y" (Elim (App (Lam "x" (Elim (Var "x"))) (Elim (Var "y"))))
test_silent_convert_var ∷ T.TestTree
test_silent_convert_var = shouldParse "xyz" (Elim (Var "xyz"))

test_silent_convert_app ∷ T.TestTree
test_silent_convert_app =
  shouldParse "fun var" (Elim (App (Var "fun") (Elim (Var "var"))))

test_app_parens ∷ T.TestTree
test_app_parens =
  shouldParse "(fun var)" (Elim (App (Var "fun") (Elim (Var "var"))))

test_silent_convert_ann ∷ T.TestTree
test_silent_convert_ann =
  shouldParse "@ (* 0) : w (* 0)" (Elim (Ann Omega (Star 0) (Star 0)))

test_ann_func ∷ T.TestTree
test_ann_func =
  shouldParse
    "@ (\\x -> x) : w (* 0)"
    (Elim (Ann Omega (Lam "x" (Elim (Var "x"))) (Star 0)))

test_ann_func_parens ∷ T.TestTree
test_ann_func_parens =
  shouldParse
    "(@ (\\x -> x) : w (* 0))"
    (Elim (Ann Omega (Lam "x" (Elim (Var "x"))) (Star 0)))

test_app_ann ∷ T.TestTree
test_app_ann =
  shouldParse
    "(@ (\\x -> x) : w (* 0)) y"
    (Elim (App (Ann Omega (Lam "x" (Elim (Var "x"))) (Star 0)) (Elim (Var "y"))))

test_2_paren ∷ T.TestTree
test_2_paren = shouldParse "(2)" (Elim (Prim (Natural 2)))

test_parse_add ∷ T.TestTree
test_parse_add =
  shouldParse
    "(+ 3 4)"
    ( Elim
        (App (App (Prim Add) (Elim (Prim (Natural 3)))) (Elim (Prim (Natural 4))))
    )

test_parse_sub ∷ T.TestTree
test_parse_sub =
  shouldParse
    "(- 4 3)"
    ( Elim
        (App (App (Prim Sub) (Elim (Prim (Natural 4)))) (Elim (Prim (Natural 3))))
    )

test_parse_mul ∷ T.TestTree
test_parse_mul =
  shouldParse
    "(* 4 3)"
    ( Elim
        (App (App (Prim Mul) (Elim (Prim (Natural 4)))) (Elim (Prim (Natural 3))))
    )
