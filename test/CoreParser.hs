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

coreParser ∷ T.TestTree
coreParser =
  T.testGroup
    "Core parser"
    [ shouldParse "* 0" (Star 0),
      shouldParse "(* 1)" (Star 1),
      shouldParsePrim nat "Nat" (PrimTy Nat),
      shouldParsePrim unit "Unit" (PrimTy TUnit),
      shouldParsePrim unit "()" (Elim (Prim Unit)),
      shouldParse "[Π] 1 * 0 * 0" (Pi (SNat 1) (Star 0) (Star 0)),
      shouldParse "\\x -> x" (Lam "x" (Elim (Var "x"))),
      shouldParse "\\x -> y" (Lam "x" (Elim (Var "y"))),
      shouldParse "\\x -> \\y -> x" (Lam "x" (Lam "y" (Elim (Var "x")))),
      shouldParse
        "\\x -> \\y -> x y"
        (Lam "x" (Lam "y" (Elim (App (Var "x") (Elim (Var "y")))))),
      shouldParse "3" (Elim (Prim (Natural 3))),
      shouldParse "xyz" (Elim (Var "xyz")),
      shouldParse "fun var" (Elim (App (Var "fun") (Elim (Var "var")))),
      shouldParse "(fun var)" (Elim (App (Var "fun") (Elim (Var "var")))),
      shouldParse "@ (* 0) : w (* 0)" (Elim (Ann Omega (Star 0) (Star 0))),
      shouldParse
        "@ (\\x -> x) : w (* 0)"
        (Elim (Ann Omega (Lam "x" (Elim (Var "x"))) (Star 0))),
      shouldParse
        "(@ (\\x -> x) : w (* 0))"
        (Elim (Ann Omega (Lam "x" (Elim (Var "x"))) (Star 0))),
      shouldParse
        "(@ (\\x -> x) : w (* 0)) y"
        (Elim (App (Ann Omega (Lam "x" (Elim (Var "x"))) (Star 0)) (Elim (Var "y")))),
      shouldParse "(2)" (Elim (Prim (Natural 2))),
      shouldParse
        "(+ 3 4)"
        (Elim (App (App (Prim Add) (Elim (Prim (Natural 3)))) (Elim (Prim (Natural 4))))),
      shouldParse
        "(- 4 3)"
        (Elim (App (App (Prim Sub) (Elim (Prim (Natural 4)))) (Elim (Prim (Natural 3))))),
      shouldParse
        "(* 4 3)"
        (Elim (App (App (Prim Mul) (Elim (Prim (Natural 4)))) (Elim (Prim (Natural 3)))))
    ]
-- TODO: Fix this; currently only applications of eliminations can be parsed.
-- shouldParse "(\\x -> x) y" (Elim (App (Lam "x" (Elim (Var "x"))) (Elim (Var "y"))))
