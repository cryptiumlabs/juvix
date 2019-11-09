module Backends.Michelson where

import Juvix.Backends.Michelson.Compilation
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
    (show (fst (compile term ty)) T.@=? contract)

--(show (fst (compile term ty)) T.@=? ((show (Right contract :: Either () M.SomeContract)) :: Text))

test_identity ∷ T.TestTree
test_identity = shouldCompile identityTerm identityType ""

{-
identityContract :: M.SomeContract
identityContract = M.SomeContract (MT.Seq MT.DUP MT.DROP)
  (MT.STPair (MT.STList MT.STOperation) MT.STUnit M.-:& M.SNil)
  (MT.STPair (MT.STList MT.STOperation) MT.STUnit M.-:& M.SNil)
-}

identityTerm ∷ Term
identityTerm = J.Lam "x" (J.Var "x")

identityType ∷ Type
identityType = J.Pi unit unit

unit ∷ Type
unit = J.PrimTy (PrimTy (M.Type M.TUnit ""))
