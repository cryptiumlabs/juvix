module Backends.ArithmeticCircuit where

import Juvix.Backends.ArithmeticCircuit.Compilation as Base
import Juvix.Backends.ArithmeticCircuit.Compilation.Types
import Juvix.Backends.ArithmeticCircuit.Parameterisation
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation as Par
import qualified Juvix.Core.ErasedAnn as J
import Juvix.Core.Usage
import Juvix.Library hiding (Type)
import qualified Circuit
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

shouldCompile :: Term → Type → Circuit.ArithCircuit Par.F → T.TestTree
shouldCompile term ty circuit =
  T.testCase
    (show term <> " :: " <> show ty <> " should compile to " <> show circuit)
    (Base.compile term ty T.@=? Just circuit)

backendCircuit :: T.TestTree
backendCircuit =
  T.testGroup
    "Backend arithmetic circuit"
    [shouldCompile equalTerm equalType equalCircuit]

equalTerm :: Term
equalTerm = J.Lam "x" (J.Var "x")

equalType :: Type
equalType = J.Pi Omega (J.PrimTy ()) (J.PrimTy ())

power :: Term
power = J.LamM "x" (J.LamM "w" (Base.eq (Base.exp (J.Var "w") (Base.c 2)) (J.Var "x")))

quadratic :: Parameterisation.Integer a b => a → b → Term
quadratic a b = J.LamM "x" (J.LamM "w"
                           (Base.eq
                            (Base.sub
                              (Base.add
                               (Base.mul a (Base.exp (J.Var "w") (Base.int 2)))
                               (Base.mul b (J.Var "w"))
                              (J.Var "x"))
                           (Base.c 0))))

disjointKnowledge =  J.LamM "x" (J.LamM "w"
                                (Base.and
                                 (Base.eq (Base.exp (J.Var "w") (Base.c 2)) (J.Var "x")))
                                 (Base.eq (Base.exp (J.Var "w") (Base.c 3)) (Base.add (J.Var "x") (Base.c 4)))
                               )

disjointKnowledge' =  J.LamM "x" (J.LamM "w"
                                (Base.or
                                 (Base.eq (Base.exp (J.Var "w") (Base.c 3)) (J.Var "x"))
                                 (Base.eq (Base.exp (J.Var "w") (Base.c 4)) (Base.sub (J.Var "x") (Base.c 2))))
                                )

preimage =  J.LamM "x" (J.LamM "w"
                       (Base.eq (mimc (J.Var "w")) (J.Var "x"))
                      )

mimc :: Term → Term
mimc = undefined
