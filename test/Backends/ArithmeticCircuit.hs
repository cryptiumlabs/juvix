module Backends.ArithmeticCircuit where

import Juvix.Backends.ArithmeticCircuit.Compilation as Base
import Juvix.Backends.ArithmeticCircuit.Parameterisation
import Juvix.Backends.ArithmeticCircuit.ZKP as Zkp
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation as Par
import qualified Juvix.Core.ErasedAnn as J
import Juvix.Core.Usage
import Juvix.Library hiding (Type)
import qualified Circuit as Circ
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

shouldCompile :: Term → Type → Circ.ArithCircuit Par.F → T.TestTree
shouldCompile term ty circuit =
  T.testCase
    (show term <> " :: " <> show ty <> " should compile to " <> show circuit)
    (Base.compile term ty T.@=? circuit)

shouldProve :: Term → Type → Circ.ArithCircuit Par.F → [(Int, Par.F)] → T.TestTree
shouldProve term ty circuit params =
  T.testCase
    (show term <> " :: " <> show ty <> " should verify proof for " <> show circuit)
    (do
        setup <- Zkp.runSetup circuit
        proof <- Zkp.prove term ty params setup
        proof' <- Zkp.runProve circuit params setup
        let ver = Zkp.verify params setup proof
            ver' = Zkp.runVerify params setup proof'
        ver T.@=? ver'
    )

backendCircuit :: T.TestTree
backendCircuit =
  T.testGroup
    "Backend arithmetic circuit"
    [ shouldCompile equalTerm equalType equalCircuit
    , shouldProve equalTerm equalType equalCircuit [(0, 1)]
    , shouldCompile squareRoot equalType squareRootCircuit
    , shouldProve squareRoot equalType squareRootCircuit [(0, 1), (1, 1)]
    , shouldCompile (quadratic 1 1) equalType (quadraticCircuit 1 1)
    , shouldProve (quadratic 1 1) equalType (quadraticCircuit 1 1) [(0, 1), (1, 1)]
    , shouldCompile disjointKnowledge equalType disjointKnowledgeCircuit
    , shouldProve disjointKnowledge equalType disjointKnowledgeCircuit [(0, 1), (1, 1)]
    , shouldCompile disjointKnowledge' equalType disjointKnowledgeCircuit'
    , shouldProve disjointKnowledge' equalType disjointKnowledgeCircuit' [(0, 1), (1, 1)]
    -- , shouldCompile preimage equalTypes preimageCircuit
    -- , shouldProve preimage equalTypes preimageCircuit
    ]

runCirc :: Num f => Circ.Expr Circ.Wire f ty -> Circ.ArithCircuit f
runCirc = Circ.execCircuitBuilder . Circ.compile

equalTerm :: Term
equalTerm = Base.lambda ["x"] (Base.var "x")

equalType :: Type
equalType = J.Pi Omega (J.PrimTy ()) (J.PrimTy ())

equalCircuit :: Circ.ArithCircuit Par.F
equalCircuit = Base.runCirc (Base.input 0)

squareRoot :: Base.Term
squareRoot = Base.lambda ["x", "w"]
                    (Base.eq (Base.exp (Base.var "w") (Base.int 2)) (Base.var "x"))

squareRootCircuit :: Circ.ArithCircuit Par.F
squareRootCircuit = Base.runCirc
  (let x = Base.input 0
       w = Base.input 1
       sqr = Circ.mul w w
    in Circ.eq sqr x)

quadratic :: Par.F → Par.F → Term
quadratic a b = Base.lambda ["x", "w"]
                (Base.eq
                  (Base.sub
                    (Base.add
                      (Base.mul (Base.c a) (Base.exp (Base.var "w") (Base.int 2)))
                      (Base.mul (Base.c b) (Base.var "w")))
                    (Base.var "x"))
                  (Base.c 0))

quadraticCircuit :: Par.F → Par.F → Circ.ArithCircuit Par.F
quadraticCircuit a b = Base.runCirc
  (let x = Base.input 0
       w = Base.input 1
       ca = Circ.c a
       cb = Circ.c b
       zero = Circ.c 0
       w2 = Circ.mul w w
       r1 = Circ.mul ca w2
       r2 = Circ.mul cb w
       r3 = Circ.add r1 r2
       r4 = Circ.sub r3 x
    in Circ.eq r4 zero)

disjointKnowledge :: Term
disjointKnowledge =  Base.lambda ["x", "w"]
                     (Base.and'
                       (Base.eq (Base.exp (Base.var "w") (Base.int 2)) (Base.var "x"))
                       (Base.eq (Base.exp (Base.var "w") (Base.int 3)) (Base.add (Base.var "x") (Base.c 4))))

disjointKnowledgeCircuit :: Circ.ArithCircuit Par.F
disjointKnowledgeCircuit = Base.runCirc
  (let x = Base.input 0
       w = Base.input 1
       four = Circ.c 4
       r0 = Circ.mul w w
       r1 = Circ.eq r0 x
       r2 = Circ.mul w (Circ.mul w w)
       r3 = Circ.add x four
       r4 = Circ.eq r2 r3
    in Circ.and_ r1 r4)

disjointKnowledge' :: Term
disjointKnowledge' =  Base.lambda ["x", "w"]
                      (Base.or'
                        (Base.eq (Base.exp (Base.var "w") (Base.int 3)) (Base.var "x"))
                        (Base.eq (Base.exp (Base.var "w") (Base.int 4)) (Base.sub (Base.var "x") (Base.c 2))))

disjointKnowledgeCircuit' :: Circ.ArithCircuit Par.F
disjointKnowledgeCircuit' = Base.runCirc
  (let x = Base.input 0
       w = Base.input 1
       two = Circ.c 2
       r0 = Circ.mul w (Circ.mul w w)
       r1 = Circ.eq r0 x
       r2 = Circ.mul w (Circ.mul w (Circ.mul w w))
       r3 = Circ.sub x two
       r4 = Circ.eq r2 r3
    in Circ.or_ r1 r4)
