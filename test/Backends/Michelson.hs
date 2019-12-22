module Backends.Michelson where

import Juvix.Backends.Michelson.Compilation
import Juvix.Backends.Michelson.Optimisation
import Juvix.Backends.Michelson.Parameterisation
import qualified Juvix.Core.ErasedAnn as J
import Juvix.Core.Usage
import Juvix.Library hiding (Type)
import qualified Michelson.Untyped as M
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

shouldCompile ∷ Term → Type → Text → T.TestTree
shouldCompile term ty contract =
  T.testCase
    (show term <> " :: " <> show ty <> " should compile to " <> show contract)
    (Right contract T.@=? ((untypedContractToSource . fst) |<< fst (compile term ty)))

shouldOptimise ∷ Op → Op → T.TestTree
shouldOptimise instr opt =
  T.testCase
    (show instr <> " should optimise to " <> show opt)
    (opt T.@=? optimiseSingle instr)

backendMichelson ∷ T.TestTree
backendMichelson =
  T.testGroup
    "Backend Michelson"
    [ optimiseDupDrop,
      optimiseLambdaExec,
      identityFn,
      identityApp,
      identityApp2
    ]

optimiseDupDrop ∷ T.TestTree
optimiseDupDrop = shouldOptimise (M.SeqEx [M.PrimEx (M.DUP ""), M.PrimEx M.DROP]) (M.SeqEx [])

optimiseLambdaExec ∷ T.TestTree
optimiseLambdaExec = shouldOptimise (M.SeqEx [M.PrimEx (M.LAMBDA "" (M.Type M.TUnit "") (M.Type M.TUnit "") []), M.PrimEx (M.EXEC "")]) (M.SeqEx [])

identityFn ∷ T.TestTree
identityFn =
  shouldCompile
    identityTerm
    identityType
    "parameter unit;storage unit;code {{PUSH (pair unit (lambda (pair (list operation) unit) (pair (pair (list operation) unit) (lambda (pair unit (pair (list operation) unit)) (pair (list operation) unit))))) (Pair Unit {{DIP {PUSH (lambda (pair unit (pair (list operation) unit)) (pair (list operation) unit)) {{DUP; CAR; DIP {CDR; CAR}; SWAP; PAIR % %}}}; PAIR % %}}); {NIL operation; {DIP {{DUP; CAR; DIP {CDR}}}; {PAIR % %; {EXEC; {PUSH (pair unit (lambda (pair (pair unit unit) unit) unit)) (Pair Unit {CAR; CAR}); {DIP {SWAP}; {SWAP; {DUP; {DIP {{SWAP; DIP {SWAP}}}; {DIP {{DUP; CAR; DIP {CDR}}}; {PAIR % %; {EXEC; {DIP {{DUP; CAR; DIP {CDR}}}; {PAIR % %; {EXEC; {DIP {DROP}; {}}}}}}}}}}}}}}}}}}};"

identityApp ∷ T.TestTree
identityApp =
  shouldCompile
    identityAppTerm
    identityType
    "parameter unit;storage unit;code {{PUSH (lambda (pair (pair unit unit) unit) (pair (list operation) unit)) {{CAR}; {{PUSH (pair unit (lambda (pair (list operation) unit) (pair (pair (list operation) unit) (lambda (pair unit (pair (list operation) unit)) (pair (list operation) unit))))) (Pair Unit {{DIP {PUSH (lambda (pair unit (pair (list operation) unit)) (pair (list operation) unit)) {{DUP; CAR; DIP {CDR; CAR}; SWAP; PAIR % %}}}; PAIR % %}}); NIL operation; DIP {{DUP; CAR; DIP {CDR}}}; PAIR % %; EXEC}; {PUSH (pair unit (lambda (pair (pair unit unit) unit) unit)) (Pair Unit {CAR; CAR}); {{DIP {SWAP}; SWAP}; DUP; DIP {{SWAP; DIP {SWAP}}}}; DIP {{DUP; CAR; DIP {CDR}}}; PAIR % %; EXEC}; DIP {{DUP; CAR; DIP {CDR}}}; PAIR % %; EXEC}; DIP {DROP}}; {PUSH unit Unit; {PAIR % %; {SWAP; {DUP; {DIP {SWAP}; {DIP {{DUP; CAR; DIP {CDR}}}; {PAIR % %; {EXEC; {DIP {DROP}; {}}}}}}}}}}}};"

identityApp2 ∷ T.TestTree
identityApp2 =
  shouldCompile
    identityAppTerm2
    identityType
    ""

identityTerm ∷ Term
identityTerm =
  ( J.Lam
      "x"
      ( J.App
          ( J.App
              ( J.Prim PrimPair,
                SNat 1,
                J.Pi
                  (SNat 1)
                  (J.PrimTy (PrimTy (M.Type (M.TList (M.Type M.TOperation "")) "")))
                  (J.Pi (SNat 1) (J.PrimTy (PrimTy (M.Type M.TUnit ""))) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type (M.TList (M.Type M.TOperation "")) "") (M.Type M.TUnit "")) ""))))
              )
              (J.Prim (PrimConst M.ValueNil), SNat 1, J.PrimTy (PrimTy (M.Type (M.TList (M.Type M.TOperation "")) ""))),
            SNat 1,
            J.Pi (SNat 1) (J.PrimTy (PrimTy (M.Type M.TUnit ""))) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type (M.TList (M.Type M.TOperation "")) "") (M.Type M.TUnit "")) "")))
          )
          ( J.App
              (J.Prim PrimFst, SNat 1, J.Pi (SNat 1) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))) (J.PrimTy (PrimTy (M.Type M.TUnit ""))))
              (J.Var "x", SNat 1, J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))),
            SNat 1,
            J.PrimTy (PrimTy (M.Type M.TUnit ""))
          ),
        SNat 1,
        J.PrimTy (PrimTy (M.Type (M.TPair "" "" opl unit) ""))
      ),
    SNat 1,
    identityType
  )

primLam ∷ NonEmpty M.Type → Type
primLam (ty :| []) = J.PrimTy (PrimTy ty)
primLam (ty :| (t : ts)) = J.Pi (SNat 1) (J.PrimTy (PrimTy ty)) (primLam (t :| ts))

primPairTy =
  J.Pi
    (SNat 1)
    (J.PrimTy (PrimTy (M.Type (M.TList (M.Type M.TOperation "")) "")))
    (J.Pi (SNat 1) (J.PrimTy (PrimTy (M.Type M.TUnit ""))) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type (M.TList (M.Type M.TOperation "")) "") (M.Type M.TUnit "")) ""))))

identityAppTerm ∷ Term
identityAppTerm =
  ( J.Lam
      "y"
      ( J.App
          ( J.Lam
              "x"
              ( J.App
                  ( J.App
                      ( J.Prim PrimPair,
                        SNat 1,
                        primPairTy
                      )
                      (J.Prim (PrimConst M.ValueNil), SNat 1, J.PrimTy (PrimTy (M.Type (M.TList (M.Type M.TOperation "")) ""))),
                    SNat 1,
                    J.Pi (SNat 1) (J.PrimTy (PrimTy (M.Type M.TUnit ""))) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type (M.TList (M.Type M.TOperation "")) "") (M.Type M.TUnit "")) "")))
                  )
                  ( J.App
                      (J.Prim PrimFst, SNat 1, J.Pi (SNat 1) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))) (J.PrimTy (PrimTy (M.Type M.TUnit ""))))
                      (J.Var "x", SNat 1, J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))),
                    SNat 1,
                    J.PrimTy (PrimTy (M.Type M.TUnit ""))
                  ),
                SNat 1,
                J.PrimTy (PrimTy (M.Type (M.TPair "" "" opl unit) ""))
              ),
            SNat 1,
            identityType
          )
          (J.Var "y", SNat 1, J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))),
        SNat 1,
        J.PrimTy (PrimTy (M.Type (M.TPair "" "" opl unit) ""))
      ),
    SNat 1,
    identityType
  )

identityAppTerm2 ∷ Term
identityAppTerm2 =
  ( J.Lam
      "x"
      ( J.App
          ( J.Lam
              "f"
              ( J.App
                  ( J.App
                      ( J.Var "f",
                        SNat 1,
                        primPairTy
                      )
                      (J.Prim (PrimConst M.ValueNil), SNat 1, J.PrimTy (PrimTy (M.Type (M.TList (M.Type M.TOperation "")) ""))),
                    SNat 1,
                    J.Pi (SNat 1) (J.PrimTy (PrimTy (M.Type M.TUnit ""))) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type (M.TList (M.Type M.TOperation "")) "") (M.Type M.TUnit "")) "")))
                  )
                  ( J.App
                      (J.Prim PrimFst, SNat 1, J.Pi (SNat 1) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))) (J.PrimTy (PrimTy (M.Type M.TUnit ""))))
                      (J.Var "x", SNat 1, J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))),
                    SNat 1,
                    J.PrimTy (PrimTy (M.Type M.TUnit ""))
                  ),
                SNat 1,
                J.PrimTy (PrimTy (M.Type (M.TPair "" "" opl unit) ""))
              ),
            SNat 1,
            J.Pi (SNat 1) primPairTy (J.PrimTy (PrimTy (M.Type (M.TPair "" "" opl unit) "")))
          )
          (J.Prim PrimPair, SNat 1, primPairTy),
        SNat 1,
        J.PrimTy (PrimTy (M.Type (M.TPair "" "" opl unit) ""))
      ),
    SNat 1,
    identityType
  )

fstTy ∷ Type
fstTy = J.Pi (SNat 1) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))) (J.PrimTy (PrimTy (M.Type M.TUnit "")))

pairTy ∷ Type
pairTy =
  J.Pi
    (SNat 1)
    (J.PrimTy (PrimTy (M.Type (M.TList (M.Type M.TOperation "")) "")))
    (J.Pi (SNat 1) (J.PrimTy (PrimTy (M.Type M.TUnit ""))) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type (M.TList (M.Type M.TOperation "")) "") (M.Type M.TUnit "")) ""))))

identityType ∷ Type
identityType = J.Pi Omega (J.PrimTy (PrimTy (M.Type (M.TPair "" "" unit unit) ""))) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" opl unit) "")))

opl ∷ M.Type
opl = M.Type (M.TList (M.Type M.TOperation "")) ""

unit ∷ M.Type
unit = M.Type M.TUnit ""
