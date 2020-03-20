module Backends.Michelson where

import Juvix.Backends.Michelson.Compilation
import Juvix.Backends.Michelson.Compilation.Types
import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions
import qualified Juvix.Backends.Michelson.DSL.Untyped as Untyped
import Juvix.Backends.Michelson.Optimisation
import qualified Juvix.Core.ErasedAnn as J
import Juvix.Core.Usage
import Juvix.Library hiding (Type)
import qualified Michelson.Untyped as M
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

--------------------------------------------------------------------------------
-- Test Abstractions
--------------------------------------------------------------------------------

-- TODO: Switch these tests to use the interpreter (ideally through the parameterisation :) ).
shouldCompile ∷ Term → Type → Text → T.TestTree
shouldCompile term ty contract =
  T.testCase
    (show term <> " :: " <> show ty <> " should compile to " <> show contract)
    (Right contract T.@=? ((untypedContractToSource . fst) |<< fst (compileContract term ty)))

shouldOptimise ∷ Op → Op → T.TestTree
shouldOptimise instr opt =
  T.testCase
    (show instr <> " should optimise to " <> show opt)
    (opt T.@=? optimiseSingle instr)

shouldCompileExpr ∷ Term → Type → T.TestTree
shouldCompileExpr term ty =
  T.testCase
    (show term <> " should compile to an instruction sequence")
    (isRight (fst (compileExpr term ty)) T.@? "failed to compile")

--------------------------------------------------------------------------------
-- Test Groups
--------------------------------------------------------------------------------

backendMichelson ∷ T.TestTree
backendMichelson =
  T.testGroup
    "Backend Michelson"
    [ -- identityFn,
      -- identityApp,
      -- identityApp2,
      -- identityExpr,
      optimiseDupDrop,
      optimiseLambdaExec
    ]

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

optimiseDupDrop ∷ T.TestTree
optimiseDupDrop =
  shouldOptimise
    (Instructions.dup <> Instructions.drop)
    (M.SeqEx [])

optimiseLambdaExec ∷ T.TestTree
optimiseLambdaExec =
  shouldOptimise
    (Instructions.lambda Untyped.unit Untyped.unit [] <> Instructions.exec)
    (M.SeqEx [])

identityExpr ∷ T.TestTree
identityExpr =
  shouldCompileExpr
    identityTerm2
    identityType2

identityExpr2 ∷ T.TestTree
identityExpr2 =
  shouldCompileExpr
    identityAppExpr
    identityType2

identityExpr3 ∷ T.TestTree
identityExpr3 =
  shouldCompileExpr
    identityAppExpr2
    identityType2

identityApp2 ∷ T.TestTree
identityApp2 =
  shouldCompile
    identityAppTerm2
    identityType
    ""

unitTest ∷ T.TestTree
unitTest =
  shouldCompileExpr unitExpr1 (J.PrimTy (PrimTy unit))

-- compileExpr unitExpr1 (J.PrimTy (PrimTy unit))

identityFn ∷ T.TestTree
identityFn =
  shouldCompile
    identityTerm
    identityType
    "parameter unit;storage unit;code {{PUSH (pair unit (lambda (pair (list operation) \
    \unit) (pair (pair (list operation) unit) (lambda (pair unit (pair (list operation) \
    \unit)) (pair (list operation) unit))))) (Pair Unit {{DIP {PUSH (lambda (pair \
    \unit (pair (list operation) unit)) (pair (list operation) unit)) {{DUP; CAR; DIP \
    \{CDR; CAR}; SWAP; PAIR % %}}}; PAIR % %}}); {NIL operation; {DIP {{DUP; CAR; DIP \
    \{CDR}}}; {PAIR % %; {EXEC; {PUSH (pair unit (lambda (pair (pair unit unit) unit) \
    \unit)) (Pair Unit {CAR; CAR}); {DIP {SWAP}; {SWAP; {DUP; {DIP {{SWAP; DIP {SWAP}}}; \
    \{DIP {{DUP; CAR; DIP {CDR}}}; {PAIR % %; {EXEC; {DIP {{DUP; CAR; DIP {CDR}}}; {PAIR \
    \% %; {EXEC; {DIP {DROP}; {}}}}}}}}}}}}}}}}}}};"

identityApp ∷ T.TestTree
identityApp =
  shouldCompile
    identityAppTerm
    identityType
    "parameter unit;storage unit;code {{PUSH (lambda (pair (pair unit unit) unit) (pair \
    \(list operation) unit)) {{CAR}; {{PUSH (pair unit (lambda (pair (list operation) \
    \unit) (pair (pair (list operation) unit) (lambda (pair unit (pair (list operation) \
    \unit)) (pair (list operation) unit))))) (Pair Unit {{DIP {PUSH (lambda (pair \
    \unit (pair (list operation) unit)) (pair (list operation) unit)) {{DUP; CAR; DIP \
    \{CDR; CAR}; SWAP; PAIR % %}}}; PAIR % %}}); NIL operation; DIP {{DUP; CAR; DIP \
    \{CDR}}}; PAIR % %; EXEC}; {PUSH (pair unit (lambda (pair (pair unit unit) unit) \
    \unit)) (Pair Unit {CAR; CAR}); {{DIP {SWAP}; SWAP}; DUP; DIP {{SWAP; DIP {SWAP}}}}; \
    \DIP {{DUP; CAR; DIP {CDR}}}; PAIR % %; EXEC}; DIP {{DUP; CAR; DIP {CDR}}}; PAIR \
    \% %; EXEC}; DIP {DROP}}; {PUSH unit Unit; {PAIR % %; {SWAP; {DUP; {DIP {SWAP}; \
    \{DIP {{DUP; CAR; DIP {CDR}}}; {PAIR % %; {EXEC; {DIP {DROP}; {}}}}}}}}}}}};"

--------------------------------------------------------------------------------
-- Terms to test against
--------------------------------------------------------------------------------

unitExpr1 ∷ Term
unitExpr1 =
  Ann
    (J.Prim (Constant M.ValueUnit))
    one
    (J.PrimTy (PrimTy unit))

identityTerm ∷ Term
identityTerm =
  Ann
    ( J.LamM
        []
        ["x"]
        ( Ann
            ( J.AppM
                ( Ann
                    (J.Prim (Instructions.toNewPrimErr Instructions.pair))
                    one
                    ( J.Pi
                        one
                        (J.PrimTy (PrimTy (M.Type (M.TList (M.Type M.TOperation "")) "")))
                        (J.Pi one (J.PrimTy (PrimTy (M.Type M.TUnit ""))) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type (M.TList (M.Type M.TOperation "")) "") (M.Type M.TUnit "")) ""))))
                    )
                )
                [ Ann (J.Prim (Constant M.ValueNil)) one (J.PrimTy (PrimTy (M.Type (M.TList (M.Type M.TOperation "")) ""))),
                  Ann
                    ( J.AppM
                        (Ann (J.Prim (Instructions.toNewPrimErr Instructions.car)) one (J.Pi one (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))) (J.PrimTy (PrimTy (M.Type M.TUnit "")))))
                        [Ann (J.Var "x") one (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) "")))]
                    )
                    one
                    (J.PrimTy (PrimTy (M.Type M.TUnit "")))
                ]
            )
            one
            (J.PrimTy (PrimTy (M.Type (M.TPair "" "" opl unit) "")))
        )
    )
    one
    identityType

identityTerm2 ∷ Term
identityTerm2 =
  ( Ann
      ( J.LamM
          []
          ["x"]
          ( Ann
              ( J.AppM
                  ( Ann
                      (J.Prim (Instructions.toNewPrimErr Instructions.pair))
                      one
                      ( J.Pi
                          one
                          (J.PrimTy (PrimTy (M.Type (M.TList (M.Type M.TUnit "")) "")))
                          (J.Pi one (J.PrimTy (PrimTy (M.Type M.TUnit ""))) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type (M.TList (M.Type M.TUnit "")) "") (M.Type M.TUnit "")) ""))))
                      )
                  )
                  [ Ann (J.Prim (Constant M.ValueNil)) one (J.PrimTy (PrimTy (M.Type (M.TList (M.Type M.TUnit "")) ""))),
                    Ann
                      ( J.AppM
                          ( Ann
                              (J.Prim (Instructions.toNewPrimErr Instructions.car))
                              one
                              (J.Pi one (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))) (J.PrimTy (PrimTy (M.Type M.TUnit ""))))
                          )
                          [(Ann (J.Var "x") one (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))))]
                      )
                      one
                      (J.PrimTy (PrimTy (M.Type M.TUnit "")))
                  ]
              )
              one
              (J.PrimTy (PrimTy (M.Type (M.TPair "" "" opl unit) "")))
          )
      )
      one
      identityType
  )

primLam ∷ NonEmpty M.Type → Type
primLam (ty :| []) = J.PrimTy (PrimTy ty)
primLam (ty :| (t : ts)) = J.Pi one (J.PrimTy (PrimTy ty)) (primLam (t :| ts))

identityAppTerm ∷ Term
identityAppTerm =
  ( Ann
      ( J.LamM
          ["y"]
          []
          ( Ann
              ( J.AppM
                  ( Ann
                      ( J.LamM
                          ["x"]
                          []
                          ( Ann
                              ( J.AppM
                                  ( Ann
                                      (J.Prim (Instructions.toNewPrimErr Instructions.pair))
                                      one
                                      primPairTy
                                  )
                                  [ Ann (J.Prim (Constant M.ValueNil)) one (J.PrimTy (PrimTy (M.Type (M.TList (M.Type M.TOperation "")) ""))),
                                    ( Ann
                                        ( J.AppM
                                            (Ann (J.Prim (Instructions.toNewPrimErr Instructions.car)) one (J.Pi one (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))) (J.PrimTy (PrimTy (M.Type M.TUnit "")))))
                                            [(Ann (J.Var "x") one (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))))]
                                        )
                                        one
                                        (J.PrimTy (PrimTy (M.Type M.TUnit "")))
                                    )
                                  ]
                              )
                              one
                              (J.PrimTy (PrimTy (M.Type (M.TPair "" "" opl unit) "")))
                          )
                      )
                      one
                      identityType
                  )
                  [(Ann (J.Var "y") one (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))))]
              )
              one
              (J.PrimTy (PrimTy (M.Type (M.TPair "" "" opl unit) "")))
          )
      )
      one
      identityType
  )

identityAppExpr ∷ Term
identityAppExpr =
  ( Ann
      ( J.LamM
          ["y"]
          []
          ( Ann
              ( J.AppM
                  ( Ann
                      ( J.LamM
                          ["x"]
                          []
                          ( Ann
                              ( J.AppM
                                  ( Ann
                                      (J.Prim (Instructions.toNewPrimErr Instructions.pair))
                                      one
                                      primPairTy2
                                  )
                                  [ Ann (J.Prim (Constant M.ValueNil)) one (J.PrimTy (PrimTy (M.Type (M.TList (M.Type M.TUnit "")) ""))),
                                    Ann
                                      ( J.AppM
                                          (Ann (J.Prim (Instructions.toNewPrimErr Instructions.car)) one (J.Pi one (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))) (J.PrimTy (PrimTy (M.Type M.TUnit "")))))
                                          [Ann (J.Var "x") one (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) "")))]
                                      )
                                      one
                                      (J.PrimTy (PrimTy (M.Type M.TUnit "")))
                                  ]
                              )
                              one
                              (J.PrimTy (PrimTy (M.Type (M.TPair "" "" unitl unit) "")))
                          )
                      )
                      one
                      identityType2
                  )
                  [Ann (J.Var "y") one (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) "")))]
              )
              one
              (J.PrimTy (PrimTy (M.Type (M.TPair "" "" unitl unit) "")))
          )
      )
      one
      identityType2
  )

identityAppTerm2 ∷ Term
identityAppTerm2 =
  ( Ann
      ( J.LamM
          ["x"]
          []
          ( Ann
              ( J.AppM
                  ( Ann
                      ( J.LamM
                          ["f"]
                          ["x"]
                          ( Ann
                              ( J.AppM
                                  ( Ann
                                      (J.Var "f")
                                      one
                                      primPairTy
                                  )
                                  [ Ann (J.Prim (Constant M.ValueNil)) one (J.PrimTy (PrimTy (M.Type (M.TList (M.Type M.TOperation "")) ""))),
                                    Ann
                                      ( J.AppM
                                          (Ann (J.Prim (Instructions.toNewPrimErr Instructions.car)) one (J.Pi one (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))) (J.PrimTy (PrimTy (M.Type M.TUnit "")))))
                                          [Ann (J.Var "x") one (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) "")))]
                                      )
                                      one
                                      (J.PrimTy (PrimTy (M.Type M.TUnit "")))
                                  ]
                              )
                              one
                              (J.PrimTy (PrimTy (M.Type (M.TPair "" "" opl unit) "")))
                          )
                      )
                      one
                      (J.Pi one primPairTy (J.PrimTy (PrimTy (M.Type (M.TPair "" "" opl unit) ""))))
                  )
                  [Ann (J.Prim (Instructions.toNewPrimErr Instructions.pair)) one primPairTy]
              )
              one
              (J.PrimTy (PrimTy (M.Type (M.TPair "" "" opl unit) "")))
          )
      )
      one
      identityType
  )

identityAppExpr2 ∷ Term
identityAppExpr2 =
  Ann
    ( J.LamM
        ["x"]
        []
        ( Ann
            ( J.AppM
                ( Ann
                    ( J.LamM
                        ["f"]
                        ["x"]
                        ( Ann
                            ( J.AppM
                                ( Ann
                                    (J.Var "f")
                                    one
                                    primPairTy2
                                )
                                [ Ann (J.Prim (Constant M.ValueNil)) one (J.PrimTy (PrimTy (M.Type (M.TList (M.Type M.TUnit "")) ""))),
                                  ( Ann
                                      ( J.AppM
                                          (Ann (J.Prim (Instructions.toNewPrimErr Instructions.car)) one (J.Pi one (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))) (J.PrimTy (PrimTy (M.Type M.TUnit "")))))
                                          [(Ann (J.Var "x") one (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))))]
                                      )
                                      one
                                      (J.PrimTy (PrimTy (M.Type M.TUnit "")))
                                  )
                                ]
                            )
                            one
                            (J.PrimTy (PrimTy (M.Type (M.TPair "" "" unitl unit) "")))
                        )
                    )
                    one
                    (J.Pi one primPairTy2 (J.PrimTy (PrimTy (M.Type (M.TPair "" "" unitl unit) ""))))
                )
                [(Ann (J.Prim (Instructions.toNewPrimErr Instructions.pair)) one primPairTy2)]
            )
            one
            (J.PrimTy (PrimTy (M.Type (M.TPair "" "" unitl unit) "")))
        )
    )
    one
    identityType2

--------------------------------------------------------------------------------
-- Type Abstractions
--------------------------------------------------------------------------------

fstTy ∷ Type
fstTy = J.Pi one (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type M.TUnit "") (M.Type M.TUnit "")) ""))) (J.PrimTy (PrimTy (M.Type M.TUnit "")))

pairTy ∷ Type
pairTy =
  J.Pi
    one
    (J.PrimTy (PrimTy (M.Type (M.TList (M.Type M.TOperation "")) "")))
    (J.Pi one (J.PrimTy (PrimTy (M.Type M.TUnit ""))) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type (M.TList (M.Type M.TOperation "")) "") (M.Type M.TUnit "")) ""))))

identityType ∷ Type
identityType = J.Pi Omega (J.PrimTy (PrimTy (M.Type (M.TPair "" "" unit unit) ""))) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" opl unit) "")))

identityType2 ∷ Type
identityType2 = J.Pi Omega (J.PrimTy (PrimTy (M.Type (M.TPair "" "" unit unit) ""))) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" unitl unit) "")))

unitl ∷ M.Type
unitl = M.Type (M.TList (M.Type M.TUnit "")) ""

opl ∷ M.Type
opl = M.Type (M.TList (M.Type M.TOperation "")) ""

unit ∷ M.Type
unit = M.Type M.TUnit ""

primPairTy =
  J.Pi
    one
    (J.PrimTy (PrimTy (M.Type (M.TList (M.Type M.TOperation "")) "")))
    (J.Pi one (J.PrimTy (PrimTy (M.Type M.TUnit ""))) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type (M.TList (M.Type M.TOperation "")) "") (M.Type M.TUnit "")) ""))))

primPairTy2 =
  J.Pi
    one
    (J.PrimTy (PrimTy (M.Type (M.TList (M.Type M.TUnit "")) "")))
    (J.Pi one (J.PrimTy (PrimTy (M.Type M.TUnit ""))) (J.PrimTy (PrimTy (M.Type (M.TPair "" "" (M.Type (M.TList (M.Type M.TUnit "")) "") (M.Type M.TUnit "")) ""))))
