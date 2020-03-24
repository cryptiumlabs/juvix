module Backends.Michelson where

import Juvix.Backends.Michelson.Compilation
import Juvix.Backends.Michelson.Compilation.Types
import qualified Juvix.Backends.Michelson.DSL.Environment as DSL
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

-- TODO ∷ promote to a tasty test!
extractTest ∷ NewTerm → (Either DSL.CompError M.ExpandedOp, DSL.Env)
extractTest = DSL.execMichelson . runMichelsonExpr

testRun = extractTest unitExpr1

unitExpr1 ∷ Term
unitExpr1 =
  Ann
    one
    (primTy Untyped.unit)
    (J.Prim (Constant M.ValueUnit))

symbIdent ∷ Term
symbIdent =
  Ann one (primTy Untyped.unit) (J.AppM lamxx [unitExpr1])

constApp ∷ Term
constApp =
  Ann
    one
    (primTy Untyped.unit)
    $ J.AppM constUInt [unitExpr1, annIntOne 3]

lamxx ∷ Term
lamxx =
  Ann one (J.Pi one (primTy Untyped.unit) (primTy Untyped.unit)) $
    J.LamM [] ["x"] lookupX

lookupX ∷ Term
lookupX = Ann one (primTy Untyped.unit) (J.Var "x")

constUInt ∷ Term
constUInt =
  Ann
    one
    ( J.Pi one (primTy Untyped.unit)
        $ J.Pi
          mempty
          (primTy (Untyped.tc Untyped.int))
        $ primTy Untyped.unit
    )
    $ J.LamM [] ["x", "y"] lookupX

pairGen ∷ [AnnTerm PrimTy NewPrim] → AnnTerm PrimTy NewPrim
pairGen =
  Ann
    one
    (primTy (Untyped.pair Untyped.unit Untyped.unit))
    . J.AppM
      ( Ann
          one
          ( J.Pi one (primTy Untyped.unit)
              $ J.Pi one (primTy Untyped.unit)
              $ primTy
              $ Untyped.pair Untyped.unit Untyped.unit
          )
          $ J.Prim
          $ Instructions.toNewPrimErr Instructions.pair
      )

pairConstant ∷ Term
pairConstant = pairGen [unitExpr1, unitExpr1]

pairNotConstant ∷ Term
pairNotConstant = pairGen [unitExpr1, push1 M.ValueUnit Untyped.unit]

-- | 'underExactGen' tests for under application of a multi argument lambda
-- then gives it the exact number of arguments
underExactGen ∷ Term → Term
underExactGen x =
  Ann
    one
    (primTy Untyped.unit)
    $ J.AppM
      ( Ann
          one
          ( J.Pi one (primTy Untyped.unit) $
              primTy Untyped.unit
          )
          $ J.AppM
            ( Ann
                one
                ( J.Pi one (primTy Untyped.unit)
                    $ J.Pi one (primTy Untyped.unit)
                    $ primTy Untyped.unit
                )
                $ J.LamM [] ["x", "y"] lookupX
            )
            [x]
      )
      [x]

-- Generates optimal code!
underExactConst ∷ Term
underExactConst = underExactGen unitExpr1

-- generates (read it in reverse!):
-- [ PrimEx (DIG 1), PrimEx (DUP @), PrimEx (DUG 1)
-- , PrimEx (PUSH @ (Type TUnit :) ValueUnit)
-- , PrimEx (PUSH @ (Type TUnit :) ValueUnit)]
-- note the dup, this is because in the stack, we pushed it as omega
-- if we did better constant propagation this would be free
-- we could probably turn the last (DIG 1) into (DIG 0)
underExactNonConst ∷ Term
underExactNonConst = underExactGen (push1 M.ValueUnit Untyped.unit)

identityTerm ∷ Term
identityTerm =
  Ann one identityType
    $ J.LamM [] ["x"]
    $ Ann one (primTy (Untyped.pair opl Untyped.unit))
    $ J.AppM
      ( Ann
          one
          ( J.Pi
              one
              (primTy opl)
              $ J.Pi one (primTy Untyped.unit)
              $ primTy
              $ Untyped.pair opl Untyped.unit
          )
          $ J.Prim
          $ Instructions.toNewPrimErr Instructions.pair
      )
      [ Ann one (primTy opl) (J.Prim (Constant M.ValueNil)),
        Ann
          one
          (J.PrimTy (PrimTy (M.Type M.TUnit "")))
          $ J.AppM
            ( Ann
                one
                (J.Pi one (primTy unitPair) (primTy Untyped.unit))
                $ J.Prim
                $ Instructions.toNewPrimErr Instructions.car
            )
            [Ann one (primTy unitPair) (J.Var "x")]
      ]

-- this should really be a pair we are sending in, but we can let it compile
-- (wrongly typed of course), by instead sending in a non constant unit
identityCall =
  Ann one (primTy Untyped.unit) $
    J.AppM identityTerm2 [push1 M.ValueUnit Untyped.unit]

identityTerm2 ∷ Term
identityTerm2 =
  Ann one identityType
    $ J.LamM [] ["x"]
    $ Ann one (primTy (Untyped.pair opl Untyped.unit))
    $ J.AppM
      ( Ann
          one
          ( J.Pi
              one
              (primTy unitl)
              (J.Pi one (primTy Untyped.unit) (primTy (Untyped.pair unitl Untyped.unit)))
          )
          $ J.Prim
          $ Instructions.toNewPrimErr Instructions.pair
      )
      -- Force the push to be a non constant. This should do nothing
      -- as it's already forced by the second
      [ push1 M.ValueNil unitl,
        Ann
          one
          (primTy Untyped.unit)
          $ J.AppM
            ( Ann
                one
                (J.Pi one (primTy unitPair) (primTy Untyped.unit))
                (J.Prim (Instructions.toNewPrimErr Instructions.car))
            )
            [Ann one (primTy unitPair) (J.Var "x")]
      ]

primLam ∷ NonEmpty M.Type → Type
primLam (ty :| []) = J.PrimTy (PrimTy ty)
primLam (ty :| (t : ts)) = J.Pi one (J.PrimTy (PrimTy ty)) (primLam (t :| ts))

identityAppTerm ∷ Term
identityAppTerm =
  Ann one identityType
    $ J.LamM [] ["y"]
    $ Ann one (primTy (Untyped.pair opl Untyped.unit))
    $ J.AppM
      ( Ann one identityType
          $ J.LamM [] ["x"]
          $ Ann one (primTy (Untyped.pair opl Untyped.unit))
          $ J.AppM
            ( Ann one primPairTy $
                J.Prim (Instructions.toNewPrimErr Instructions.pair)
            )
            [ Ann one (primTy opl) (J.Prim (Constant M.ValueNil)),
              Ann one (primTy Untyped.unit) $
                J.AppM
                  ( Ann
                      one
                      (J.Pi one (primTy unitPair) (primTy Untyped.unit))
                      (J.Prim (Instructions.toNewPrimErr Instructions.car))
                  )
                  [Ann one (primTy (Untyped.pair Untyped.unit Untyped.unit)) (J.Var "x")]
            ]
      )
      [Ann one (primTy unitPair) (J.Var "y")]

identityAppExpr ∷ Term
identityAppExpr =
  Ann one identityType2
    $ J.LamM [] ["y"]
    $ Ann one (primTy (Untyped.pair unitl Untyped.unit))
    $ J.AppM
      ( Ann one identityType2
          $ J.LamM [] ["x"]
          $ Ann one (primTy (Untyped.pair unitl Untyped.unit))
          $ J.AppM
            ( Ann one primPairTy2
                $ J.Prim
                $ Instructions.toNewPrimErr Instructions.pair
            )
            [ Ann one (primTy unitl) (J.Prim (Constant M.ValueNil)),
              Ann
                one
                (primTy Untyped.unit)
                $ J.AppM
                  ( Ann
                      one
                      (J.Pi one (primTy unitPair) (primTy unit))
                      (J.Prim (Instructions.toNewPrimErr Instructions.car))
                  )
                  [ Ann
                      one
                      (primTy unitPair)
                      (J.Var "x")
                  ]
            ]
      )
      [Ann one (primTy unitPair) (J.Var "y")]

identityAppTerm2 ∷ Term
identityAppTerm2 =
  Ann one identityType
    $ J.LamM [] ["x"]
    $ Ann one (primTy (Untyped.pair opl Untyped.unit))
    $ J.AppM
      ( Ann
          one
          (J.Pi one primPairTy (primTy (Untyped.pair opl Untyped.unit)))
          $ J.LamM ["x"] ["f"]
          $ Ann one (primTy (Untyped.pair opl Untyped.unit))
          $ J.AppM
            (Ann one primPairTy (J.Var "f"))
            [ Ann one (primTy opl) (J.Prim (Constant M.ValueNil)),
              Ann one (primTy Untyped.unit) $
                J.AppM
                  ( Ann
                      one
                      (J.Pi one (primTy unitPair) (primTy Untyped.unit))
                      (J.Prim (Instructions.toNewPrimErr Instructions.car))
                  )
                  [Ann one (primTy unitPair) (J.Var "x")]
            ]
      )
      [Ann one primPairTy (J.Prim (Instructions.toNewPrimErr Instructions.pair))]

identityAppExpr2 ∷ Term
identityAppExpr2 =
  Ann
    one
    identityType2
    $ J.LamM [] ["x"]
    $ Ann one (primTy (Untyped.pair unitl Untyped.unit))
    $ J.AppM
      ( Ann
          one
          (J.Pi one primPairTy2 (primTy (Untyped.pair unitl Untyped.unit)))
          $ J.LamM ["x"] ["f"]
          $ Ann one (primTy (Untyped.pair unitl Untyped.unit))
          $ J.AppM
            (Ann one primPairTy2 (J.Var "f"))
            [ Ann one (primTy unitl) (J.Prim (Constant M.ValueNil)),
              Ann one (primTy Untyped.unit) $
                J.AppM
                  ( Ann
                      one
                      (J.Pi one (primTy unitPair) (primTy Untyped.unit))
                      (J.Prim (Instructions.toNewPrimErr Instructions.car))
                  )
                  [ Ann
                      one
                      (primTy unitPair)
                      (J.Var "x")
                  ]
            ]
      )
      [Ann one primPairTy2 (J.Prim (Instructions.toNewPrimErr Instructions.pair))]

--------------------------------------------------------------------------------
-- Type Abstractions
--------------------------------------------------------------------------------

fstTy ∷ Type
fstTy =
  J.Pi one (primTy unitPair) (primTy Untyped.unit)

pairTy ∷ Type
pairTy = primPairTy

identityType ∷ Type
identityType =
  J.Pi Omega (primTy unitPair) (primTy (Untyped.pair opl unit))

identityType2 ∷ Type
identityType2 =
  J.Pi Omega (primTy unitPair) (primTy unitPair)

unitl ∷ M.Type
unitl = Untyped.list Untyped.unit

unitPair ∷ M.Type
unitPair = Untyped.pair unit unit

opl ∷ M.Type
opl = Untyped.list Untyped.operation

unit ∷ M.Type
unit = Untyped.unit

primPairTy ∷ Type
primPairTy =
  J.Pi one (primTy opl)
    $ J.Pi one (primTy Untyped.unit)
    $ primTy
    $ Untyped.pair opl Untyped.unit

primPairTy2 ∷ Type
primPairTy2 =
  J.Pi one (primTy unitl)
    $ J.Pi one (primTy Untyped.unit)
    $ primTy
    $ Untyped.pair unitl Untyped.unit

--------------------------------------------------------------------------------
-- general abstractions
--------------------------------------------------------------------------------

primTy ∷ M.Type → J.Type PrimTy primVal
primTy = J.PrimTy . PrimTy

annIntOne ∷ Integer → Term
annIntOne i =
  Ann one (primTy (Untyped.tc Untyped.int)) (J.Prim (Constant (M.ValueInt i)))

push1 ∷ M.Value' Op → M.Type → AnnTerm PrimTy NewPrim
push1 const ty =
  Ann
    one
    (primTy Untyped.unit)
    $ J.AppM
      ( Ann one (J.Pi one (primTy ty) (primTy ty))
          $ J.Prim
          $ Instructions.toNewPrimErr
          $ Instructions.push ty undefined -- the undefined here is never used
      )
      [Ann one (primTy ty) (J.Prim (Constant const))]
