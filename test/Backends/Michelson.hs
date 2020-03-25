module Backends.Michelson where

import Juvix.Backends.Michelson.Compilation
import Juvix.Backends.Michelson.Compilation.Types
import qualified Juvix.Backends.Michelson.DSL.Environment as DSL
import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions
import qualified Juvix.Backends.Michelson.DSL.Untyped as Untyped
import Juvix.Backends.Michelson.Optimisation
import qualified Juvix.Core.ErasedAnn as J
import Juvix.Core.Usage
import Juvix.Library hiding (Type, show)
import qualified Michelson.Untyped as M
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (show)

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

lamxx ∷ Term
lamxx =
  Ann one (J.Pi one (primTy Untyped.unit) (primTy Untyped.unit)) $
    J.LamM [] ["x"] lookupX

lookupX ∷ Term
lookupX = Ann one (primTy Untyped.unit) (J.Var "x")

-- [SeqEx
--  [PrimEx (DUP @)
--  ,PrimEx (CAR @ %)
--  ,PrimEx (DIP [PrimEx (CDR @ %)])
--  ,PrimEx (DIP [SeqEx []])]
--  ,PrimEx (DIG 0)]

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

-- nonConstApp generates:
-- [PrimEx (PUSH @ (Type (Tc CInt) :) (ValueInt 3))
-- ,PrimEx (PUSH @ (Type TUnit :) ValueUnit)
-- ,SeqEx [PrimEx (DIG 0)
--        ,PrimEx (DUP @)
--        ,PrimEx (DUG 1)]]

nonConstApp ∷ Term
nonConstApp =
  Ann
    one
    (primTy Untyped.unit)
    $ J.AppM
      constUInt
      [ push1 M.ValueUnit Untyped.unit,
        push1 (M.ValueInt 3) (Untyped.tc Untyped.int)
      ]

-- [PrimEx (PUSH @ (Type TUnit :) ValueUnit)]
constApp ∷ Term
constApp =
  Ann
    one
    (primTy Untyped.unit)
    $ J.AppM constUInt [unitExpr1, annIntOne 3]

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

-- [PrimEx (PUSH @ (Type TUnit :) ValueUnit)
--   ,PrimEx (PUSH @ (Type TUnit :) ValueUnit)
--   ,PrimEx (PAIR : @ % %)]

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

-- underExactNonConst generates:

-- [PrimEx (PUSH @ (Type TUnit :) ValueUnit)
-- ,PrimEx (PUSH @ (Type TUnit :) ValueUnit)
-- ,SeqEx [PrimEx (DIG 1)
--        ,PrimEx (DUP @)
--        ,PrimEx (DUG 2)]]

-- note the dup, this is because in the stack, we pushed it as omega
-- if we did better constant propagation this would be free
underExactNonConst ∷ Term
underExactNonConst = underExactGen (push1 M.ValueUnit Untyped.unit)

-- | 'overExactGen' tests for overapplication of a multi argument lambda
-- then feeds the rest of the arguments into the inner lambda perfectly
overExactGen ∷ Term → Term
overExactGen x =
  Ann
    one
    (primTy Untyped.unit)
    $ J.AppM
      ( Ann
          one
          ( J.Pi one (primTy Untyped.unit)
              $ J.Pi one (primTy Untyped.unit)
              $ J.Pi one (primTy Untyped.unit)
              $ primTy Untyped.unit
          )
          $ J.LamM [] ["y", "z"]
          $ Ann one (J.Pi one (primTy Untyped.unit) (primTy Untyped.unit))
          $ J.LamM [] ["x"] lookupX
      )
      [x, x, x]

-- Optimal code generation
-- [PrimEx (PUSH @ (Type TUnit :) ValueUnit)]
overExactConst ∷ Term
overExactConst = overExactGen unitExpr1

-- overExactNonConst generates:
-- Seems fairly optimal
-- [PrimEx (PUSH @ (Type TUnit :) ValueUnit)
-- , PrimEx (PUSH @ (Type TUnit :) ValueUnit)
-- , PrimEx (PUSH @ (Type TUnit :) ValueUnit)
-- , SeqEx [ PrimEx (DIG 2)
--         , PrimEx (DUP @)
--         , PrimEx (DUG 3)]]

overExactNonConst ∷ Term
overExactNonConst = overExactGen (push1 M.ValueUnit Untyped.unit)

-- IdentityTerm generates
-- [SeqEx []
-- , PrimEx (PUSH @ (Type (TList (Type TOperation :)) :) ValueNil)
-- , SeqEx [PrimEx (DIG 1),PrimEx (DUP @),PrimEx (DUG 2)]
-- , PrimEx (CAR @ %)
-- , PrimEx (PAIR : @ % %)]

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

intPair ∷ Integer → Integer → Term
intPair x y =
  Ann one t $
    J.AppM
      ( Ann one t
          $ J.Prim
          $ Instructions.toNewPrimErr Instructions.pair
      )
      [push1Int x, push1Int y]
  where
    t =
      J.Pi one (primTy int)
        $ J.Pi one (primTy int)
        $ primTy pairInt

-- intPairs1 generates:
-- [PrimEx (PUSH @ (Type (Tc CInt) :) (ValueInt 3))
-- ,PrimEx (PUSH @ (Type (Tc CInt) :) (ValueInt 4))
-- ,PrimEx (PAIR : @ % %)
-- ,PrimEx (PUSH @ (Type (Tc CInt) :) (ValueInt 5))
-- ,PrimEx (PUSH @ (Type (Tc CInt) :) (ValueInt 6))
-- ,PrimEx (PAIR : @ % %)
-- ,PrimEx (PAIR : @ % %)]

intPairs1 ∷ Term
intPairs1 =
  Ann (SNat 2) (primTy (Untyped.pair pairInt pairInt)) $
    J.AppM
      ( Ann one t
          $ J.Prim
          $ Instructions.toNewPrimErr Instructions.pair
      )
      [intPair 3 4, intPair 5 6]
  where
    t =
      J.Pi one (primTy pairInt)
        $ J.Pi one (primTy pairInt)
        $ primTy
        $ Untyped.pair pairInt pairInt

-- addPairs "x"
-- [SeqEx []
-- ,SeqEx [PrimEx (DIG 0),PrimEx (DUP @),PrimEx (DUG 1)]
-- ,PrimEx (CAR @ %)
-- ,PrimEx (DIG 1)
-- ,PrimEx (CDR @ %)
-- ,PrimEx (ADD @)]

addPairs ∷ Symbol → Term
addPairs name =
  Ann one t'
    $ J.LamM [] [name]
    $ Ann one t
    $ J.AppM
      ( Ann one (J.Pi one (primTy int) (J.Pi one (primTy int) t))
          $ J.Prim
          $ Instructions.toNewPrimErr Instructions.add
      )
      [car int int xLook, cdr int int xLook]
  where
    t = primTy int

    t' = J.Pi (SNat 2) (primTy pairInt) t

    xLook = Ann one (primTy pairInt) (J.Var name)

-- ,SeqEx [PrimEx (DIG 5)
--        ,PrimEx (DUP @)
--        ,PrimEx (DUG 6)] -- stack: [??? : 7 : (3,4) : ((3,4),(5,6))] WRONG
--   -- IT seems that inconsistent usage causes an error
-- ,PrimEx (CDR @ %)
-- ,SeqEx [PrimEx (DIG 0),PrimEx (DUP @),PrimEx (DUG 1)]
-- ,PrimEx (CAR @ %)
-- ,SeqEx [PrimEx (DIG 1),PrimEx (DUP @),PrimEx (DUG 2)]
-- ,PrimEx (CDR @ %)
-- ,PrimEx (ADD @) -- (5 + 6)
-- ,PrimEx (PAIR : @ % %)]

-- [PrimEx (PUSH @ (Type (Tc CInt) :) (ValueInt 3))
-- ,PrimEx (PUSH @ (Type (Tc CInt) :) (ValueInt 4))
-- ,PrimEx (PAIR : @ % %)
-- ,PrimEx (PUSH @ (Type (Tc CInt) :) (ValueInt 5))
-- ,PrimEx (PUSH @ (Type (Tc CInt) :) (ValueInt 6))
-- ,PrimEx (PAIR : @ % %)
-- ,PrimEx (PAIR : @ % %) -- stack: [((3,4),(5,6))]
-- ,SeqEx [PrimEx (DIG 0)
--        ,PrimEx (DUP @)
--        ,PrimEx (DUG 1)]  -- stack: [((3,4),(5,6)) : ((3,4),(5,6))]
-- ,PrimEx (CAR @ %)        -- stack: [(3,4) : ((3,4),(5,6))]
-- ,SeqEx [PrimEx (DIG 0)
--        ,PrimEx (DUP @)
--        ,PrimEx (DUG 1)] -- stack: [(3,4) : (3,4) : ((3,4),(5,6))]
-- ,PrimEx (CAR @ %)       -- stack: [3 : (3,4) : ((3,4),(5,6))]
-- ,SeqEx [PrimEx (DIG 1)
--        ,PrimEx (DUP @)
--        ,PrimEx (DUG 2)] -- stack: [(3,4) : 3 : (3,4) : ((3,4),(5,6))]
-- ,PrimEx (CDR @ %)       -- stack: [4 : 3 : (3,4) : ((3,4),(5,6))]
-- ,PrimEx (ADD @) -- (3 + 4) stack: [7 : (3,4) : ((3,4),(5,6))]
-- ,SeqEx [PrimEx (DIG 2)
--        ,PrimEx (DUP @)
--        ,PrimEx (DUG 3)] -- stack: [((3,4),(5,6)) : 7 : (3,4) : ((3,4),(5,6))]
-- ,PrimEx (CDR @ %)       -- stack: [(5,6) : 7 : (3,4) : ((3,4),(5,6))]
-- ,SeqEx [PrimEx (DIG 0)
--        ,PrimEx (DUP @)
--        ,PrimEx (DUG 1)] -- stack: [(5,6) : (5,6) : 7 : (3,4) : ((3,4),(5,6))]
-- ,PrimEx (CAR @ %)       -- stack: [5 : (5,6) : 7 : (3,4) : ((3,4),(5,6))]
-- ,SeqEx [PrimEx (DIG 1)
--        ,PrimEx (DUP @)
--        ,PrimEx (DUG 2)] -- stack: [(5,6) : 5 : (5,6) : 7 : (3,4) : ((3,4),(5,6))]
-- ,PrimEx (CDR @ %)       -- stack: [6 : 5 : (5,6) : 7 : (3,4) : ((3,4),(5,6))]
-- ,PrimEx (ADD @)         -- stack: [11 : (5,6) : 7 : (3,4) : ((3,4),(5,6))]
-- ,PrimEx (PAIR : @ % %)] -- Incorrect, however this is due to usage propagation

addDoublePairs ∷ Term
addDoublePairs =
  Ann one t $
    J.AppM
      ( Ann one (J.Pi (SNat 2) (primTy (Untyped.pair pairInt pairInt)) t)
          $ J.LamM [] ["y"]
          $ Ann one t
          $ J.AppM
            ( Ann one pairIntType
                $ J.Prim
                $ Instructions.toNewPrimErr Instructions.pair
            )
            [ applyPlus (car pairInt pairInt xLook) "a",
              applyPlus (cdr pairInt pairInt xLook) "b"
            ]
      )
      [intPairs1]
  where
    t = primTy pairInt

    pairIntType =
      J.Pi one (primTy int)
        $ J.Pi one (primTy int)
        $ primTy pairInt

    xLook =
      Ann one (primTy (Untyped.pair pairInt pairInt)) (J.Var "y")

    applyPlus term name =
      Ann one (primTy int) (J.AppM (addPairs name) [term])

-- this should really be a pair we are sending in, but we can let it compile
-- (wrongly typed of course), by instead sending in a non constant unit
identityCall =
  Ann one (primTy Untyped.unit) $
    J.AppM identityTerm2 [push1Int 3]

identityTerm2 ∷ Term
identityTerm2 =
  Ann one identityType
    $ J.LamM [] ["x"]
    $ Ann one (primTy (Untyped.pair unitl Untyped.unit))
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
        car Untyped.unit Untyped.unit
          $ Ann one (primTy (Untyped.pair Untyped.unit Untyped.unit))
          $ J.Var "x"
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
              car Untyped.unit Untyped.unit
                $ Ann one (primTy (Untyped.pair Untyped.unit Untyped.unit))
                $ J.Var "x"
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

int = Untyped.tc Untyped.int

pairInt ∷ M.Type
pairInt =
  Untyped.pair int int

car pairFst pairSnd pair =
  Ann one (primTy pairFst) $
    J.AppM
      ( Ann
          one
          (J.Pi one (primTy (Untyped.pair pairFst pairSnd)) (primTy pairFst))
          (J.Prim (Instructions.toNewPrimErr Instructions.car))
      )
      [pair]

cdr pairFst pairSnd pair =
  Ann one (primTy pairSnd) $
    J.AppM
      ( Ann
          one
          (J.Pi one (primTy (Untyped.pair pairFst pairSnd)) (primTy pairFst))
          (J.Prim (Instructions.toNewPrimErr Instructions.cdr))
      )
      [pair]

--------------------------------------------------------------------------------
-- general abstractions
--------------------------------------------------------------------------------

primTy ∷ M.Type → J.Type PrimTy primVal
primTy = J.PrimTy . PrimTy

annIntOne ∷ Integer → Term
annIntOne i =
  Ann one (primTy (Untyped.tc Untyped.int)) (J.Prim (Constant (M.ValueInt i)))

push1Int ∷ Integer → AnnTerm PrimTy NewPrim
push1Int i = push1 (M.ValueInt i) (Untyped.tc Untyped.int)

push1 ∷ M.Value' Op → M.Type → AnnTerm PrimTy NewPrim
push1 const ty =
  Ann
    one
    (primTy Untyped.unit)
    $ J.AppM
      ( Ann one (J.Pi one (primTy ty) (primTy ty))
          $ J.Prim
          $ Instructions.toNewPrimErr
          $ Instructions.push ty (M.ValueNil) -- the undefined here is never used
      )
      [Ann one (primTy ty) (J.Prim (Constant const))]
