-- |
-- - =EAC= serves as the place where the interaction net rules for the
--   EAC layer gets run
-- - The form given to =EAC= is not the base EAC AST, but instead a
--   pre processed =EAC= graph that the initial graph will be made on
--
-- - _Allocation_
--   + layout :
--     eac{tag | NodePtr*[portSize | PortArray[portLocation | NodePtr]* | DataArray[Data]*]}
--     * Similar to the one in Graph, however it also has the eac tag
--
--    | Part         | Alloca Or Malloc                   |
--    |--------------+------------------------------------|
--    | eac          | Malloc                             |
--    | tag          | Stored on Eac Malloc               |
--    | NodePtr*     | Malloc from =mallocNode=           |
--    | portSize     | Stored on Node Malloc              |
--    | PortArray    | Malloc                             |
--    | DataArray    | Malloc Maybe                       |
--    | PortLocation | (Null) Allocad from PortArray Call |
--    | NodePtr      | (Null) Allocad from PortArray Call |
--    | Data         | (Null) Allocad from DataArray Call |
--
-- - Node Pointers are allocated at node creation time, so not the
--   responsibility of the node to de-allocate, but instead uses the
--   default strategy laid out in [[Codegen/Graph]]
module Juvix.Backends.LLVM.Net.EAC where

-- TODO ∷ abstract all all imports to LLVM

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import qualified Juvix.Backends.LLVM.DSL as DSL
import qualified Juvix.Backends.LLVM.Net.EAC.Debug as Debug
import qualified Juvix.Backends.LLVM.Net.EAC.Defs as Defs
import Juvix.Backends.LLVM.Net.EAC.MonadEnvironment
import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import Juvix.Library hiding (reduce)
import qualified Juvix.Library.HashMap as Map
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IntPred
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type
import qualified LLVM.AST as AST
--------------------------------------------------------------------------------
-- Interaction Net runner
--------------------------------------------------------------------------------

-- TODO ∷ consider the return type
-- TODO ∷ remove boileprlate
defineReduce ∷ Codegen.Define m ⇒ m Operand.Operand
defineReduce = Codegen.defineFunction Type.void "reduce" args $
  do
    -- switch creations
    eacLPtr ← Codegen.externf "eac_list"
    appCase ← Codegen.addBlock "switch.app"
    lamCase ← Codegen.addBlock "switch.lam"
    eraCase ← Codegen.addBlock "switch.era"
    dupCase ← Codegen.addBlock "switch.dup"
    defCase ← Codegen.addBlock "switch.default"
    nullCase ← Codegen.addBlock "empty.list"
    carExists ← Codegen.addBlock "car.check"
    extCase ← Codegen.addBlockNumber "switch.exit" 1000
    nullCheck ← Types.checkNull eacLPtr
    _ ← Codegen.cbr nullCheck carExists nullCase
    -- %empty.list branch
    ------------------------------------------------------
    Codegen.setBlock nullCase
    _ ← Codegen.retNull
    -- %car.check branch
    ------------------------------------------------------
    Codegen.setBlock carExists
    car ← Types.loadCar eacLPtr
    -- TODO ∷ cdr may still be unsafe!??!?!
    cdr ← Types.loadCdr eacLPtr
    -- moved from %app case.
    -- Should not do extra work being here----
    nodePtr ← nodeOf car
    tagNode ← Defs.isBothPrimary [nodePtr]
    isPrimary ← Codegen.loadIsPrimaryEle tagNode
    test ←
      Codegen.icmp
        IntPred.EQ
        isPrimary
        (Operand.ConstantOperand (C.Int 1 1))
    -- end of moved code----------------------
    tagP ← Types.tagOf car
    tag ← Codegen.load Types.tag tagP
    _term ←
      Codegen.switch
        tag
        defCase -- this should never happen
        [ (Types.app, appCase),
          (Types.lam, lamCase),
          (Types.era, eraCase),
          (Types.dup, dupCase)
        ]
    -- TODO ∷ Prove this branch is unnecessary
    let contCase case' name = do
          Codegen.setBlock case'
          conCase ← Codegen.addBlock name
          _ ← Codegen.cbr test conCase extCase
          -- %switch.*.continue
          Codegen.setBlock conCase

        genContinueCaseD = genContinueCase tagNode car cdr defCase

        swapArgs (x : y : xs) = y : x : xs
        swapArgs xs = xs

    -- %app case
    ------------------------------------------------------
    contCase appCase "switch.app.continue"
    (aCdr, aExit) ←
      genContinueCaseD
        "app"
        [ (Types.lam, "switch.lam", annihilateRewireAux),
          (Types.dup, "switch.dup", fanInAux2App),
          (Types.era, "switch.era", eraseNodes)
        ]
    _ ← Codegen.br extCase
    -- %lam case
    ------------------------------------------------------
    contCase lamCase "switch.lam.continue"
    (lCdr, lExit) ←
      genContinueCaseD
        "lam"
        [ (Types.app, "switch.app", (\x → annihilateRewireAux (swapArgs x))),
          (Types.dup, "switch.dup", fanInAux2Lambda),
          (Types.era, "switch.era", eraseNodes)
        ]
    _ ← Codegen.br extCase
    -- %era case
    ------------------------------------------------------
    contCase eraCase "switch.era.continue"
    (eCdr, eExit) ←
      genContinueCaseD
        "fan_in"
        [ (Types.app, "switch.app", eraseNodes),
          (Types.lam, "switch.lam", eraseNodes),
          (Types.dup, "switch.dup", fanInAux0Era),
          (Types.era, "switch.era", eraseNodes)
        ]
    _ ← Codegen.br extCase
    -- %dup case
    ------------------------------------------------------
    contCase dupCase "switch.fan_in.continue"
    (fCdr, fExit) ←
      genContinueCaseD
        "fan_in"
        [ (Types.app, "switch.app", fanInAux2App . swapArgs),
          (Types.lam, "switch.lam", fanInAux2Lambda . swapArgs),
          (Types.dup, "switch.dup", fanInFanIn),
          (Types.era, "switch.era", fanInAux0Era . swapArgs)
        ]
    _ ← Codegen.br extCase
    -- %default case
    ------------------------------------------------------
    Codegen.setBlock defCase
    _ ← Codegen.br extCase
    -- %exit case
    ------------------------------------------------------
    Codegen.setBlock extCase
    cdr ←
      Codegen.phi
        Types.eacLPointer
        [ (cdr, defCase),
          (cdr, dupCase),
          (cdr, appCase),
          (cdr, eraCase),
          (cdr, lamCase),
          (aCdr, aExit),
          (lCdr, lExit),
          (fCdr, fExit),
          (eCdr, eExit)
        ]
    _ ← Codegen.free eacLPtr
    -- recursive function, properly register
    reduce ← Codegen.externf "reduce"
    Codegen.callVoid reduce (Codegen.emptyArgs [cdr])
    Codegen.retNull
  where
    args = [(Types.eacLPointer, "eac_list")]

--------------------------------------------------------------------------------
-- Code generation rules
--------------------------------------------------------------------------------

genContinueCase ∷
  Codegen.Define m ⇒
  Operand.Operand →
  Operand.Operand →
  Operand.Operand →
  Name.Name →
  Symbol →
  [(C.Constant, Symbol, [Operand.Operand] → m Operand.Operand)] →
  m (Operand.Operand, Name.Name)
genContinueCase tagNode mainEac cdr defCase prefix cases = do
  nodeEacPtr ← Defs.loadPrimaryNode tagNode
  tagOther ← Types.tagOf nodeEacPtr >>= Codegen.load Types.tag
  blocksGeneratedList ← genBlockNames
  extBranch ← Codegen.addBlock (prefix <> "switch.exit")
  let generateBody (_, branch, rule) = do
        -- %prefix.branch case
        ------------------------------------------------------
        _ ← Codegen.setBlock branch
        updateList ←
          rule
            [ mainEac,
              nodeEacPtr,
              cdr
            ]
        _ ← Codegen.br extBranch
        pure (updateList, branch)
      -- remove the rule as it's not needed in the switch
      switchArgs = fmap namesOf blocksGeneratedList
  _ ← Codegen.switch tagOther defCase switchArgs
  -- generate body, and return the list, switch pair
  phiList ← traverse generateBody blocksGeneratedList
  -- %prefix.switch.exit case
  ------------------------------------------------------
  _ ← Codegen.setBlock extBranch
  newList ←
    Codegen.phi
      Types.eacLPointer
      phiList
  pure (newList, extBranch)
  where
    appendName = (\(t, b, f) → (t, prefix <> b, f)) <$> cases

    genBlockNames =
      traverse
        ( \(t, b, f) →
            (,,) t <$> Codegen.addBlock b <*> pure f
        )
        appendName

    namesOf (t, b, _) = (t, b)

--------------------------------------------------------------------------------
-- Reduction rules
--------------------------------------------------------------------------------

-- | args is the argument for all rules that get called
args ∷ IsString b ⇒ [(Type.Type, b)]
args =
  [ (Codegen.nodePointer, "node_1"),
    (Codegen.nodePointer, "node_2"),
    (Types.eacLPointer, "eac_list")
  ]

-- TODO ∷ modify these functions to return Types.eacLPointer type

-- TODO ∷ Maybe add metadata at some point?

-- this function work off the nodeType signature not Types.eac

annihilateRewireAux ∷ Codegen.Call m ⇒ [Operand.Operand] → m Operand.Operand
annihilateRewireAux args =
  Codegen.callGen Types.eacLPointer args "annihilate_rewire_aux"

-- TODO ∷ send in eac Pointers and deallocate them as well
-- TODO ∷ fixup node type being sent in!

-- mimic rules from the interpreter
-- This rule applies to Application ↔ Lambda
defineAnnihilateRewireAux ∷ (Codegen.Define m, HasReader "debug" Int m) ⇒ m Operand.Operand
defineAnnihilateRewireAux =
  Codegen.defineFunction Types.eacLPointer "annihilate_rewire_aux" args $
    do
      node1P ← Codegen.externf "node_1"
      node2P ← Codegen.externf "node_2"
      debugLevelOne $ do
        _ ←
          Codegen.printCString
            "rule annihilate_rewire_aux: node_1 %p | node_2 %p \n"
            [node1P, node2P]
        pure ()
      aux1 ← Defs.auxiliary1
      aux2 ← Defs.auxiliary2
      -- TODO :: check if these calls create more main nodes to put back
      debugLevelOne $ do
        _ ← Codegen.printCString " firing off rewire 1\n" []
        Debug.printNodePort node1P aux1
        Debug.printNodePort node2P aux1
        _ ← Codegen.printCString " firing off rewire 2\n" []
        Debug.printNodePort node1P aux2
        Debug.printNodePort node2P aux2
      Codegen.rewire [node1P, aux1, node2P, aux1]
      Codegen.rewire [node1P, aux2, node2P, aux2]
      _ ← Codegen.deAllocateNode node1P
      _ ← Codegen.deAllocateNode node2P
      _ ← freeEac node1P
      _ ← freeEac node2P
      Codegen.externf "eac_list" >>= Codegen.ret

-- TODO ∷ fully generalize fanIn Logic and generate them dynamically

defineFanInAux0 ∷ (Codegen.Define m, HasReader "debug" Int m, Codegen.MallocNode m)
  ⇒ Symbol → m Operand.Operand → m Operand.Operand
defineFanInAux0 name allocF = Codegen.defineFunction Types.eacLPointer name args $
  do
    node ← Codegen.externf "node_1"
    fanIn ← Codegen.externf "node_2"
    debugLevelOne $ do
        -- TODO ∷ print fan label as well
        _ ←
          Codegen.printCString
            "rule fan_in_aux_0: node_1 %p | fan_in %p \n"
            [node, fanIn]
        pure ()
    eacList ← Codegen.externf "eac_list"
    era1 ← allocF
    era2 ← allocF
    aux1 ← Defs.auxiliary1
    aux2 ← Defs.auxiliary2
    debugLevelOne $ do
        -- TODO ∷ print fan label as well
        _ ← Codegen.printCString "Connecting new Node 1 main port to \n" []
        Debug.printNodePort fanIn aux1
        _ ← Codegen.printCString "Connecting new Node 2 main port to \n" []
        Debug.printNodePort fanIn aux2
        pure ()
    -- TODO :: determine if these create a new main port which we must append
    -- to the eac_list
    eacList ←
      Defs.linkAllCons
        eacList
        DSL.defRel
        { DSL.node = DSL.Node {DSL.tagNode = era1, DSL.node' = era1},
          DSL.primary = DSL.LinkConnected fanIn DSL.Aux1
        }
    eacList ←
      Defs.linkAllCons
        eacList
        DSL.defRel
        { DSL.node = DSL.Node {DSL.tagNode = era2, DSL.node' = era2},
          DSL.primary = DSL.LinkConnected fanIn DSL.Aux2
        }
    _ ← eraseNodes [node, fanIn, eacList]
    Codegen.ret eacList

aux2Gen ∷
  ( Codegen.MallocNode m,
    Codegen.Define m
  ) ⇒
  m Operand.Operand →
  Operand.Operand →
  Operand.Operand →
  Operand.Operand →
  (Operand.Operand → Operand.Operand → m ()) →
  (Operand.Operand → Operand.Operand → m ()) →
  m Operand.Operand
aux2Gen allocF node fanIn eacList copyFanData copyNodeData = do
  -- new nodes
  fan1Eac ← mallocFanIn
  fan2Eac ← mallocFanIn
  nod1Eac ← allocF
  nod2Eac ← allocF
  fan1 ← nodeOf fan1Eac
  fan2 ← nodeOf fan2Eac
  nod1 ← nodeOf nod1Eac
  nod2 ← nodeOf nod2Eac
  -- TODO :: determine if these create a new main port which we must append
  -- to the eac_list
  eacList ←
    Defs.linkAllCons
      eacList
      DSL.defRel
        { DSL.node = DSL.Node {DSL.tagNode = nod1Eac, DSL.node' = nod1},
          DSL.primary = DSL.LinkConnected fanIn DSL.Aux1,
          DSL.auxiliary1 = DSL.Link fan2 DSL.Aux1,
          DSL.auxiliary2 = DSL.Link fan1 DSL.Aux1
        }
  eacList ←
    Defs.linkAllCons
      eacList
      DSL.defRel
        { DSL.node = DSL.Node {DSL.tagNode = nod2Eac, DSL.node' = nod2},
          DSL.primary = DSL.LinkConnected fanIn DSL.Aux2,
          DSL.auxiliary1 = DSL.Link fan2 DSL.Aux2,
          DSL.auxiliary2 = DSL.Link fan1 DSL.Aux2
        }
  eacList ←
    Defs.linkAllCons
      eacList
      DSL.defRel
        { DSL.node = DSL.Node {DSL.tagNode = fan1Eac, DSL.node' = fan1},
          DSL.primary = DSL.LinkConnected node DSL.Aux2
        }
  eacList ←
    Defs.linkAllCons
      eacList
      DSL.defRel
        { DSL.node = DSL.Node {DSL.tagNode = fan2Eac, DSL.node' = fan2},
          DSL.primary = DSL.LinkConnected node DSL.Aux1
        }
  copyNodeData node nod1 -- with the custom function we may ignore node/FanIn
  copyNodeData node nod2 -- for one that already grabs data to save some
  copyFanData fanIn fan1 -- operations as to get here we may have already
  copyFanData fanIn fan2 -- gotten it
  eraseNodes [node, fanIn, eacList]

defineFanInAux2 ∷
  ( Codegen.MallocNode m,
    Codegen.Define m,
    HasReader "debug" Int m
  ) ⇒
  Symbol →
  m Operand.Operand →
  m Operand.Operand
defineFanInAux2 name allocF = Codegen.defineFunction Types.eacLPointer name args $
  do
    -- Nodes in env
    node ← Codegen.externf "node_1"
    fanIn ← Codegen.externf "node_2"
    eacList ← Codegen.externf "eac_list"
    eacList ←
      aux2Gen
        allocF
        node
        fanIn
        eacList
        (\oldF newF → Codegen.loadDataArray oldF >>= addDataWhole newF)
        (\_ _ → pure ()) -- no data on node for now!
    Codegen.ret eacList

fanInFanIn ∷ Codegen.Call m ⇒ [Operand.Operand] → m Operand.Operand
fanInFanIn args = Codegen.callGen Types.eacLPointer args "fan_in_rule"

defineFanInFanIn ∷
  ( Codegen.MallocNode m,
    Codegen.Define m
  ) ⇒
  m Operand.Operand
defineFanInFanIn = Codegen.defineFunction Types.eacLPointer "fan_in_rule" args $
  do
    eacList ← Codegen.externf "eac_list"
    fanIn1 ← Codegen.externf "node_1"
    fanIn2 ← Codegen.externf "node_2"
    data1 ← Codegen.loadDataArray fanIn1
    data2 ← Codegen.loadDataArray fanIn2
    label1 ← fanLabelLookup data1
    label2 ← fanLabelLookup data2
    test ← Codegen.icmp IntPred.EQ label1 label2
    sameFan ← Codegen.addBlock "same.fan"
    diffFan ← Codegen.addBlock "diff.fan"
    _ ← Codegen.cbr test sameFan diffFan
    -- %same.fan
    ------------------------------------------------------
    Codegen.setBlock sameFan
    aux1 ← Defs.auxiliary1
    aux2 ← Defs.auxiliary2
    Codegen.rewire [fanIn1, aux1, fanIn2, aux2]
    Codegen.rewire [fanIn1, aux2, fanIn2, aux1]
    let sameList = eacList
    _ ← eraseNodes [fanIn1, fanIn2, eacList]
    -- %diff.fan
    ------------------------------------------------------
    Codegen.setBlock diffFan
    diffList ←
      aux2Gen
        mallocFanIn
        fanIn1
        fanIn2
        eacList
        (\_ newF → addData newF label2)
        (\_ newN → addData newN label1)
    lastDiff ← Codegen.getBlock
    -- weird ordering to get continue.fan to be last!
    continue ← Codegen.addBlock "continue.fan"
    _ ← Codegen.br continue
    -- %same.fan
    ------------------------------------------------------
    Codegen.setBlock sameFan
    _ ← Codegen.br continue
    -- %continue.fan
    ------------------------------------------------------
    Codegen.setBlock continue
    finalList ←
      Codegen.phi
        Types.eacLPointer
        [(sameList, sameFan), (diffList, lastDiff)]
    Codegen.ret finalList

-- TODO ∷ remove, put these in the environment with some kind of decalarative
-- dispatch system that can handle dynamic node addition

-- instantiations
defineFanInAux2F,
  defineFanInAux2A,
  defineFanInAux2L,
  defineFanInAux0E ∷
    ( Codegen.MallocNode m,
      Codegen.Define m,
      HasReader "debug" Int m
    ) ⇒
    m Operand.Operand
defineFanInAux2A = defineFanInAux2 "fan_in_aux_2_app" mallocApp
defineFanInAux2F = defineFanInAux2 "fan_in_aux_2_fan_in" mallocFanIn
defineFanInAux2L = defineFanInAux2 "fan_in_aux_2_lambda" mallocLam
defineFanInAux0E = defineFanInAux0 "fan_in_aux_0_era" mallocEra

fanInAux2App,
  fanInAux2FanIn,
  fanInAux2Lambda,
  fanInAux0Era ∷
    Codegen.Call m ⇒ [Operand.Operand] → m Operand.Operand
fanInAux2App args = Codegen.callGen Types.eacLPointer args "fan_in_aux_2_app"
fanInAux2FanIn args = Codegen.callGen Types.eacLPointer args "fan_in_aux_2_fan_in"
fanInAux2Lambda args = Codegen.callGen Types.eacLPointer args "fan_in_aux_2_lambda"
fanInAux0Era args = Codegen.callGen Types.eacLPointer args "fan_in_aux_0_era"

eraseNodes ∷ Codegen.Call m ⇒ [Operand.Operand] → m Operand.Operand
eraseNodes args = Codegen.callGen Types.eacLPointer args "erase_nodes"

defineEraseNodes ∷ Codegen.Define m ⇒ m Operand.Operand
defineEraseNodes = Codegen.defineFunction Types.eacLPointer "erase_nodes" args $
  do
    nodePtr1 ← Codegen.externf "node_1"
    nodePtr2 ← Codegen.externf "node_2"
    _ ← Codegen.deAllocateNode nodePtr1
    _ ← Codegen.deAllocateNode nodePtr2
    Codegen.externf "eac_list" >>= Codegen.ret

--------------------------------------------------------------------------------
-- Allocations
--------------------------------------------------------------------------------

freeEac ∷ Codegen.Call m ⇒ Operand.Operand → m ()
freeEac = Codegen.free

mallocGen ∷
  Codegen.MallocNode m ⇒ C.Constant → Int → Int → m Operand.Operand
mallocGen type' portLen dataLen = do
  -- malloc call
  node ← Defs.mallocNodeH (replicate portLen Nothing) (replicate dataLen Nothing)
  tagPtr ← Types.tagOf node
  Codegen.store tagPtr (Operand.ConstantOperand type')
  pure node

mallocTop,
  mallocEra,
  mallocFanIn,
  mallocApp,
  mallocLam ∷
    Codegen.MallocNode m ⇒ m Operand.Operand
mallocTop = mallocGen Types.top 2 0
mallocEra = mallocGen Types.era 1 0
mallocApp = mallocGen Types.app 3 0
mallocLam = mallocGen Types.lam 3 0
mallocFanIn = mallocGen Types.dup 3 1

nodeOf ∷ Codegen.RetInstruction m ⇒ Operand.Operand → m Operand.Operand
nodeOf eac = pure eac

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

addData ∷ Codegen.RetInstruction m ⇒ Operand.Operand → Operand.Operand → m ()
addData addr data' = do
  arr ← Codegen.loadDataArray addr
  labPtr ← Codegen.getElementPtr $
    Codegen.Minimal
      { Codegen.type' = Codegen.pointerOf Codegen.dataType,
        Codegen.address' = arr,
        Codegen.indincies' = Codegen.constant32List [0, 0]
      }
  Codegen.store labPtr data'

addDataWhole ∷ Codegen.RetInstruction m ⇒ Operand.Operand → Operand.Operand → m ()
addDataWhole addr data' = do
  arr ← Codegen.getDataArray addr
  Codegen.store arr data'

fanLabelLookup ∷ Codegen.RetInstruction m ⇒ Operand.Operand → m Operand.Operand
fanLabelLookup addr = Codegen.loadElementPtr $
  Codegen.Minimal
    { Codegen.type' = Codegen.dataType,
      Codegen.address' = addr,
      Codegen.indincies' = Codegen.constant32List [0, 0]
    }

-- dumb define test
defineTest ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.T Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Codegen.Names m,
    HasState "symTab" Codegen.SymbolTable m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m,
    HasReader "debug" Int m
  ) ⇒
  m Operand.Operand
defineTest = Codegen.defineFunction Types.eacPointer "test_function" [] $ do
  era ← mallocEra
  app ← mallocApp
  main ← Codegen.mainPort
  debugLevelOne $ do
    tag ← Types.tagOf era >>= Codegen.load Types.tag
    _ ← Codegen.printCString "eraTag %i \n" [tag]
    _ ← Codegen.printCString "eraPtr %p \n" [era]
    pure ()
  Codegen.link [era, main, app, main]
  _ ← Codegen.free app
  Codegen.ret era

-- TODO ∷ remove when the segfault in LLVM2 is over!
testLink ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.T Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Codegen.Names m,
    HasState "symTab" Codegen.SymbolTable m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m,
    HasReader "debug" Int m
  ) ⇒
  m Operand.Operand
testLink = Codegen.defineFunction Type.void "test_link" [] $ do
  era ← mallocEra
  app ← mallocApp
  main ← Codegen.mainPort
  Codegen.link [era, main, app, main]
  debugLevelOne $ do
    portEra ← Codegen.getPort era main
    hpefullyAppNode ← Codegen.loadElementPtr $
      Codegen.Minimal
        { Codegen.type' = Codegen.nodePointer,
          Codegen.address' = portEra,
          Codegen.indincies' = Codegen.constant32List [0, 0]
        }
    hopefullyMainPort ← Codegen.loadElementPtr $
      Codegen.Minimal
        { Codegen.type' = Codegen.numPortsNameRef,
          Codegen.address' = portEra,
          Codegen.indincies' = Codegen.constant32List [0, 1]
        }
    _ ← Codegen.printCString "appPointer %p \n" [app]
    _ ← Codegen.printCString "mainPortEra: port %i, node %p \n" [hopefullyMainPort, hpefullyAppNode]
    pure ()
  _ ← Codegen.free app
  _ ← Codegen.free era
  Codegen.retNull
