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
import qualified Juvix.Backends.LLVM.Net.EAC.Defs as Defs
import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import Juvix.Library hiding (reduce)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IntPred
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type

--------------------------------------------------------------------------------
-- Interaction Net runner
--------------------------------------------------------------------------------

-- TODO ∷ consider the return type
-- TODO ∷ remove boileprlate
defineReduce ∷ Codegen.Define m ⇒ m Operand.Operand
defineReduce = Codegen.defineFunction Type.void "reduce" args $
  do
    -- recursive function, properly register
    reduce ← Codegen.externf "reduce"
    -- switch creations
    eacLPtr ← Codegen.externf "eac_list"
    appCase ← Codegen.addBlock "switch.app"
    lamCase ← Codegen.addBlock "switch.lam"
    eraCase ← Codegen.addBlock "switch.era"
    dupCase ← Codegen.addBlock "switch.dup"
    defCase ← Codegen.addBlock "switch.default"
    extCase ← Codegen.addBlock "switch.exit"
    nullCase ← Codegen.addBlock "empty.list"
    carExists ← Codegen.addBlock "car.check"
    nullCheck ← Types.checkNull eacLPtr
    _ ← Codegen.cbr nullCheck carExists nullCase
    -- %empty.list branch
    ------------------------------------------------------
    Codegen.setBlock nullCase
    _ ← Codegen.retNull
    -- %car.check branch
    ------------------------------------------------------
    Codegen.setBlock carExists
    eacList ← Types.loadList eacLPtr
    car ← Types.loadCar eacList
    -- TODO ∷ cdr may still be unsafe!??!?!
    cdr ← Types.loadCdr eacList
    -- moved from %app case.
    -- Should not do extra work being here----
    nodePtr ← nodeOf car
    tagNode ← Defs.isBothPrimary [nodePtr]
    isPrimary ← Codegen.loadIsPrimaryEle tagNode
    test ←
      Codegen.icmp
        IntPred.EQ
        isPrimary
        (Operand.ConstantOperand (C.Int 2 1))
    -- end of moved code----------------------
    tag ← tagOf car
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

        genContinueCaseD = genContinueCase tagNode (nodePtr, car) cdr defCase

        swapArgs (x : y : xs) = y : x : xs
        swapArgs xs = xs

    -- %app case
    ------------------------------------------------------
    contCase appCase "switch.app.continue"
    (aCdr, aExit) ←
      genContinueCaseD
        "app"
        [ (Types.lam, "switch.lam", (\x → annihilateRewireAux x >> pure cdr)),
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
        [ (Types.app, "switch.app", (\x → annihilateRewireAux (swapArgs x) >> pure cdr)),
          (Types.dup, "switch.dup", fanInAux2Lambda),
          (Types.era, "switch.era", eraseNodes)
        ]
    _ ← Codegen.br extCase
    -- %era case
    ------------------------------------------------------
    contCase lamCase "switch.era.continue"
    (eCdr, eExit) ←
      genContinueCaseD
        "fan_in"
        [ (Types.app, "switch.app", eraseNodes),
          (Types.lam, "switch.lam", eraseNodes),
          (Types.dup, "switch.dup", fanInAux2Era),
          (Types.era, "switch.era", eraseNodes)
        ]
    _ ← Codegen.br extCase
    -- %dup case
    ------------------------------------------------------
    contCase lamCase "switch.fan_in.continue"
    (fCdr, fExit) ←
      genContinueCaseD
        "fan_in"
        [ (Types.app, "switch.app", fanInAux2App . swapArgs),
          (Types.lam, "switch.lam", fanInAux2Lambda . swapArgs),
          (Types.dup, "switch.dup", fanInFanIn),
          (Types.era, "switch.era", fanInAux2Era . swapArgs)
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
        Types.eacList
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
    Codegen.call Type.void reduce (Codegen.emptyArgs [cdr])
  where
    args = [(Types.eacPointer, "eac_list")]

--------------------------------------------------------------------------------
-- Code generation rules
--------------------------------------------------------------------------------

genContinueCase ∷
  Codegen.Define m ⇒
  Operand.Operand →
  (Operand.Operand, Operand.Operand) →
  Operand.Operand →
  Name.Name →
  Symbol →
  [(C.Constant, Symbol, [Operand.Operand] → m Operand.Operand)] →
  m (Operand.Operand, Name.Name)
genContinueCase tagNode (mainNodePtr, mainEac) cdr defCase prefix cases = do
  nodeEacPtr ← Defs.loadPrimaryNode tagNode
  nodeEac ← Codegen.load Types.eac nodeEacPtr
  tagOther ← tagOf nodeEac
  blocksGeneratedList ← genBlockNames
  extBranch ← Codegen.addBlock (prefix <> "switch.exit")
  let generateBody (_, branch, rule) = do
        -- %prefix.branch case
        ------------------------------------------------------
        _ ← Codegen.setBlock branch
        -- node Pointer in which to do operations on
        nodeOtherPtr ← nodeOf nodeEac
        updateList ←
          rule
            [ mainNodePtr,
              nodeEacPtr,
              nodeOtherPtr,
              mainEac,
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
      Types.eacList
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
    (Types.eacList, "eac_list")
  ]

-- TODO ∷ modify these functions to return Types.eacLPointer type

-- TODO ∷ Maybe add metadata at some point?

-- this function work off the nodeType signature not Types.eac

annihilateRewireAux ∷ Codegen.Call m ⇒ [Operand.Operand] → m Operand.Operand
annihilateRewireAux args =
  Codegen.callGen Types.eacList args "annihilate_rewire_aux"

-- TODO ∷ send in eac Pointers and deallocate them as well
-- TODO ∷ fixup node type being sent in!

-- mimic rules from the interpreter
-- This rule applies to Application ↔ Lambda
defineAnnihilateRewireAux ∷ Codegen.Define m ⇒ m Operand.Operand
defineAnnihilateRewireAux =
  Codegen.defineFunction Types.eacList "annihilate_rewire_aux" args $
    do
      aux1 ← Defs.auxiliary1
      aux2 ← Defs.auxiliary2
      node1P ← Codegen.externf "node_1"
      node2P ← Codegen.externf "node_2"
      node1 ← Codegen.load Codegen.nodeTypeNameRef node1P
      node2 ← Codegen.load Codegen.nodeTypeNameRef node2P
      -- TODO :: check if these calls create more main nodes to put back
      Codegen.rewire [node1, aux1, node2, aux1]
      Codegen.rewire [node1, aux2, node2, aux2]
      _ ← Codegen.deAllocateNode node1P
      _ ← Codegen.deAllocateNode node2P
      _ ← freeEac node1P
      _ ← freeEac node2P
      Codegen.externf "eac_list" >>= Codegen.ret

-- TODO ∷ fully generalize fanIn Logic and generate them dynamically

fanInAux0 ∷ Codegen.Define m ⇒ m Operand.Operand → m Operand.Operand
fanInAux0 allocF = Codegen.defineFunction Types.eacList "fan_in_aux_0" args $
  do
    node ← Codegen.externf "node_1"
    fanIn ← Codegen.externf "node_2"
    era1 ← allocF
    era2 ← allocF
    aux1 ← Defs.auxiliary1
    aux2 ← Defs.auxiliary2
    mainPort ← Defs.mainPort
    -- TODO :: determine if these create a new main port which we must append
    -- to the eac_list
    Codegen.linkConnectedPort [fanIn, aux1, era1, mainPort]
    Codegen.linkConnectedPort [fanIn, aux2, era2, mainPort]
    eacList ← Codegen.externf "eac_list"
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
    Codegen.Define m
  ) ⇒
  Symbol →
  m Operand.Operand →
  m Operand.Operand
defineFanInAux2 name allocF = Codegen.defineFunction Types.eacList name args $
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
        (\oldF newF → dataPortLookup oldF >>= addData newF)
        (\_ _ → pure ()) -- no data on node for now!
    Codegen.ret eacList

fanInFanIn ∷ Codegen.Call m ⇒ [Operand.Operand] → m Operand.Operand
fanInFanIn args = Codegen.callGen Types.eacList args "fan_in_rule"

defineFanInFanIn ∷
  ( Codegen.MallocNode m,
    Codegen.Define m
  ) ⇒
  m Operand.Operand
defineFanInFanIn = Codegen.defineFunction Types.eacList "fan_in_rule" args $
  do
    eacList ← Codegen.externf "eac_list"
    fanIn1 ← Codegen.externf "node_1"
    fanIn2 ← Codegen.externf "node_2"
    data1 ← dataPortLookup fanIn1
    data2 ← dataPortLookup fanIn2
    label1 ← fanLabelLookup data1
    label2 ← fanLabelLookup data2
    test ← Codegen.icmp IntPred.EQ label1 label2
    sameFan ← Codegen.addBlock "same.fan"
    diffFan ← Codegen.addBlock "diff.fan"
    continue ← Codegen.addBlock "continue.fan"
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
    _ ← Codegen.br continue
    -- %diff.fan
    ------------------------------------------------------
    Codegen.setBlock diffFan
    diffList ←
      aux2Gen
        mallocFanIn
        fanIn1
        fanIn2
        eacList
        (\_ newF → addData newF data2)
        (\_ newN → addData newN data1)
    _ ← Codegen.br continue
    -- %continue.fan
    ------------------------------------------------------
    finalList ←
      Codegen.phi
        Types.eacList
        [(sameList, sameFan), (diffList, diffFan)]
    Codegen.ret finalList

-- TODO ∷ remove, put these in the environment with some kind of decalarative
-- dispatch system that can handle dynamic node addition

-- instantiations
defineFanInAux2F,
  defineFanInAux2A,
  defineFanInAux2L,
  defineFanInAux2E ∷
    ( Codegen.MallocNode m,
      Codegen.Define m
    ) ⇒
    m Operand.Operand
defineFanInAux2A = defineFanInAux2 "fan_in_aux_2_app" mallocApp
defineFanInAux2F = defineFanInAux2 "fan_in_aux_2_fan_in" mallocFanIn
defineFanInAux2L = defineFanInAux2 "fan_in_aux_2_fan_in" mallocFanIn
defineFanInAux2E = defineFanInAux2 "fan_in_aux_2_era" mallocEra

fanInAux2App,
  fanInAux2FanIn,
  fanInAux2Lambda,
  fanInAux2Era ∷
    Codegen.Call m ⇒ [Operand.Operand] → m Operand.Operand
fanInAux2App args = Codegen.callGen Type.void args "fan_in_aux_2_app"
fanInAux2Era args = Codegen.callGen Type.void args "fan_in_aux_2_era"
fanInAux2FanIn args = Codegen.callGen Type.void args "fan_in_aux_2_fan_in"
fanInAux2Lambda args = Codegen.callGen Type.void args "fan_in_aux_2_fan_in"

eraseNodes ∷ Codegen.Call m ⇒ [Operand.Operand] → m Operand.Operand
eraseNodes args = Codegen.callGen Types.eacList args "erase_nodes"

defineEraseNodes ∷ Codegen.Define m ⇒ m Operand.Operand
defineEraseNodes = Codegen.defineFunction Types.eacList "erase_nodes" args $
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
  tagPtr ← tagOf node
  Codegen.store tagPtr (Operand.ConstantOperand type')
  pure node

mallocEra,
  mallocFanIn,
  mallocApp,
  mallocLam ∷
    Codegen.MallocNode m ⇒ m Operand.Operand
mallocEra = mallocGen Types.era 1 0
mallocApp = mallocGen Types.app 3 0
mallocLam = mallocGen Types.lam 3 0
mallocFanIn = mallocGen Types.dup 3 1

nodeOf ∷ Codegen.RetInstruction m ⇒ Operand.Operand → m Operand.Operand
nodeOf eac = pure eac

tagOf ∷ Codegen.RetInstruction m ⇒ Operand.Operand → m Operand.Operand
tagOf eac =
  Codegen.getElementPtr $
    Codegen.Minimal
      { Codegen.type' = Codegen.pointerOf Types.tag,
        Codegen.address' = eac,
        Codegen.indincies' = Codegen.constant32List [0, 3]
      }

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

dataPortLookup ∷ Codegen.RetInstruction m ⇒ Operand.Operand → m Operand.Operand
dataPortLookup addr = Codegen.loadElementPtr $
  Codegen.Minimal
    { Codegen.type' = Codegen.dataArray,
      Codegen.address' = addr,
      Codegen.indincies' = Codegen.constant32List [0, 2]
    }

addData ∷ Codegen.RetInstruction m ⇒ Operand.Operand → Operand.Operand → m ()
addData addr data' = do
  arr ← dataPortLookup addr
  labPtr ← Codegen.getElementPtr $
    Codegen.Minimal
      { Codegen.type' = Codegen.pointerOf Codegen.dataType,
        Codegen.address' = arr,
        Codegen.indincies' = Codegen.constant32List [0, 0]
      }
  Codegen.store labPtr data'

fanLabelLookup ∷ Codegen.RetInstruction m ⇒ Operand.Operand → m Operand.Operand
fanLabelLookup addr = Codegen.loadElementPtr $
  Codegen.Minimal
    { Codegen.type' = Codegen.dataType,
      Codegen.address' = addr,
      Codegen.indincies' = Codegen.constant32List [0, 0]
    }

-- dumb define test
defineTest = Codegen.defineFunction Types.eacPointer "test_function" [] $ do
  era ← mallocEra
  app ← mallocApp
  main ← Codegen.mainPort
  Codegen.link [era, main, app, main]
  _ ← Codegen.free app
  Codegen.ret era
