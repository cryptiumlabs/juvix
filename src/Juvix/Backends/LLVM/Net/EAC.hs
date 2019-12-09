-- |
-- - =EAC= serves as the place where the interaction net rules for the
--   EAC layer gets run
-- - The form given to =EAC= is not the base EAC AST, but instead a
--   pre processed =EAC= graph that the initial graph will be made on
module Juvix.Backends.LLVM.Net.EAC where

-- TODO ∷ abstract all all imports to LLVM

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import qualified Juvix.Backends.LLVM.DSL as DSL
import qualified Juvix.Backends.LLVM.Net.EAC.Defs as Defs
import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import Juvix.Library hiding (reduce)
import qualified Juvix.Library.HashMap as Map
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
reduce ∷ Codegen.Define m ⇒ m Operand.Operand
reduce = Codegen.defineFunction Type.void "reduce" args $
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

        genContinueCaseD = genContinueCase tagNode nodePtr cdr defCase

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
          (Types.era, "switch.era", (\xs → undefined xs))
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
          (Types.era, "switch.era", (\xs → undefined xs))
        ]
    _ ← Codegen.br extCase
    -- %era case
    ------------------------------------------------------
    contCase lamCase "switch.era.continue"
    (eCdr, eExit) ←
      genContinueCaseD
        "fan_in"
        [ (Types.app, "switch.app", undefined),
          (Types.lam, "switch.lam", undefined),
          (Types.dup, "switch.dup", fanInAux2Era),
          (Types.era, "switch.era", undefined)
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
          (Types.dup, "switch.dup", fanInAux2FanIn . swapArgs),
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
    Codegen.call Type.void reduce (Codegen.emptyArgs [cdr])
  where
    args = [(Types.eacPointer, "eac_list")]

--------------------------------------------------------------------------------
-- Code generation rules
--------------------------------------------------------------------------------

genContinueCase ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "names" Codegen.Names m
  ) ⇒
  Operand.Operand →
  Operand.Operand →
  Operand.Operand →
  Name.Name →
  Symbol →
  [(C.Constant, Symbol, [Operand.Operand] → m Operand.Operand)] →
  m (Operand.Operand, Name.Name)
genContinueCase tagNode nodePtr cdr defCase prefix cases = do
  nodeEac ← Defs.loadPrimaryNode tagNode >>= Codegen.load Types.eac
  tagOther ← tagOf nodeEac
  blocksGeneratedList ← genBlockNames
  extBranch ← Codegen.addBlock (prefix <> "switch.exit")
  let generateBody (_, branch, rule) = do
        -- %prefix.branch case
        ------------------------------------------------------
        _ ← Codegen.setBlock branch
        -- node Types in which to do operations on
        nodeOther' ← nodeOf nodeEac >>= Codegen.load Defs.nodeType
        nodePrimar ← Codegen.load Defs.nodeType nodePtr
        updateList ← rule [nodePrimar, nodeOther', cdr]
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

-- TODO ∷ modify these functions to return Types.eacLPointer type

-- TODO ∷ Maybe add metadata at some point?

-- this function work off the nodeType signature not Types.eac

annihilateRewireAux ∷ Codegen.Call m ⇒ [Operand.Operand] → m ()
annihilateRewireAux args = do
  annihilate ← Codegen.externf "annihilate_rewire_aux"
  _ ← Codegen.call Type.void annihilate (Codegen.emptyArgs args)
  pure ()

-- mimic rules from the interpreter
-- This rule applies to Application ↔ Lambda
annihilateRewireAux' ∷ Codegen.Define m ⇒ m Operand.Operand
annihilateRewireAux' = Codegen.defineFunction Type.void "annihilate_rewire_aux" args $
  do
    -- TODO remove these explicit allocations
    aux1 ← Defs.auxiliary1
    aux2 ← Defs.auxiliary2
    node1 ← Codegen.externf "node_1"
    node2 ← Codegen.externf "node_2"
    Codegen.rewire [node1, aux1, node2, aux1]
    Codegen.rewire [node1, aux2, node2, aux2]
    _ ← Codegen.delNode node1
    Codegen.delNode node2
  where
    args = [(Defs.nodeType, "node_1"), (Defs.nodeType, "node_2")]

-- TODO ∷ make fast fanInAux and slow fanInAux

-- | 'fanInAuxStar' is a slower version of 'fanInAux*' where * ∈ ℤ/4ℤ.
-- This function is used when it can not be determined that 'fanInAux*'
fanInAuxStar = undefined

fanInAux0 ∷ Codegen.Define m ⇒ m Operand.Operand → m Operand.Operand
fanInAux0 allocF = Codegen.defineFunction Type.void "fan_in_aux_0" args $
  do
    fanIn ← Codegen.externf "fan_in"
    era1 ← allocF >>= nodeOf
    era2 ← allocF >>= nodeOf
    aux1 ← Defs.auxiliary1
    aux2 ← Defs.auxiliary2
    mainPort ← Defs.mainPort
    -- abstract out this string!
    Codegen.linkConnectedPort [fanIn, aux1, era1, mainPort]
    Codegen.linkConnectedPort [fanIn, aux2, era2, mainPort]
    Codegen.retNull
  where
    args =
      [ (Defs.nodeType, "node"), -- we send in the alloc function for it. Do case before
        (Defs.nodeType, "fan_in") -- we know this must be a fanIn so no need for tag
      ]

fanInAux1' ∷
  ( Codegen.Define m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m
  ) ⇒
  Symbol →
  m Operand.Operand →
  m Operand.Operand
fanInAux1' _allocF = undefined

fanInAux2' ∷
  ( Codegen.Define m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m
  ) ⇒
  Symbol →
  m Operand.Operand →
  m Operand.Operand
fanInAux2' name allocF = Codegen.defineFunction Type.void name args $
  do
    -- Nodes in env
    fanIn ← Codegen.externf "fan_in"
    node ← Codegen.externf "node"
    -- new nodes
    fan1 ← mallocFanIn >>= nodeOf
    fan2 ← mallocFanIn >>= nodeOf
    nod1 ← allocF >>= nodeOf
    nod2 ← allocF >>= nodeOf
    Defs.linkAll
      DSL.defRel
        { DSL.node = nod1,
          DSL.primary = DSL.LinkConnected fanIn DSL.Aux1,
          DSL.auxiliary1 = DSL.Link fan2 DSL.Aux1,
          DSL.auxiliary2 = DSL.Link fan1 DSL.Aux1
        }
    Defs.linkAll
      DSL.defRel
        { DSL.node = nod2,
          DSL.primary = DSL.LinkConnected fanIn DSL.Aux2,
          DSL.auxiliary1 = DSL.Link fan2 DSL.Aux2,
          DSL.auxiliary2 = DSL.Link fan1 DSL.Aux2
        }
    Defs.linkAll
      DSL.defRel
        { DSL.node = fan1,
          DSL.primary = DSL.LinkConnected node DSL.Aux2
        }
    Defs.linkAll
      DSL.defRel
        { DSL.node = fan2,
          DSL.primary = DSL.LinkConnected node DSL.Aux1
        }
    Codegen.retNull
  where
    args =
      [ (Defs.nodeType, "node"),
        (Defs.nodeType, "fan_in")
      ]

-- TODO ∷ remove, put these in the environment with some kind of decalarative
-- dispatch system that can handle dynamic node addition

-- instantiations
fanInAux2F',
  fanInAux2A',
  fanInAux2L',
  fanInAux2E' ∷
    ( Codegen.Define m,
      HasState "typTab" Codegen.TypeTable m,
      HasState "varTab" Codegen.VariantToType m
    ) ⇒
    m Operand.Operand
fanInAux2A' = fanInAux2' "fan_in_aux_2_app" mallocApp
fanInAux2F' = fanInAux2' "fan_in_aux_2_fan_in" mallocFanIn
fanInAux2L' = fanInAux2' "fan_in_aux_2_fan_in" mallocFanIn
fanInAux2E' = fanInAux2' "fan_in_aux_2_era" mallocEra

fanInAux2App,
  fanInAux2FanIn,
  fanInAux2Lambda,
  fanInAux2Era ∷
    Codegen.Call m ⇒ [Operand.Operand] → m Operand.Operand
fanInAux2App args = Codegen.callGen Type.void args "fan_in_aux_2_app"
fanInAux2Era args = Codegen.callGen Type.void args "fan_in_aux_2_era"
fanInAux2FanIn args = Codegen.callGen Type.void args "fan_in_aux_2_fan_in"
fanInAux2Lambda args = Codegen.callGen Type.void args "fan_in_aux_2_fan_in"

--------------------------------------------------------------------------------
-- Allocations
--------------------------------------------------------------------------------
mallocGen ∷
  ( Codegen.Call m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m
  ) ⇒
  C.Constant →
  Int →
  Int →
  m Operand.Operand
mallocGen type' portLen dataLen = do
  eac ← Codegen.mallocType Types.tagInt Types.eac
  node ← Defs.mallocNodeH (replicate portLen Nothing) (replicate dataLen Nothing)
  tagPtr ← Codegen.getElementPtr $
    Codegen.Minimal
      { Codegen.type' = Types.tag,
        Codegen.address' = eac,
        Codegen.indincies' = Codegen.constant32List [0, 0]
      }
  Codegen.store tagPtr (Operand.ConstantOperand type')
  nodePtr ← Codegen.getElementPtr $
    Codegen.Minimal
      { Codegen.type' = Defs.nodeType,
        Codegen.address' = eac,
        Codegen.indincies' = Codegen.constant32List [0, 1]
      }
  Codegen.store nodePtr node
  pure eac

mallocEra,
  mallocFanIn,
  mallocApp,
  mallocLam ∷
    ( Codegen.Call m,
      HasState "typTab" Codegen.TypeTable m,
      HasState "varTab" Codegen.VariantToType m
    ) ⇒
    m Operand.Operand
mallocEra = mallocGen Types.era 1 0
mallocApp = mallocGen Types.app 3 0
mallocLam = mallocGen Types.lam 3 0
mallocFanIn = mallocGen Types.dup 3 1

nodeOf ∷ Codegen.RetInstruction m ⇒ Operand.Operand → m Operand.Operand
nodeOf eac =
  Codegen.loadElementPtr $
    Codegen.Minimal
      { Codegen.type' = Defs.nodeType,
        Codegen.address' = eac,
        Codegen.indincies' = Codegen.constant32List [0, 1]
      }

tagOf ∷ Codegen.RetInstruction m ⇒ Operand.Operand → m Operand.Operand
tagOf eac =
  Codegen.loadElementPtr $
    Codegen.Minimal
      { Codegen.type' = Types.tag,
        Codegen.address' = eac,
        Codegen.indincies' = Codegen.constant32List [0, 0]
      }
