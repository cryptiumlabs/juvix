-- |
-- - =EAC= serves as the place where the interaction net rules for the
--   EAC layer gets run
-- - The form given to =EAC= is not the base EAC AST, but instead a
--   pre processed =EAC= graph that the initial graph will be made on
module Juvix.Backends.LLVM.Net.EAC where

-- TODO ∷ abstract all all imports to LLVM

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import Juvix.Library hiding (reduce)
import qualified Juvix.Library.HashMap as Map
import qualified LLVM.AST as AST
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
reduce ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Codegen.Names m,
    HasState "symtab" (Map.HashMap Symbol Operand.Operand) m
  ) ⇒
  m Operand.Operand
reduce = Codegen.defineFunction Type.void "reduce" args $
  do
    -- recursive function, properly register
    reduce ← Codegen.externf "reduce"
    isBothPrimary ← Codegen.externf "is_both_primary"
    anihilateRewireAux ← Codegen.externf "anihilate_rewire_aux"
    -- switch creations
    eacList ← Codegen.externf "eac_list"
    appCase ← Codegen.addBlock "switch.app"
    lamCase ← Codegen.addBlock "switch.lam"
    eraCase ← Codegen.addBlock "switch.era"
    dupCase ← Codegen.addBlock "switch.dup"
    defCase ← Codegen.addBlock "switch.default"
    extCase ← Codegen.addBlock "switch.exit"
    car ← Types.loadCar eacList
    cdr ← Types.loadCdr eacList
    tag ← tagOf car
    _term ←
      Codegen.switch
        tag
        defCase
        [ (Types.app, appCase),
          (Types.lam, lamCase),
          (Types.era, eraCase),
          (Types.dup, dupCase)
        ]
    -- %app case
    ------------------------------------------------------
    Codegen.setBlock appCase
    -- nested switch cases
    appLamCase ← Codegen.addBlock "switch.app.lam"
    appEraCase ← Codegen.addBlock "switch.app.era"
    appDupCase ← Codegen.addBlock "switch.app.dup"
    appExtCase ← Codegen.addBlock "switch.app.exit"
    -- TODO ∷ Prove this branch is unnecessary
    appContCase ← Codegen.addBlock "switch.app.continue"
    nodePtr ← nodeOf car
    tagNode ← Codegen.call Codegen.bothPrimary isBothPrimary (Codegen.emptyArgs [nodePtr])
    isPrimary ← Codegen.loadIsPrimaryEle tagNode
    -- TODO ∷ Prove this branch is unnecessary
    test ←
      Codegen.icmp
        IntPred.EQ
        isPrimary
        (Operand.ConstantOperand (C.Int 2 1))
    Codegen.cbr test appContCase extCase
    -- %switch.app.continue
    ---------------------------------------------
    -- TODO ∷ fix the type of this.... need the rules to work on eac!!!
    Codegen.setBlock appContCase
    nodeOther ← Codegen.loadPrimaryNode isPrimary >>= Codegen.load Codegen.nodeType
    tagOther ← tagOf nodeOther
    _ ←
      Codegen.switch
        tagOther
        defCase
        [ (Types.lam, appLamCase),
          (Types.era, appEraCase),
          (Types.dup, appDupCase)
        ]
    -- %switch.app.lam
    ---------------------------------------------
    Codegen.setBlock appLamCase
    nodeOther ← Codegen.loadPrimaryNode isPrimary >>= Codegen.load Codegen.nodeType
    node ← Codegen.load Codegen.nodeType nodePtr
    -- No new nodes are made
    _ ← Codegen.call Type.void anihilateRewireAux (Codegen.emptyArgs [node, nodeOther])
    aCdr ← pure cdr
    aCdr ← undefined
    Codegen.br extCase
    -- %lam case
    ------------------------------------------------------
    Codegen.setBlock lamCase
    -- nested switch cases
    lamAppCase ← Codegen.addBlock "switch.lam.app"
    lamEraCase ← Codegen.addBlock "switch.lam.era"
    lamDupCase ← Codegen.addBlock "switch.lam.dup"
    lamExtCase ← Codegen.addBlock "switch.lam.exit"
    lCdr ← undefined
    Codegen.br extCase
    -- %era case
    ------------------------------------------------------
    Codegen.setBlock eraCase
    -- nested switch cases
    eraAppCase ← Codegen.addBlock "switch.era.app"
    eraLamCase ← Codegen.addBlock "switch.era.lam"
    eraEraCase ← Codegen.addBlock "switch.era.era"
    eraDupCase ← Codegen.addBlock "switch.era.dup"
    eraExtCase ← Codegen.addBlock "switch.era.exit"
    eCdr ← undefined
    Codegen.br extCase
    -- %dup case
    ------------------------------------------------------
    Codegen.setBlock dupCase
    -- nested switch cases
    dupAppCase ← Codegen.addBlock "switch.dup.app"
    dupLamCase ← Codegen.addBlock "switch.dup.lam"
    dupEraCase ← Codegen.addBlock "switch.dup.era"
    dupDupCase ← Codegen.addBlock "switch.dup.dup"
    dupExtCase ← Codegen.addBlock "switch.dup.exit"
    dCdr ← undefined
    Codegen.br extCase
    -- %default case
    ------------------------------------------------------
    Codegen.setBlock defCase
    deCdr ← pure cdr
    Codegen.br extCase
    -- %exit case
    ------------------------------------------------------
    Codegen.setBlock extCase
    cdr ←
      Codegen.phi
        Types.eacList
        [ (deCdr, defCase),
          (dCdr, dupCase),
          (aCdr, appCase),
          (eCdr, eraCase),
          (lCdr, lamCase)
        ]
    Codegen.call Type.void reduce (Codegen.emptyArgs [cdr])
  where
    args = [(Types.eacList, "eac_list")]

--------------------------------------------------------------------------------
-- Reduction rules
--------------------------------------------------------------------------------

-- TODO ∷ modify these functions to return Types.eacLPointer type

-- TODO ∷ Maybe add metadata at some point?

-- this function work off the nodeType signature not Types.eac

-- mimic rules from the interpreter
-- This rule applies to Application ↔ Lambda
anihilateRewireAux ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Codegen.Names m,
    HasState "symtab" (Map.HashMap Symbol Operand.Operand) m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m
  ) ⇒
  m Operand.Operand
anihilateRewireAux = Codegen.defineFunction Type.void "anihilate_rewire_aux" args $
  do
    -- TODO remove these explicit allocations
    aux1 ← Codegen.auxiliary1
    aux2 ← Codegen.auxiliary2
    rewire ← Codegen.externf "rewire"
    node1 ← Codegen.externf "node_1"
    node2 ← Codegen.externf "node_2"
    _ ← Codegen.call Type.void rewire (Codegen.emptyArgs [node1, aux1, node2, aux1])
    _ ← Codegen.call Type.void rewire (Codegen.emptyArgs [node1, aux2, node2, aux2])
    _ ← Codegen.delNode node1
    Codegen.delNode node2
  where
    args = [(Codegen.nodeType, "node_1"), (Codegen.nodeType, "node_2")]

-- TODO ∷ make fast fanInAux and slow fanInAux

-- | 'fanInAuxStar' is a slower version of 'fanInAux*' where * ∈ ℤ/4ℤ.
-- This function is used when it can not be determined that 'fanInAux*'
fanInAuxStar = undefined

fanInAux0 ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Codegen.Names m,
    HasState "symtab" (Map.HashMap Symbol Operand.Operand) m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m
  ) ⇒
  m Operand.Operand →
  m Operand.Operand
fanInAux0 allocF = Codegen.defineFunction Type.void "fan_in_aux_0" args $
  do
    fanIn ← Codegen.externf "fan_in"
    node ← Codegen.externf "node"
    era1 ← allocF >>= nodeOf
    era2 ← allocF >>= nodeOf
    aux1 ← Codegen.auxiliary1
    aux2 ← Codegen.auxiliary2
    mainPort ← Codegen.mainPort
    -- abstract out this string!
    linkConnectedPort ← Codegen.externf "link_connected_port"
    _ ←
      Codegen.call
        Type.void
        linkConnectedPort
        (Codegen.emptyArgs [fanIn, aux1, era1, mainPort])
    _ ←
      Codegen.call
        Type.void
        linkConnectedPort
        (Codegen.emptyArgs [fanIn, aux2, era2, mainPort])
    Codegen.retNull
  where
    args =
      [ (Codegen.nodeType, "node"), -- we send in the alloc function for it. Do case before
        (Codegen.nodeType, "fan_in") -- we know this must be a fanIn so no need for tag
      ]

fanInAux1 allocF = undefined

-- TODO ∷ change interface for link and linkOtherNode to be more like
-- the interpreter

fanInAux2 ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Codegen.Names m,
    HasState "symtab" (Map.HashMap Symbol Operand.Operand) m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m
  ) ⇒
  m Operand.Operand →
  m Operand.Operand
fanInAux2 allocF = Codegen.defineFunction Type.void "fan_in_aux_2" args $
  do
    -- Nodes in env
    fanIn ← Codegen.externf "fan_in"
    node ← Codegen.externf "node"
    -- new nodes
    fan1 ← allocaFanIn >>= nodeOf
    fan2 ← allocaFanIn >>= nodeOf
    nod1 ← allocF >>= nodeOf
    nod2 ← allocF >>= nodeOf
    -- ports and functions
    mainPort ← Codegen.mainPort
    auxiliary1 ← Codegen.auxiliary1
    auxiliary2 ← Codegen.auxiliary2
    linkConnectedPort ← Codegen.externf "link_connected_port"
    link ← Codegen.externf "link"
    -- TODO ∷ Abstract
    _ ←
      Codegen.call
        Type.void
        linkConnectedPort
        (Codegen.emptyArgs [fanIn, auxiliary1, nod1, mainPort])
    _ ←
      Codegen.call
        Type.void
        link
        (Codegen.emptyArgs [nod1, auxiliary1, fan2, auxiliary1])
    _ ←
      Codegen.call
        Type.void
        link
        (Codegen.emptyArgs [nod1, auxiliary2, fan1, auxiliary1])
    _ ←
      Codegen.call
        Type.void
        linkConnectedPort
        (Codegen.emptyArgs [fanIn, auxiliary2, nod2, mainPort])
    _ ←
      Codegen.call
        Type.void
        link
        (Codegen.emptyArgs [nod2, auxiliary1, fan2, auxiliary2])
    _ ←
      Codegen.call
        Type.void
        link
        (Codegen.emptyArgs [nod2, auxiliary2, fan1, auxiliary2])
    _ ←
      Codegen.call
        Type.void
        linkConnectedPort
        (Codegen.emptyArgs [node, auxiliary2, fan1, mainPort])
    _ ←
      Codegen.call
        Type.void
        linkConnectedPort
        (Codegen.emptyArgs [node, auxiliary1, fan2, mainPort])
    Codegen.retNull
  where
    args =
      [ (Codegen.nodeType, "node"),
        (Codegen.nodeType, "fan_in")
      ]

fanInAux3 allocF = undefined

--------------------------------------------------------------------------------
-- Allocations
--------------------------------------------------------------------------------
allocaGen ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m
  ) ⇒
  C.Constant →
  Int →
  Int →
  m Operand.Operand
allocaGen type' portLen dataLen = do
  eac ← Codegen.alloca Types.eac
  node ← Codegen.allocaNodeH (replicate portLen Nothing) (replicate dataLen Nothing)
  tagPtr ← Codegen.getElementPtr $
    Codegen.Minimal
      { Codegen.type' = Types.tag,
        Codegen.address' = eac,
        Codegen.indincies' = Codegen.constant32List [0, 0]
      }
  Codegen.store tagPtr (Operand.ConstantOperand type')
  nodePtr ← Codegen.getElementPtr $
    Codegen.Minimal
      { Codegen.type' = Codegen.nodeType,
        Codegen.address' = eac,
        Codegen.indincies' = Codegen.constant32List [0, 1]
      }
  Codegen.store nodePtr node
  pure eac

allocaEra,
  allocaFanIn,
  allocaApp,
  allocaLam ∷
    ( HasThrow "err" Codegen.Errors m,
      HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
      HasState "count" Word m,
      HasState "currentBlock" Name.Name m,
      HasState "typTab" Codegen.TypeTable m,
      HasState "varTab" Codegen.VariantToType m
    ) ⇒
    m Operand.Operand
allocaEra = allocaGen Types.era 1 0
allocaApp = allocaGen Types.app 3 0
allocaLam = allocaGen Types.lam 3 0
allocaFanIn = allocaGen Types.dup 3 1

nodeOf ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m
  ) ⇒
  Operand.Operand →
  m Operand.Operand
nodeOf eac = do
  Codegen.loadElementPtr $
    Codegen.Minimal
      { Codegen.type' = Codegen.nodeType,
        Codegen.address' = eac,
        Codegen.indincies' = Codegen.constant32List [0, 1]
      }

tagOf ∷
  ( HasThrow "err" Codegen.Errors m,
    HasState "blocks" (Map.HashMap Name.Name Codegen.BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m
  ) ⇒
  Operand.Operand →
  m Operand.Operand
tagOf eac = do
  Codegen.loadElementPtr $
    Codegen.Minimal
      { Codegen.type' = Types.tag,
        Codegen.address' = eac,
        Codegen.indincies' = Codegen.constant32List [0, 0]
      }
