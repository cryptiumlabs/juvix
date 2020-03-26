{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- - Interface serves as a file that has common types between the various
--   back ends along with two interfaces each back-end must support
--   + This also includes functions derived from the interface functions!
-- - This file will be the file [[Nets]] will import as it provides the
--   interface along with derived functions
module Juvix.Interpreter.InteractionNet.Backends.Interface where

import Control.Lens
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Juvix.Interpreter.InteractionNet.NodeInterface as Interface
import Juvix.Library hiding (link)

type Node = Int

type NetState net m = HasState "net" net m

data PortType
  = Prim
  | Aux1
  | Aux2
  | Aux3
  | Aux4
  | Aux5
  deriving (Ord, Eq, Show, Enum)

data NumPort
  = Port PortType Node
  | FreePort
  deriving (Show)

-- Rewrite REL into tagless final, so we aren't memory
-- wasting on this silly tag, just pass in the function!

-- | REL: a type that displays whether we are linking
-- from an old node or just adding a new link
data REL a
  = Link a
  | ReLink Node PortType
  deriving (Show)

-- | Type for specifying how one wants to link nodes
data Relink
  = RELAuxiliary3
      { node ∷ Node,
        primary ∷ REL NumPort,
        auxiliary1 ∷ REL NumPort,
        auxiliary2 ∷ REL NumPort,
        auxiliary3 ∷ REL NumPort
      }
  | RELAuxiliary2
      { node ∷ Node,
        primary ∷ REL NumPort,
        auxiliary1 ∷ REL NumPort,
        auxiliary2 ∷ REL NumPort
      }
  | RELAuxiliary1 {node ∷ Node, primary ∷ REL NumPort, auxiliary1 ∷ REL NumPort}
  | RELAuxiliary0 {node ∷ Node, primary ∷ REL NumPort}
  deriving (Show)

-- | An Edge type, used to sometimes represent edges
-- The graph uses this for the edge representation
data EdgeInfo = Edge (Node, PortType) (Node, PortType)
  deriving (Show, Eq)

-- TODO ∷ Once neighbor is in, delete this type class
-- and provide these as all derived functions!

type Primary' = Interface.Primary

type Auxiliary' = Interface.Auxiliary

-- | a network that has one type for nodes but another for
-- actually doing computation on the node
class Network net ⇒ DifferentRep net where

  aux0FromGraph ∷
    (NetState (net a) m, Interface.Prim b) ⇒
    (Primary' → b) →
    Node →
    m (Maybe b)

  aux1FromGraph ∷
    (NetState (net a) m, Interface.Aux1 b) ⇒
    (Primary' → Auxiliary' → b) →
    Node →
    m (Maybe b)

  aux2FromGraph ∷
    (NetState (net a) m, Interface.Aux2 b) ⇒
    (Primary' → Auxiliary' → Auxiliary' → b) →
    Node →
    m (Maybe b)

  aux3FromGraph ∷
    (NetState (net a) m, Interface.Aux3 b) ⇒
    (Primary' → Auxiliary' → Auxiliary' → Auxiliary' → b) →
    Node →
    m (Maybe b)

  aux4FromGraph ∷
    (NetState (net a) m, Interface.Aux4 b) ⇒
    (Primary' → Auxiliary' → Auxiliary' → Auxiliary' → Auxiliary' → b) →
    Node →
    m (Maybe b)

  aux5FromGraph ∷
    (NetState (net a) m, Interface.Aux5 b) ⇒
    (Primary' → Auxiliary' → Auxiliary' → Auxiliary' → Auxiliary' → Auxiliary' → b) →
    Node →
    m (Maybe b)

  langToPort ∷ NetState (net a) m ⇒ Node → (a → m (Maybe b)) → m (Maybe b)

class Network net where

  isBothPrimary ∷ NetState (net a) m ⇒ Node → m Bool

  link ∷ NetState (net a) m ⇒ (Node, PortType) → (Node, PortType) → m ()

  newNode ∷ NetState (net a) m ⇒ a → m Node

  delNodes ∷ NetState (net a) m ⇒ [Node] → m ()

  -- TODO ∷ remove deleteRewire, add neighbors, and move auxFromGraph to here!
  -- TODO ∷ make a helper function that does most of deleteRewire,
  --        just send in neighbors
  deleteRewire ∷ NetState (net a) m ⇒ [Node] → [Node] → m ()

  deleteEdge ∷ NetState (net a) m ⇒ (Node, PortType) → (Node, PortType) → m ()

  nodes ∷ NetState (net a) m ⇒ m [Node]

  empty ∷ net a

  findEdge ∷ NetState (net a) m ⇒ (Node, PortType) → m (Maybe (Node, PortType))

  allEdges ∷ NetState (net a) m ⇒ Node → m [(PortType, Node, PortType)]

-- Derived function of network -------------------------------------------------

linkAll ∷ (Network net, NetState (net a) m) ⇒ Relink → m ()
linkAll (RELAuxiliary0 {primary, node}) =
  linkHelper primary Prim node
linkAll (RELAuxiliary1 {primary, node, auxiliary1}) =
  traverse_
    (\(t, nt) → linkHelper t nt node)
    [(primary, Prim), (auxiliary1, Aux1)]
linkAll (RELAuxiliary2 {primary, node, auxiliary1, auxiliary2}) =
  traverse_
    (\(t, nt) → linkHelper t nt node)
    [(primary, Prim), (auxiliary1, Aux1), (auxiliary2, Aux2)]
linkAll (RELAuxiliary3 {primary, node, auxiliary1, auxiliary2, auxiliary3}) =
  traverse_
    (\(t, nt) → linkHelper t nt node)
    [(primary, Prim), (auxiliary1, Aux1), (auxiliary2, Aux2), (auxiliary3, Aux3)]

linkHelper ∷
  (Network net, NetState (net a) m) ⇒ REL NumPort → PortType → Node → m ()
linkHelper rel nodeType node =
  case rel of
    Link (Port portType node1) → link (node, nodeType) (node1, portType)
    Link FreePort → pure ()
    ReLink oldNode oldPort → relink (oldNode, oldPort) (node, nodeType)

-- | rewire is used to wire two auxiliary nodes together
-- when the main nodes annihilate each other
rewire ∷
  (Network net, NetState (net a) m) ⇒ (Node, PortType) → (Node, PortType) → m ()
rewire (a, pa) (b, pb) = do
  edge ← findEdge (b, pb)
  traverse_ (relink (a, pa)) edge

-- post condition, must delete the old node passed
-- after the set of transitions are done!
relink ∷
  (Network net, NetState (net a) m) ⇒ (Node, PortType) → (Node, PortType) → m ()
relink old new = do
  findEdge old >>= \case
    Just portToRelinkTo → link new portToRelinkTo
    Nothing → pure () -- The port was really free to begin with!

-- | given a set of new nodes, and the edge neighbors of old nodes, give back all
-- pairs of nodes from nodes and neighbors that need to be wired together
findConflict ∷ Set.Set Node → [EdgeInfo] → [((Node, PortType), (Node, PortType))]
findConflict nodes neighbors = Set.toList (foldr f mempty neighbors)
  where
    f (Edge t1 t2) xs =
      case (makeMap Map.!? t2, makeMap Map.!? t1) of
        (Just m2, Just m1) → Set.insert (m2, m1) xs
        _ → xs
    makeMap = foldr f mempty neighbors
      where
        f (Edge t1@(n1, _) t2@(n2, _)) hash
          | Set.member n1 nodes = Map.insert t2 t1 hash
          | Set.member n2 nodes = Map.insert t1 t2 hash
          | otherwise = hash

-- Helper functions for DifferentRep -------------------------------------------

-- these are made so we restrict what is needed the most by auxFromGraph
convPrim ∷ Interface.Prim p ⇒ (Node, PortType) → p → p
convPrim (n, Prim) con = set Interface.prim (Interface.Primary n) con
convPrim (_, _) con = con

convAux1 ∷ Interface.Aux1 p ⇒ (Node, PortType) → p → p
convAux1 (n, Aux1) con = set Interface.aux1 (Interface.Auxiliary n) con
convAux1 a con = convPrim a con

convAux2 ∷ Interface.Aux2 p ⇒ (Node, PortType) → p → p
convAux2 (n, Aux2) con = set Interface.aux2 (Interface.Auxiliary n) con
convAux2 a con = convAux1 a con

convAux3 ∷ Interface.Aux3 p ⇒ (Node, PortType) → p → p
convAux3 (n, Aux2) con = set Interface.aux3 (Interface.Auxiliary n) con
convAux3 a con = convAux2 a con

convAux4 ∷ Interface.Aux4 p ⇒ (Node, PortType) → p → p
convAux4 (n, Aux2) con = set Interface.aux4 (Interface.Auxiliary n) con
convAux4 a con = convAux3 a con

convAux5 ∷ Interface.Aux5 p ⇒ (Node, PortType) → p → p
convAux5 (n, Aux2) con = set Interface.aux5 (Interface.Auxiliary n) con
convAux5 a con = convAux4 a con
