{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}

module Juvix.Backends.Interface where

import Juvix.Library hiding (link)
import Juvix.NodeInterface

import           Control.Lens

type Node = Int

type NetState net m = HasState "net" net m

data PortType = Prim
              | Aux1
              | Aux2
              | Aux3
              | Aux4
              | Aux5
              deriving (Ord,Eq, Show, Enum)

data NumPort = Port PortType Node
             | FreePort
             deriving Show

-- Rewrite REL into tagless final, so we aren't memory
-- wasting on this silly tag, just pass in the function!
-- | REL: a type that displays whether we are linking from an old node or just adding a new link
data REL a = Link a
           | ReLink Node PortType
           deriving (Show)

-- | Type for specifying how one wants to link nodes
data Relink
  = RELAuxiliary2 { node       :: Node
                  , primary    :: REL NumPort
                  , auxiliary1 :: REL NumPort
                  , auxiliary2 :: REL NumPort }
  | RELAuxiliary1 { node :: Node, primary :: REL NumPort, auxiliary1 :: REL NumPort }
  | RELAuxiliary0 { node :: Node, primary :: REL NumPort }
  deriving (Show)

-- | a network that has one type for nodes but another for
-- actually doing computation on the node
class Network net ⇒ DifferentRep net where
  aux0FromGraph :: (NetState (net a) m, Prim b)
                ⇒ (Primary → b)
                → Node
                → m (Maybe b)
  aux1FromGraph :: (NetState (net a) m, Aux1 b)
                ⇒ (Primary → Auxiliary → b)
                → Node
                → m (Maybe b)
  aux2FromGraph :: (NetState (net a) m, Aux2 b)
                ⇒ (Primary → Auxiliary → Auxiliary → b)
                → Node
                → m (Maybe b)
  aux3FromGraph :: (NetState (net a) m, Aux3 b)
                ⇒ (Primary → Auxiliary → Auxiliary → Auxiliary → b)
                → Node
                → m (Maybe b)
  aux4FromGraph :: (NetState (net a) m, Aux4 b)
                ⇒ (Primary → Auxiliary → Auxiliary → Auxiliary → Auxiliary → b)
                → Node
                → m (Maybe b)
  aux5FromGraph :: (NetState (net a) m, Aux5 b)
                ⇒ (Primary → Auxiliary → Auxiliary → Auxiliary → Auxiliary → Auxiliary → b)
                → Node
                → m (Maybe b)
  langToPort :: NetState (net a) m ⇒ Node → (a → m (Maybe b)) → m (Maybe b)


class Network net where
  isBothPrimary :: NetState (net a) m ⇒ Node → m Bool
  link          :: NetState (net a) m ⇒ (Node, PortType) → (Node, PortType) → m ()
  newNode       :: NetState (net a) m ⇒ a → m Node
  delNodes      :: NetState (net a) m ⇒ [Node] → m ()
  deleteRewire  :: NetState (net a) m ⇒ [Node] → [Node] → m ()
  deleteEdge    :: NetState (net a) m ⇒ (Node, PortType) → (Node, PortType) → m ()
  nodes         :: NetState (net a) m ⇒ m [Node]
  empty             :: net a
  findEdge      ∷ NetState (net a) m ⇒ (Node, PortType) → m (Maybe (Node, PortType))

-- Derived function of network -------------------------------------------------

linkAll :: (Network net, NetState (net a) m) ⇒ Relink → m ()
linkAll (RELAuxiliary0 {primary, node}) =
  linkHelper primary Prim node
linkAll (RELAuxiliary1 {primary, node, auxiliary1}) =
  traverse_ (\ (t, nt) → linkHelper t nt node)
            [(primary, Prim), (auxiliary1, Aux1)]
linkAll (RELAuxiliary2 {primary, node, auxiliary1, auxiliary2}) =
  traverse_ (\ (t, nt) → linkHelper t nt node)
            [(primary, Prim), (auxiliary1, Aux1), (auxiliary2, Aux2)]

linkHelper :: (Network net, NetState (net a) m) ⇒ REL NumPort → PortType → Node → m ()
linkHelper rel nodeType node =
  case rel of
    Link (Port portType node1) → link (node, nodeType) (node1, portType)
    Link FreePort              → pure ()
    ReLink oldNode oldPort     → relink (oldNode, oldPort) (node, nodeType)

-- | rewire is used to wire two auxiliary nodes together
-- when the main nodes annihilate each other
rewire :: (Network net, NetState (net a) m) ⇒ (Node, PortType) -> (Node, PortType) -> m ()
rewire (a, pa) (b, pb) = do
  edge ← findEdge (b, pb)
  traverse_ (relink (a, pa)) edge

-- post condition, must delete the old node passed after the set of transitions are done!
relink :: (Network net, NetState (net a) m) ⇒ (Node, PortType) → (Node, PortType) → m ()
relink (oldNode, port) new = do
  findEdge (oldNode, port) >>= \case
    Just portToRelinkTo → link new portToRelinkTo
    Nothing             → pure () -- The port was really free to begin with!
-- Helper functions for DifferentRep -------------------------------------------

-- these are made so we restrict what is needed the most by auxFromGraph
convPrim :: Prim p ⇒ (Node, PortType) → p → p
convPrim (n,Prim) con = set prim (Primary n) con
convPrim (_,_) con    = con

convAux1 :: Aux1 p ⇒ (Node, PortType) → p → p
convAux1 (n,Aux1) con = set aux1 (Auxiliary n) con
convAux1 a con        = convPrim a con

convAux2 :: Aux2 p ⇒ (Node, PortType) → p → p
convAux2 (n,Aux2) con = set aux2 (Auxiliary n) con
convAux2 a con        = convAux1 a con

convAux3 :: Aux3 p ⇒ (Node, PortType) → p → p
convAux3 (n,Aux2) con = set aux3 (Auxiliary n) con
convAux3 a con        = convAux2 a con

convAux4 :: Aux4 p ⇒ (Node, PortType) → p → p
convAux4 (n,Aux2) con = set aux4 (Auxiliary n) con
convAux4 a con        = convAux3 a con

convAux5 :: Aux5 p ⇒ (Node, PortType) → p → p
convAux5 (n,Aux2) con = set aux5 (Auxiliary n) con
convAux5 a con        = convAux4 a con
