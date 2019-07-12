{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}

module Juvix.Interaction (module Juvix.Interaction
                         , Node
                         , delNodes
                         , nodes) where

import           Data.Graph.Inductive
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Prelude                    (error)
import           Control.Lens

import           Juvix.Library              hiding (link, reduce, empty)
import           Juvix.NodeInterface

data PortType = Prim
              | Aux1
              | Aux2
              | Aux3
              | Aux4
              | Aux5
              deriving (Ord,Eq, Show)

data NumPort = Port PortType Node
             | FreePort
             deriving Show

data EdgeInfo = Edge (Node, PortType) (Node, PortType) deriving (Show, Eq)

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

type Net a = Gr a EdgeInfo

-- | Info Stores diagnostic data on how much memory a particular graph reduction uses
data Info = Info { memoryAllocated  :: Integer
                 , sequentalSteps   :: Integer
                 , parallelSteps    :: Integer
                 , biggestGraphSize :: Integer
                 , currentGraphSize :: Integer
                 } deriving (Show)

data InfoNet a = InfoNet {net :: Net a, info :: Info} deriving (Show, Generic)

newtype EnvNetInfo b a = EnvI (State (InfoNet b) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "info" Info) via
    Rename "info" (Field "info" () (MonadState (State (InfoNet b))))
  deriving (HasState "net" (Net b)) via
    Rename "net" (Field "net" () (MonadState (State (InfoNet b))))

runInfoNet :: EnvNetInfo b a → InfoNet b → (InfoNet b)
runInfoNet (EnvI m) = execState m

-- sequentalStep ∷ MonadState StateInfo m ⇒ m ()
sequentalStep :: HasState "info" Info m ⇒ m ()
sequentalStep = modify' @"info" (\c → c {sequentalSteps = sequentalSteps c + 1})

-- incGraphSizeStep :: MonadState StateInfo m ⇒ Integer → m ()
incGraphSizeStep :: HasState "info" Info m ⇒ Integer → m ()
incGraphSizeStep n = do
  Info memAlloced seqStep parallelSteps largestGraph currGraph ← get @"info"
  let memoryAllocated | n > 0 = memAlloced + n
                      | otherwise = memAlloced
      currentGraphSize = n + currGraph
      biggestGraphSize = max currentGraphSize largestGraph
      sequentalSteps = succ seqStep
  put @"info" Info { memoryAllocated, currentGraphSize
                   , biggestGraphSize, sequentalSteps, parallelSteps }

-- Graph to more typed construction---------------------------------------------
aux0FromGraph :: (Prim b, Graph gr, HasState "net" (gr a EdgeInfo) m)
              ⇒ (Primary → b)
              → Node
              → m (Maybe b)
aux1FromGraph :: (Aux1 b, Graph gr, HasState "net" (gr a EdgeInfo) m)
              ⇒ (Primary → Auxiliary → b)
              → Node
              → m (Maybe b)
aux2FromGraph :: (Aux2 b, Graph gr, HasState "net" (gr a EdgeInfo) m)
              ⇒ (Primary → Auxiliary → Auxiliary → b)
              → Node
              → m (Maybe b)

aux3FromGraph :: (Aux3 b, Graph gr, HasState "net" (gr a EdgeInfo) m)
              ⇒ (Primary → Auxiliary → Auxiliary → Auxiliary → b)
              → Node
              → m (Maybe b)

aux4FromGraph :: (Aux4 b, Graph gr, HasState "net" (gr a EdgeInfo) m)
              ⇒ (Primary → Auxiliary → Auxiliary → Auxiliary → Auxiliary → b)
              → Node
              → m (Maybe b)

aux5FromGraph :: (Aux5 b, Graph gr, HasState "net" (gr a EdgeInfo) m)
              ⇒ (Primary → Auxiliary → Auxiliary → Auxiliary → Auxiliary → Auxiliary → b)
              → Node
              → m (Maybe b)

aux0FromGraph con = auxFromGraph convPrim (con Free)
aux1FromGraph con = auxFromGraph convAux1 (con Free FreeNode)
aux2FromGraph con = auxFromGraph convAux2 (con Free FreeNode FreeNode)
aux3FromGraph con = auxFromGraph convAux3 (con Free FreeNode FreeNode FreeNode)
aux4FromGraph con = auxFromGraph convAux4 (con Free FreeNode FreeNode FreeNode FreeNode)
aux5FromGraph con = auxFromGraph convAux5 (con Free FreeNode FreeNode FreeNode FreeNode FreeNode)

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

auxFromGraph :: (Graph gr, HasState "net" (gr a EdgeInfo) m)
             ⇒ ((Node, PortType) → b → b)
             → b
             → Node
             → m (Maybe b)
auxFromGraph conv constructor num =
  (fmap (foldr f constructor . lneighbors') . fst . match num) <$> get @"net"
  where
    f (Edge (n1, n1port) (n2, n2port), _) con
      | n1 == num = conv (n2, n1port) con
      | n2 == num = conv (n1, n2port) con
      | otherwise = con

-- extra work that could maybe be avoided by doing this else where?
isBothPrimary ∷ Graph gr ⇒ gr a EdgeInfo → Node → Bool
isBothPrimary net node =
  null
  $ filter (\ (Edge (_, p) (_, p')) → p == Prim && p' == Prim)
  $ fmap fst
  $ lneighbors net node

langToPort :: HasState "net" (Net a) m ⇒ Node → (a → m (Maybe b)) → m (Maybe b)
langToPort n f = do
  graph ← get @"net"
  case fst (match n graph) of
    Just context → f $ snd $ labNode' context
    Nothing      → pure Nothing

emptyNet :: Net a
emptyNet = empty
-- Graph manipulation ----------------------------------------------------------
runNet :: EnvNetInfo b a → Net b → InfoNet b
runNet f net = runInfoNet f (InfoNet net (Info netSize 0 0 netSize netSize))
  where
    netSize = toInteger (length (nodes net))
-- Manipulation functions ------------------------------------------------------

linkHelper :: HasState "net" (Net a) m ⇒ REL NumPort → PortType → Node → m ()
linkHelper rel nodeType node =
  case rel of
    Link (Port portType node1) → link (node, nodeType) (node1, portType)
    Link FreePort              → pure ()
    ReLink oldNode oldPort     → relink (oldNode, oldPort) (node, nodeType)

linkAll ∷ HasState "net" (Net a) m ⇒ Relink → m ()
linkAll (RELAuxiliary0 {primary, node}) =
  linkHelper primary Prim node
linkAll (RELAuxiliary1 {primary, node, auxiliary1}) =
  traverse_ (\ (t, nt) → linkHelper t nt node)
            [(primary, Prim), (auxiliary1, Aux1)]
linkAll (RELAuxiliary2 {primary, node, auxiliary1, auxiliary2}) =
  traverse_ (\ (t, nt) → linkHelper t nt node)
            [(primary, Prim), (auxiliary1, Aux1), (auxiliary2, Aux2)]

link :: HasState "net" (Net a) m ⇒ (Node, PortType) → (Node, PortType) → m ()
link (node1, port1) (node2, port2) =
  modify @"net" (insEdge (node1, node2, Edge (node1, port1) (node2, port2)))

-- post condition, must delete the old node passed after the set of transitions are done!
relink :: HasState "net" (Net a) m ⇒ (Node, PortType) → (Node, PortType) → m ()
relink (oldNode, port) new@(newNode, _newPort) = do
  net <- get @"net"
  case findEdge net oldNode port of
    Just re@(nodeToRelink, _) → put @"net" (insEdge (nodeToRelink, newNode, Edge re new) net)
    Nothing                   → pure () -- The port was really free to begin with!

-- | rewire is used to wire two auxiliary nodes together
-- when the main nodes annihilate each other
rewire ∷ HasState "net" (Net a) m ⇒ (PortType, Node) → (PortType, Node) → m ()
rewire (pa, a) (pb, b) = do
  net ← get @"net"
  traverse_ (relink (a, pa)) (findEdge net b pb)

newNode ∷ (DynGraph gr, HasState "net" (gr a b) m) ⇒ a → m Node
newNode lang = do
  net <- get @"net"
  let (_,maxNum)
        | isEmpty net = (0, 0)
        | otherwise   = nodeRange net
  put @"net" (insNode (succ maxNum, lang) net)
  pure (succ maxNum)


delNodesM :: (Graph gr, HasState "net" (gr a b) f) ⇒ [Node] → f ()
delNodesM xs = modify @"net" (delNodes xs)

deleteRewire ∷ HasState "net" (Net a) m ⇒ [Node] → [Node] → m ()
deleteRewire oldNodesToDelete newNodes = do
  net ← get @"net"
  let newNodeSet           = Set.fromList newNodes
      neighbors            = fst <$> (oldNodesToDelete >>= lneighbors net)
      conflictingNeighbors = findConflict newNodeSet neighbors
  traverse_ (uncurry link) conflictingNeighbors
  modify @"net" (delNodes oldNodesToDelete)

auxToPrimary :: Auxiliary → Primary
auxToPrimary (Auxiliary node) = Primary node
auxToPrimary FreeNode         = Free

auxToNode :: Auxiliary → Maybe Node
auxToNode (Auxiliary node) = Just node
auxToNode FreeNode         = Nothing

findConflict ∷ Set.Set Node → [EdgeInfo] → [((Node, PortType), (Node, PortType))]
findConflict nodes neighbors = Set.toList (foldr f mempty neighbors)
  where
    f (Edge t1 t2@(_,_)) xs
      | Map.member t1 makeMap && Map.member t2 makeMap =
        Set.insert ((makeMap Map.! t2), (makeMap Map.! t1)) xs
      | otherwise = xs
    makeMap = foldr f mempty neighbors
      where
        f (Edge t1@(n1,_) t2@(n2,_)) hash
          | Set.member n1 nodes = Map.insert t2 t1 hash
          | Set.member n2 nodes = Map.insert t1 t2 hash
          | otherwise           = hash

-- Precond, node must exist in the net with the respective port
findEdge ∷ Net a → Node → PortType → Maybe (Node, PortType)
findEdge net node port = fmap other $ headMay $ filter f $ lneighbors net node
  where
    f (Edge t1 t2, _)
      | t1 == (node, port) = True
      | t2 == (node, port) = True
      | otherwise          = False
    other (Edge t1 t2, _)
      | t1 == (node, port) = t2
      | t2 == (node, port) = t1
      | otherwise          = error "doesn't happen"

deleteEdge :: HasState "net" (Net a) f ⇒ (Node, PortType) → (Node, PortType) → f ()
deleteEdge t1@(n1,_) t2@(n2,_)
  = modify @"net" (delAllLEdge (n1, n2, (Edge t1 t2))
                  . delAllLEdge (n2, n1, (Edge t2 t1)))

-- Utility functions -----------------------------------------------------------
untilNothingNTimesM :: Monad f => f Bool -> Int -> f ()
untilNothingNTimesM f n
  | n <= 0  = pure ()
  | otherwise = do
      f >>= \case
        True → untilNothingNTimesM f (pred n)
        False → pure ()




untilNothing ∷ (t → Maybe t) → t → t
untilNothing f a = case f a of
  Nothing → a
  Just a  → untilNothing f a
