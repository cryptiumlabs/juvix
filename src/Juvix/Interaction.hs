{-# LANGUAGE NamedFieldPuns     #-}

module Juvix.Interaction (module Juvix.Interaction
                         , Node
                         , delNodes
                         , nodes) where

import           Control.Monad.State.Strict
import           Data.Graph.Inductive
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Prelude                    (error)
import           Protolude                  hiding (State, link, reduce,
                                                    runState)
import           Control.Lens

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

data EdgeInfo = Edge (Node, PortType) (Node, PortType) deriving Show

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

-- | StateInfo Stores diagnostic data on how much memory a particular graph reduction uses
data StateInfo = Info { memoryAllocated  :: Integer
                      , sequentalSteps   :: Integer
                      , parallelSteps    :: Integer
                      , biggestGraphSize :: Integer
                      , currentGraphSize :: Integer
                      } deriving (Show)

sequentalStep ∷ MonadState StateInfo m ⇒ m ()
sequentalStep = modify' (\c -> c {sequentalSteps = sequentalSteps c + 1})

incGraphSizeStep :: MonadState StateInfo m ⇒ Integer → m ()
incGraphSizeStep n = do
  Info memAlloced seqStep parallelSteps largestGraph currGraph <- get
  let memoryAllocated | n > 0 = memAlloced + n
                      | otherwise = memAlloced
      currentGraphSize = n + currGraph
      biggestGraphSize = max currentGraphSize largestGraph
      sequentalSteps = succ seqStep
  put Info {memoryAllocated, currentGraphSize, biggestGraphSize, sequentalSteps, parallelSteps}

-- Graph to more typed construction---------------------------------------------
aux0FromGraph :: (Prim b, Graph gr) ⇒ (Primary → b) → gr a EdgeInfo → Node → Maybe b
aux1FromGraph :: (Aux1 b, Graph gr) ⇒ (Primary → Auxiliary → b) → gr a EdgeInfo → Node → Maybe b
aux2FromGraph :: (Aux2 b, Graph gr)
              ⇒ (Primary → Auxiliary → Auxiliary → b)
              → gr a EdgeInfo
              → Node
              → Maybe b

aux3FromGraph :: (Aux3 b, Graph gr)
              ⇒ (Primary → Auxiliary → Auxiliary → Auxiliary → b)
              → gr a EdgeInfo
              → Node
              → Maybe b

aux4FromGraph :: (Aux4 b, Graph gr)
              ⇒ (Primary → Auxiliary → Auxiliary → Auxiliary → Auxiliary → b)
              → gr a EdgeInfo
              → Node
              → Maybe b

aux5FromGraph :: (Aux5 b, Graph gr)
              ⇒ (Primary → Auxiliary → Auxiliary → Auxiliary → Auxiliary → Auxiliary → b)
              → gr a EdgeInfo
              → Node
              → Maybe b

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

auxFromGraph :: Graph gr ⇒ ((Node, PortType) → b → b) → b → gr a EdgeInfo → Node → Maybe b
auxFromGraph conv constructor graph num =
  foldr f constructor . lneighbors' <$> fst (match num graph)
  where
    f (Edge (n1, n1port) (n2, n2port), _) con
      | n1 == num = conv (n2, n1port) con
      | n2 == num = conv (n1, n2port) con
      | otherwise = con

-- extra work that could maybe be avoided by doing this else where?
isBothPrimary ∷ Graph gr ⇒ gr a EdgeInfo → Node → Bool
isBothPrimary net node =
  null
  $ filter (\ (Edge (_, p) (_, p')) -> p == Prim && p' == Prim)
  $ fmap fst
  $ lneighbors net node

langToPort :: Net a → Node → (a → Maybe b) → Maybe b
langToPort graph n f = do
  context ← fst (match n graph)
  f (snd (labNode' context))
-- Graph manipulation ----------------------------------------------------------
runNet ∷ (Net a → State StateInfo (Net a)) → Net a → (Net a, StateInfo)
runNet f net = runState (f net) (Info netSize 0 0 netSize netSize)
  where
    netSize = toInteger (length (nodes net))

delMaybeNodes :: Graph gr ⇒ [Maybe Node] → gr a b → gr a b
delMaybeNodes = delNodes . catMaybes
-- Manipulation functions ------------------------------------------------------

linkHelper :: Net a → REL NumPort → PortType → Node → Net a
linkHelper net rel nodeType node =
  case rel of
    Link (Port portType node1) -> link net (node, nodeType) (node1, portType)
    Link FreePort              -> net
    ReLink oldNode oldPort     -> relink net (oldNode, oldPort) (node, nodeType)

linkAll ∷ Net a → Relink → Net a
linkAll net (RELAuxiliary0 {primary, node}) =
  linkHelper net primary Prim node
linkAll net (RELAuxiliary1 {primary, node, auxiliary1}) =
  linkHelper (linkHelper net primary Prim node) auxiliary1 Aux1 node
linkAll net (RELAuxiliary2 {primary, node, auxiliary1, auxiliary2}) =
  foldr (\ (typ,nodeType) net -> linkHelper net typ nodeType node)
        net
        [(primary, Prim), (auxiliary1, Aux1), (auxiliary2, Aux2)]


link ∷ Net a → (Node, PortType) → (Node, PortType) → Net a
link net (node1, port1) (node2, port2) =
  insEdge (node1, node2, Edge (node1, port1) (node2, port2)) net

-- post condition, must delete the old node passed after the set of transitions are done!
relink ∷ Net a → (Node, PortType) → (Node, PortType) → Net a
relink net (oldNode, port) new@(newNode, _newPort) =
  case findEdge net oldNode port of
    Just relink@(nodeToRelink, _) -> insEdge (nodeToRelink, newNode, Edge relink new) net
    Nothing                       -> net -- The port was really free to begin with!

-- | rewire is used to wire two auxiliary nodes together
-- when the main nodes annihilate each other
rewire ∷ Net a → (PortType, Auxiliary) → (PortType, Auxiliary) → Net a
rewire net (pa, (Auxiliary a)) (pb, (Auxiliary b)) = link net (a, pa) (b, pb)
rewire net _ _                                     = net

newNode ∷ DynGraph gr ⇒ gr a b → a → (Node, gr a b)
newNode graph lang = (succ maxNum, insNode (succ maxNum, lang) graph)
  where
    (_,maxNum) = nodeRange graph

deleteRewire ∷ [Node] → [Node] → Net a → Net a
deleteRewire oldNodesToDelete newNodes net = delNodes oldNodesToDelete dealWithConflict
  where
    newNodeSet           = Set.fromList newNodes
    neighbors            = fst <$> (oldNodesToDelete >>= lneighbors net)
    conflictingNeighbors = findConflict newNodeSet neighbors
    dealWithConflict     = foldr (\ (t1, t2) net -> link net t1 t2) net conflictingNeighbors

auxToPrimary :: Auxiliary → Primary
auxToPrimary (Auxiliary node) = Primary node
auxToPrimary FreeNode         = Free

auxToNode :: Auxiliary → Maybe Node
auxToNode (Auxiliary node) = Just node
auxToNode FreeNode         = Nothing

findConflict ∷ Set.Set Node → [EdgeInfo] → [((Node, PortType), (Node, PortType))]
findConflict nodes neighbors  = Set.toList (foldr f mempty neighbors)
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

-- Utility functions -----------------------------------------------------------
untilNothingNTimesM ∷ Monad m ⇒ (t → m (Maybe t)) → t → Int → m t
untilNothingNTimesM f a n
  | n <= 0  = pure a
  | otherwise = do
      v <- f a
      case v of
        Nothing -> pure a
        Just a  -> untilNothingNTimesM f a (pred n)

untilNothing ∷ (t → Maybe t) → t → t
untilNothing f a = case f a of
  Nothing -> a
  Just a  -> untilNothing f a
