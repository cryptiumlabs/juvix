module InteractionNet where

import qualified Data.Map.Strict as M
import           GHC.Generics
import           Protolude

type Net = M.Map Integer Node

data Kind
  = Constructor
  | Duplicator
  | Eraser
  | Loop

  deriving (Show, Eq, Generic)

data Node = Node {
  nodeKind  :: Kind,
  nodePorts :: Map Integer Port
} deriving (Show, Eq, Generic)

data Port
  = Pointer { pointerIndex :: Integer, pointerSlot :: Integer }
  | Free { freeName :: Text }

  deriving (Show, Eq, Generic)

type Rule = Net → Integer → Integer → Maybe Net

maxKey ∷ forall a b . (Num a, Ord a) ⇒ Map a b → a
maxKey m = fromMaybe 0 $ head $ reverse $ M.keys m

makeNet ∷ [(Kind, [Port])] → Net
makeNet nodes = M.fromList $ zip [0..] $ map (\(k, p) -> Node k (M.fromList $ zip [0..] p)) nodes

foldMaybe ∷ forall a b . a → [a → Maybe b] → Maybe b
foldMaybe _ []      = Nothing
foldMaybe x (f:fs)  =
  case f x of
    Just y  -> Just y
    Nothing -> foldMaybe x fs

newNode ∷ Kind → [Port] → Net → (Integer, Net)
newNode kind ports net =
  let index   = maxKey net + 1
      node    = Node kind (M.fromList $ zip [0..] ports)
      newNet  = M.insert index node net
  in (index, newNet)

removeNode ∷ Integer → Net → Net
removeNode = M.delete

removeNodes ∷ [Integer] → Net → Net
removeNodes nodes net = foldl (flip removeNode) net nodes

setPorts ∷ Integer → [Port] → Net → Net
setPorts index ports net =
  let node = net M.! index in
  M.insert index (node { nodePorts = M.fromList $ zip [0..] ports }) net

relink ∷ Node → Integer → Integer → Integer → Net → Net
relink node oldPort newNode newPort net =
  let port = nodePorts node M.! oldPort in
  case port of
    Pointer index slot ->
      let dest    = net M.! index
          updated = dest { nodePorts = M.insert slot (Pointer newNode newPort) $ nodePorts dest }
      in M.insert index updated net
    _ -> net

commute1 ∷ Rule
commute1 net iA iB = Nothing

commute2 ∷ Rule
commute2 net iA iB = do
  let nA = net M.! iA
      nB = net M.! iB
  case (nodeKind nA, nodeKind nB) of
    (Constructor, Eraser) -> return $ flip execState net $ do
      niA <- state (newNode Eraser [nodePorts nA M.! 1])
      niB <- state (newNode Eraser [nodePorts nA M.! 2])
      modify (relink nA 1 niA 0)
      modify (relink nA 2 niB 0)
      modify (removeNode iA)
      modify (removeNode iB)
    _ -> Nothing

commute3 ∷ Rule
commute3 net iA iB = do
  let nA = net M.! iA
      nB = net M.! iB
  case (nodeKind nA, nodeKind nB) of
    (Duplicator, Eraser) -> return $ flip execState net $ do
      niA <- state (newNode Eraser [nodePorts nA M.! 1])
      niB <- state (newNode Eraser [nodePorts nA M.! 2])
      modify (relink nA 1 niA 0)
      modify (relink nA 2 niB 0)
      modify (removeNode iA)
      modify (removeNode iB)
    _ -> Nothing

annihilate1 ∷ Rule
annihilate1 net iA iB = Nothing

annihilate2 ∷ Rule
annihilate2 net iA iB = Nothing

annihilate3 ∷ Rule
annihilate3 net iA iB = do
  let nA = net M.! iA
      nB = net M.! iB
  case (nodeKind nA, nodeKind nB) of
    (Eraser, Eraser) -> return (removeNodes [iA, iB] net)
    _                -> mempty

rules ∷ [Rule]
rules = [commute1, commute2, commute3, annihilate1, annihilate2, annihilate3]

maybeReduce ∷ Net → Maybe Net
maybeReduce net = do
  let keys = M.keys net
  foldMaybe net $ flip map keys $ \k -> \net -> do
    let node = net M.! k
    case M.lookup 0 $ nodePorts node of
      Just (Pointer index _) -> foldMaybe net $ map (\r -> \net -> r net k index) rules
      _ -> Nothing

fullReduce ∷ Net → (Net, Integer)
fullReduce net =
  let go net count =
        case maybeReduce net of
          Just net' -> go net' (count + 1)
          Nothing   -> (net, count)
  in go net 0

trivialCommute1 ∷ Net
trivialCommute1 = makeNet [
  (Constructor, [Pointer 1 0, Free "a", Free "b"]),
  (Duplicator, [Pointer 0 0, Free "c", Free "d"])
  ]

trivialCommute2 ∷ Net
trivialCommute2 = makeNet [
  (Constructor, [Pointer 1 0, Free "a", Free "b"]),
  (Eraser, [Pointer 0 0])
  ]

trivialCommute3 ∷ Net
trivialCommute3 = makeNet [
  (Duplicator, [Pointer 1 0, Free "a", Free "b"]),
  (Eraser, [Pointer 0 0])
  ]

trivialAnnihilate1 ∷ Net
trivialAnnihilate1 = makeNet [
  (Constructor, [Pointer 1 0, Free "a", Free "b"]),
  (Constructor, [Pointer 0 0, Free "c", Free "d"])
  ]

trivialAnnihilate2 ∷ Net
trivialAnnihilate2 = makeNet [
  (Duplicator, [Pointer 1 0, Free "a", Free "b"]),
  (Duplicator, [Pointer 0 0, Free "c", Free "d"])
  ]

trivialAnnihilate3 ∷ Net
trivialAnnihilate3 = makeNet [
  (Eraser, [Pointer 1 0]),
  (Eraser, [Pointer 0 0])
  ]

nonterminating ∷ Net
nonterminating = makeNet [
  (Constructor, [Pointer 1 0, Pointer 1 2, Pointer 2 0]),
  (Duplicator, [Pointer 0 0, Pointer 3 0, Pointer 0 1]),
  (Eraser, [Pointer 0 2]),
  (Eraser, [Pointer 1 1])
  ]
