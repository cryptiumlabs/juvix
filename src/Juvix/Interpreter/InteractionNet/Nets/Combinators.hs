{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- - An evaluation for a simple combination calculus language, only has
--   three ports
--   1. Con
--   2. Dup
--   3. Era
module Juvix.Interpreter.InteractionNet.Nets.Combinators where

import Control.Lens
import Data.Foldable (foldrM)
import qualified Juvix.Interpreter.InteractionNet.Backends.Env as Env
import Juvix.Interpreter.InteractionNet.Backends.Interface
import Juvix.Interpreter.InteractionNet.NodeInterface
import Juvix.Library hiding (reduce)
import Prelude (error)

-- Specific Port Type-----------------------------------------------------------

-- eventually turn this into a nice Class representation to easily extend!
data ProperPort
  = Construct {_prim ∷ Primary, _aux1 ∷ Auxiliary, _aux2 ∷ Auxiliary}
  | Duplicate {_prim ∷ Primary, _aux1 ∷ Auxiliary, _aux2 ∷ Auxiliary}
  | Erase {_prim ∷ Primary}
  deriving (Show)

makeFieldsNoPrefix ''ProperPort

-- Eventually turn this into a GADΤ with more info?
data Lang where
  Con ∷ Lang
  Dup ∷ Lang
  Era ∷ Lang

deriving instance Show Lang

-- Graph to more typed construction---------------------------------------------
-- Port manipulation
conFromGraph ∷
  (DifferentRep net, HasState "net" (net Lang) m) ⇒ Node → m (Maybe ProperPort)
conFromGraph = aux2FromGraph Construct

dupFromGraph ∷
  (DifferentRep net, HasState "net" (net Lang) m) ⇒ Node → m (Maybe ProperPort)
dupFromGraph = aux2FromGraph Duplicate

eraFromGraph ∷
  (DifferentRep net, HasState "net" (net Lang) m) ⇒ Node → m (Maybe ProperPort)
eraFromGraph = aux0FromGraph Erase

-- a bit of extra repeat work in this function!
langToProperPort ∷
  (DifferentRep net, HasState "net" (net Lang) m) ⇒ Node → m (Maybe ProperPort)
langToProperPort node = langToPort node f
  where
    f Con = conFromGraph node
    f Dup = dupFromGraph node
    f Era = eraFromGraph node

-- Graph manipulation ----------------------------------------------------------
reduceAll ∷ Env.InfoNetworkDiff net Lang m ⇒ Int → m ()
reduceAll = untilNothingNTimesM reduce

reduce ∷ Env.InfoNetworkDiff net Lang m ⇒ m Bool
reduce = do
  nodes' ← nodes
  isChanged ← foldrM update False nodes'
  if isChanged
    then do
      modify @"info" (\c → c {Env.parallelSteps = Env.parallelSteps c + 1})
      pure isChanged
    else pure isChanged
  where
    update n isChanged = do
      both ← isBothPrimary n
      if not both
        then pure isChanged
        else langToProperPort n >>= \case
          Nothing → pure isChanged
          Just port →
            -- The main port we are looking at
            case port of
              Construct Free _ _ → pure isChanged
              Duplicate Free _ _ → pure isChanged
              Erase Free → pure isChanged
              con@(Construct (Primary node) _ _) →
                True
                  <$ ( langToProperPort node >>= \case
                         Nothing →
                           error "nodes are undirected, precondition violated!"
                         Just d@Duplicate {} → conDup n node con d
                         Just Erase {} → erase n node con
                         Just c@Construct {} → annihilate n node con c
                     )
              dup@(Duplicate (Primary node) _ _) →
                True
                  <$ ( langToProperPort node >>= \case
                         Nothing →
                           error "nodes are undirected, precondition violated!"
                         Just d@Duplicate {} → annihilate n node dup d
                         Just Erase {} → erase n node dup
                         Just c@Construct {} → conDup node n c dup
                     )
              Erase (Primary node) →
                langToProperPort node >>= \case
                  Nothing → error "nodes are undirected, precondition violated!"
                  Just x → True <$ erase node n x

-- | Deals with the case when two nodes annihilate each other
annihilate ∷
  Env.InfoNetwork net Lang m ⇒
  Node →
  Node →
  ProperPort →
  ProperPort →
  m ()
annihilate conNum1 conNum2 (Construct {}) (Construct {}) = do
  Env.incGraphSizeStep (- 2)
  rewire (conNum1, Aux1) (conNum2, Aux2)
  rewire (conNum1, Aux2) (conNum2, Aux1)
  delNodes [conNum1, conNum2]
annihilate conNum1 conNum2 (Duplicate {}) (Duplicate {}) = do
  Env.incGraphSizeStep (- 2)
  rewire (conNum1, Aux1) (conNum2, Aux1)
  rewire (conNum1, Aux2) (conNum2, Aux2)
  delNodes [conNum1, conNum2]
annihilate _ _ _ _ = error "the other nodes do not annihilate eachother"

-- | Deals with the case when an Erase Node hits any other node
erase ∷ Env.InfoNetwork net Lang m ⇒ Node → Node → ProperPort → m ()
erase conNum eraseNum port =
  case port of
    Construct {} → Env.sequentalStep *> rewire
    Duplicate {} → Env.sequentalStep *> rewire
    Erase {} → Env.incGraphSizeStep (- 2) *> delNodes [conNum, eraseNum]
  where
    rewire = do
      eraA ← newNode Era
      eraB ← newNode Era
      let nodeA = RELAuxiliary0 {node = eraA, primary = ReLink conNum Aux1}
          nodeB = RELAuxiliary0 {node = eraB, primary = ReLink conNum Aux2}
      traverse_ linkAll [nodeA, nodeB]
      deleteRewire [conNum, eraseNum] [eraA, eraB]

-- | conDup deals with the case when Constructor and Duplicate share a primary
conDup ∷
  Env.InfoNetwork net Lang m ⇒
  Node →
  Node →
  ProperPort →
  ProperPort →
  m ()
conDup conNum deconNum (Construct _ _auxA _auxB) (Duplicate _ _auxC _auxD) = do
  Env.incGraphSizeStep 2
  dupA ← newNode Dup
  dupB ← newNode Dup
  conC ← newNode Con
  conD ← newNode Con
  let nodeA = RELAuxiliary2
        { node = dupA,
          primary = ReLink conNum Aux1,
          auxiliary1 = Link (Port Aux1 conD),
          auxiliary2 = Link (Port Aux1 conC)
        }

      nodeB = RELAuxiliary2
        { node = dupB,
          primary = ReLink conNum Aux2,
          auxiliary1 = Link (Port Aux2 conD),
          auxiliary2 = Link (Port Aux2 conC)
        }

      nodeC = RELAuxiliary2
        { node = conC,
          primary = ReLink deconNum Aux2,
          auxiliary1 = Link (Port Aux2 dupA),
          auxiliary2 = Link (Port Aux2 dupB)
        }

      nodeD = RELAuxiliary2
        { node = conD,
          primary = ReLink deconNum Aux1,
          auxiliary1 = Link (Port Aux1 dupA),
          auxiliary2 = Link (Port Aux1 dupB)
        }

  traverse_ linkAll [nodeA, nodeB, nodeC, nodeD]
  deleteRewire [conNum, deconNum] [dupA, dupB, conC, conD]
conDup _ _ _ _ = error "only send a construct and duplicate to conDup"
