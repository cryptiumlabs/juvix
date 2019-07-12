{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Juvix.Nets.Combinators where

import           Prelude                          (error)
import           Data.Foldable                    (foldrM)
import           Control.Lens

import           Juvix.Library                 hiding (reduce)
import           Juvix.Interaction
import           Juvix.NodeInterface

-- Specific Port Type-----------------------------------------------------------

-- eventually turn this into a nice Class representation to easily extend!
data ProperPort
  = Construct {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
  | Duplicate {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
  | Erase     {_prim :: Primary}
  deriving (Show)

makeFieldsNoPrefix ''ProperPort

-- Eventually turn this into a GADΤ with more info?
data Lang where
  Con :: Lang
  Dup :: Lang
  Era :: Lang

deriving instance Show Lang

type NetLang = Net Lang

-- Graph to more typed construction---------------------------------------------
-- Port manipulation
conFromGraph ∷ HasState "net" (NetLang) m ⇒ Node → m (Maybe ProperPort)
conFromGraph = aux2FromGraph Construct

dupFromGraph ∷ HasState "net" (NetLang) m ⇒ Node → m (Maybe ProperPort)
dupFromGraph = aux2FromGraph Duplicate

eraFromGraph ∷ HasState "net" (NetLang) m ⇒ Node → m (Maybe ProperPort)
eraFromGraph = aux0FromGraph Erase

-- a bit of extra repeat work in this function!
langToProperPort ∷ HasState "net" (NetLang) m ⇒ Node → m (Maybe ProperPort)
langToProperPort node = langToPort node f
  where
    f Con = conFromGraph node
    f Dup = dupFromGraph node
    f Era = eraFromGraph node

-- Graph manipulation ----------------------------------------------------------
reduceAll ∷ (HasState "info" Info f, HasState "net" NetLang f)
          ⇒ Int → f ()
reduceAll = untilNothingNTimesM reduce

reduce ∷ (HasState "info" Info m, HasState "net" NetLang m) ⇒ m Bool
reduce = do
  net <- get @"net"
  isChanged ← foldrM update False (nodes net)
  if isChanged then do
    modify @"info" (\c → c {parallelSteps = parallelSteps c + 1})
    pure isChanged
    else
    pure isChanged
  where
    update n isChanged = do
      net ← get @"net"
      if (isBothPrimary net n)
        then pure isChanged
        else
        langToProperPort n >>= \case
          Nothing   -> pure isChanged
          Just port ->
            -- The main port we are looking at
            case port of
              Construct Free _ _                 -> pure isChanged
              Duplicate Free _ _                 -> pure isChanged
              Erase Free                         -> pure isChanged
              con@(Construct (Primary node) _ _) ->
                True <$
                (langToProperPort node >>= \case
                  Nothing             -> error "nodes are undirected, precondition violated!"
                  Just d@Duplicate {} -> conDup     n node con d
                  Just Erase {}       -> erase      n node con
                  Just c@Construct {} -> annihilate n node con c)
              dup@(Duplicate (Primary node) _ _) ->
                True <$
                (langToProperPort node >>= \case
                  Nothing             -> error "nodes are undirected, precondition violated!"
                  Just d@Duplicate {} -> annihilate n node dup d
                  Just Erase {}       -> erase      n node dup
                  Just c@Construct {} -> conDup     node n c dup)
              Erase (Primary node) ->
                langToProperPort node >>= \case
                  Nothing -> error "nodes are undirected, precondition violated!"
                  Just x  -> True <$ erase node n x

-- | Deals with the case when two nodes annihilate each other
annihilate ∷ (HasState "info" Info m, HasState "net" NetLang m)
           ⇒ Node → Node → ProperPort → ProperPort → m ()
annihilate conNum1 conNum2 (Construct {}) (Construct {}) = do
  incGraphSizeStep (-2)
  rewire (Aux1, conNum1) (Aux2, conNum2)
  rewire (Aux2, conNum1) (Aux1, conNum2)
  delNodesM [conNum1, conNum2]

annihilate conNum1 conNum2 (Duplicate {}) (Duplicate {}) = do
  incGraphSizeStep (-2)
  rewire (Aux1, conNum1) (Aux1, conNum2)
  rewire (Aux2, conNum1) (Aux2, conNum2)
  delNodesM [conNum1, conNum2]
annihilate _ _ _ _ = error "the other nodes do not annihilate eachother"

-- | Deals with the case when an Erase Node hits any other node
erase ∷ (HasState "info" Info m, HasState "net" NetLang m)
      ⇒ Node → Node → ProperPort → m ()
erase conNum eraseNum port
  = case port of
      Construct {} → sequentalStep         *> rewire
      Duplicate {} → sequentalStep         *> rewire
      Erase {}     → incGraphSizeStep (-2) *> delNodesM [conNum, eraseNum]
  where
    rewire = do
      eraA ← newNode Era
      eraB ← newNode Era
      let nodeA = RELAuxiliary0 { node = eraA, primary = ReLink conNum Aux1 }
          nodeB = RELAuxiliary0 { node = eraB, primary = ReLink conNum Aux2 }
      traverse_ linkAll [nodeA, nodeB]
      deleteRewire [conNum, eraseNum] [eraA, eraB]

-- | conDup deals with the case when Constructor and Duplicate share a primary
conDup ∷ (HasState "info" Info m, HasState "net" NetLang m)
       ⇒ Node → Node → ProperPort → ProperPort → m ()
conDup conNum deconNum (Construct _ _auxA _auxB) (Duplicate _ _auxC _auxD) = do
  incGraphSizeStep 2
  dupA ← newNode Dup
  dupB ← newNode Dup
  conC ← newNode Con
  conD ← newNode Con
  let nodeA = RELAuxiliary2 { node       = dupA
                            , primary    = ReLink conNum Aux1
                            , auxiliary1 = Link (Port Aux1 conD)
                            , auxiliary2 = Link (Port Aux1 conC)
                            }
      nodeB = RELAuxiliary2 { node       = dupB
                            , primary    = ReLink conNum Aux2
                            , auxiliary1 = Link (Port Aux2 conD)
                            , auxiliary2 = Link (Port Aux2 conC)
                            }
      nodeC = RELAuxiliary2 { node       = conC
                            , primary    = ReLink deconNum Aux2
                            , auxiliary1 = Link (Port Aux2 dupA)
                            , auxiliary2 = Link (Port Aux2 dupB)
                            }
      nodeD = RELAuxiliary2 { node       = conD
                            , primary    = ReLink deconNum Aux1
                            , auxiliary1 = Link (Port Aux1 dupA)
                            , auxiliary2 = Link (Port Aux1 dupB)
                            }
  traverse_ linkAll [nodeA, nodeB, nodeC, nodeD]
  deleteRewire [conNum, deconNum] [dupA, dupB, conC, conD]

conDup _ _ _ _ = error "only send a construct and duplicate to conDup"
