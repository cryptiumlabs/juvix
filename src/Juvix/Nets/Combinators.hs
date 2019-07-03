{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Juvix.Nets.Combinators where

import           Control.Monad.State.Strict
import           Protolude                 hiding (reduce)
import           Prelude                          (error)
import           Data.Foldable                    (foldrM)
import           Control.Lens

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
conFromGraph ∷ NetLang → Node → Maybe ProperPort
conFromGraph = aux2FromGraph Construct

dupFromGraph ∷ NetLang → Node → Maybe ProperPort
dupFromGraph = aux2FromGraph Duplicate

eraFromGraph ∷ NetLang → Node → Maybe ProperPort
eraFromGraph = aux0FromGraph Erase

-- a bit of extra repeat work in this function!
langToProperPort ∷ NetLang → Node → Maybe ProperPort
langToProperPort net node = langToPort net node f
  where
    f Con = conFromGraph net node
    f Dup = dupFromGraph net node
    f Era = eraFromGraph net node

-- Graph manipulation ----------------------------------------------------------
reduceAll ∷ MonadState StateInfo m ⇒ Int → NetLang → m NetLang
reduceAll = flip (untilNothingNTimesM reduce)

reduce ∷ MonadState StateInfo m ⇒ NetLang → m (Maybe NetLang)
reduce net = do
  (newNetLang, isChanged) ← foldrM update (net,False) netNodes
  if isChanged then do
    modify' (\c → c {parallelSteps = parallelSteps c + 1})
    return (Just newNetLang)
    else
    return Nothing
  where
    netNodes = nodes net
    updated c = (c, True)
    update n (net, isChanged)
      | isBothPrimary net n = return (net, isChanged)
      | otherwise =
        case langToProperPort net n of
          Nothing   -> pure (net, isChanged)
          Just port ->
            -- The main port we are looking at
            case port of
              Construct Free _ _                 -> pure (net, isChanged)
              Duplicate Free _ _                 -> pure (net, isChanged)
              Erase Free                         -> pure (net, isChanged)
              con@(Construct (Primary node) _ _) ->
                updated <$>
                case langToProperPort net node of
                  Nothing             -> error "nodes are undirected, precondition violated!"
                  Just d@Duplicate {} -> conDup     net n node con d
                  Just Erase {}       -> erase      net n node con
                  Just c@Construct {} -> annihilate net n node con c
              dup@(Duplicate (Primary node) _ _) ->
                updated <$>
                case langToProperPort net node of
                  Nothing             -> error "nodes are undirected, precondition violated!"
                  Just d@Duplicate {} -> annihilate net n node dup d
                  Just Erase {}       -> erase      net n node dup
                  Just c@Construct {} -> conDup     net node n c dup
              Erase (Primary node) ->
                case langToProperPort net node of
                  Nothing -> error "nodes are undirected, precondition violated!"
                  Just x  -> updated <$> erase net node n x

-- | Deals with the case when two nodes annihilate each other
annihilate ∷ MonadState StateInfo m ⇒ NetLang → Node → Node → ProperPort → ProperPort → m NetLang
annihilate net conNum1 conNum2 (Construct _ auxA auxB) (Construct _ auxC auxD) = do
  incGraphSizeStep (-2)
  return $ delNodes [conNum1, conNum2]
         $ rewire (rewire net (Aux1, auxA) (Aux2, auxD)) (Aux2, auxB) (Aux1, auxC)

annihilate net conNum1 conNum2 (Duplicate _ auxA auxB) (Duplicate _ auxC auxD) = do
  incGraphSizeStep (-2)
  return $ delNodes [conNum1, conNum2]
         $ rewire (rewire net (Aux1, auxA) (Aux1, auxC)) (Aux2, auxB) (Aux2, auxD)
annihilate _ _ _ _ _ = error "the other nodes do not annihilate eachother"

-- | Deals with the case when an Erase Node hits any other node
erase ∷ MonadState StateInfo m ⇒ NetLang → Node → Node → ProperPort → m NetLang
erase net conNum eraseNum port
  = case port of
      (Construct {}) -> rewire <$ sequentalStep
      (Duplicate {}) -> rewire <$ sequentalStep
      (Erase {})     -> delNodes [conNum, eraseNum] net <$ incGraphSizeStep (-2)
  where
    rewire = deleteRewire [conNum, eraseNum]
                          [eraA, eraB]
                          (foldr (flip linkAll) net'' [nodeA, nodeB])
    (eraA, net')  = newNode net Era
    (eraB, net'') = newNode net' Era
    nodeA         = RELAuxiliary0 { node = eraA, primary = ReLink conNum Aux1 }
    nodeB         = RELAuxiliary0 { node = eraB, primary = ReLink conNum Aux2 }

-- | conDup deals with the case when Constructor and Duplicate share a primary
conDup ∷ MonadState StateInfo m ⇒ NetLang → Node → Node → ProperPort → ProperPort → m NetLang
conDup net conNum deconNum (Construct _ _auxA _auxB) (Duplicate _ _auxC _auxD)
  = do
  incGraphSizeStep 2
  return $ deleteRewire [conNum, deconNum] [dupA, dupB, conC, conD]
         $ foldr (flip linkAll)
                 net''''
                 [nodeA, nodeB, nodeC, nodeD]
  where
    (dupA, net')    = newNode net    Dup
    (dupB, net'')   = newNode net'   Dup
    (conC, net''')  = newNode net''  Con
    (conD, net'''') = newNode net''' Con
    nodeA = RELAuxiliary2 { node       = dupA
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
conDup _ _ _ _ _ = error "only send a construct and duplicate to conDup"
