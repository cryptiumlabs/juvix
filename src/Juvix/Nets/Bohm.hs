{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Juvix.Nets.Bohm where

import           Control.Lens
import           Prelude (Show(..), error)

import           Juvix.Library hiding (link, reduce)
import           Juvix.Interaction
import           Juvix.NodeInterface

data Lang
 = Or'
 | And'
 | Not'
 | Eq'
 | Neq'
 | More'
 | Less'
 | Meq'
 | Leq'
 | Cons'
 | Car'
 | Cdr'
 | Nil'
 | TestNil'
 | IfElse'
 | Mu'
 | Div'
 | Sub'
 | Add'
 | Prod'
 | Mod'
 | Tru'
 | Fals'
 | IntLit' Int
 | Lambda'
 | Symbol' SomeSymbol
 | App'
 | FanIn' Int
 | Curried' (Int → Int)
 | CurriedB' (Int → Bool)
 | Erase'
 deriving Show


data ProperPort
 = Or      {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | And     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Not     {_prim :: Primary, _aux1 :: Auxiliary}
 | Eq      {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Neq     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | More    {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Less    {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Meq     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Leq     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Cons    {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Car     {_prim :: Primary, _aux1 :: Auxiliary}
 | Cdr     {_prim :: Primary, _aux1 :: Auxiliary}
 | Nil     {_prim :: Primary}
 | TestNil {_prim :: Primary, _aux1 :: Auxiliary}
 | IfElse  {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary, _aux3 :: Auxiliary}
 | Mu      {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Div     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Sub     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Add     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Prod    {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Mod     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Tru     {_prim :: Primary}
 | Fals    {_prim :: Primary}
 | IntLit  {_prim :: Primary, _int :: Int}
 | Lambda  {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | Symbol  {_prim :: Primary, _symb :: SomeSymbol}
 | App     {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | FanIn   {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary, _lab :: Int}
   -- Lang could be a tighter bound, but that would require more boiler plate
 | Curried  {_prim :: Primary, _aux1 :: Auxiliary, _curried :: (Int → Int)}
 | CurriedB {_prim :: Primary, _aux1 :: Auxiliary, _curriedB :: (Int → Bool)}
 | Erase   {_prim :: Primary}
 deriving Show

instance Show ((->) a b) where
  show _ = "fun"

makeFieldsNoPrefix ''ProperPort

-- Graph to more typed construction---------------------------------------------

-- Find a way to fix the ugliness!
langToProperPort :: HasState "net" (Net Lang) m ⇒ Node → m (Maybe ProperPort)
langToProperPort node = langToPort node (\l -> f l node)
  where
    f Or'      = aux2FromGraph Or
    f And'     = aux2FromGraph And
    f Not'     = aux1FromGraph Not
    f Meq'     = aux2FromGraph Meq
    f Eq'      = aux2FromGraph Eq
    f Neq'     = aux2FromGraph Neq
    f More'    = aux2FromGraph More
    f Less'    = aux2FromGraph Less
    f Cons'    = aux2FromGraph Cons
    f Car'     = aux1FromGraph Car
    f Cdr'     = aux1FromGraph Cdr
    f Nil'     = aux0FromGraph Nil
    f TestNil' = aux1FromGraph TestNil
    f IfElse'  = aux3FromGraph IfElse
    f Mu'      = aux2FromGraph Mu
    f Div'     = aux2FromGraph Div
    f Sub'     = aux2FromGraph Sub
    f Add'     = aux2FromGraph Add
    f Prod'    = aux2FromGraph Prod
    f Mod'     = aux2FromGraph Mod
    f Tru'     = aux0FromGraph Tru
    f Fals'    = aux0FromGraph Fals
    f Leq'     = aux2FromGraph Leq
    f Lambda'  = aux2FromGraph Lambda
    f App'     = aux2FromGraph App
    f Erase'   = aux0FromGraph Erase
    f (FanIn' i)    = aux2FromGraph (\p a1 a2 → FanIn p a1 a2 i)
    f (IntLit' i)   = aux0FromGraph (\x -> IntLit x i)
    f (Symbol' s)   = aux0FromGraph (\x -> Symbol x s)
    f (Curried' c)  = aux1FromGraph (\x y -> Curried x y c)
    f (CurriedB' c) = aux1FromGraph (\x y -> CurriedB x y c)
-- Rewrite rules----------------------------------------------------------------
reduceAll :: (HasState "info" Info f, HasState "net" (Net Lang) f)
          ⇒ Int → f ()
reduceAll = untilNothingNTimesM reduce

reduce :: (HasState "info" Info m, HasState "net" (Net Lang) m) ⇒ m Bool
reduce = do
  net       ← get @"net"
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
          Nothing → pure isChanged
          Just port →
            case port of
              And (Primary node) _ _ → do
                langToProperPort node >>= \case
                  Just Fals {} → True <$ propPrimary (n, port) node
                  Just Tru  {} → True <$ anihilateRewireAux node n
                  _            → pure isChanged
              Or (Primary node) _ _ →
                langToProperPort node >>= \case
                  Just Fals {} → True <$ anihilateRewireAux node n
                  Just Tru  {} → True <$ propPrimary (n, port) node
                  _            → pure isChanged
              Not (Primary node) _ →
                langToProperPort node >>= \case
                  Nothing      → pure isChanged
                  Just x       → notExpand (node, x) (n, port) isChanged
              IfElse (Primary node) _ _ _ →
                langToProperPort node >>= \case
                  Just Fals {} → True <$ ifElseRule node n False
                  Just Tru  {} → True <$ ifElseRule node n True
                  _            → pure isChanged
              Lambda (Primary node) _ _ →
                langToProperPort node >>= \case
                  Just App {}       → True <$ anihilateRewireAuxTogether n node
                  Just FanIn {_lab} → True <$ fanInAux2 node (n, Lambda') _lab
                  _                 → pure isChanged
              App (Primary node) _ _ →
                langToProperPort node >>= \case
                  Just App {}       → True <$ anihilateRewireAuxTogether n node
                  Just FanIn {_lab} → True <$ fanInAux2 node (n, App') _lab
                  _                 → pure isChanged
              Cdr (Primary node) _ →
                langToProperPort node >>= \case
                  Just Nil {} → True <$ propPrimary (n, port) node
                  _           → pure isChanged
              Nil (Primary node) →
                langToProperPort node >>= \case
                  Just c@Cdr {} → True <$ propPrimary (node, c) n
                  _             → pure isChanged
              Cons (Primary node) _ _ →
                langToProperPort node >>= \case
                  Just Cdr {} → True <$ consCdr n node
                  Just Car {} → True <$ consCar n node
                  _           → pure isChanged
              FanIn (Primary node) _ _ level →
                langToProperPort node >>= \case
                  Just App     {} → True <$ fanInAux2 n (node, App')    level
                  Just Lambda  {} → True <$ fanInAux2 n (node, Lambda') level
                  Just f@FanIn {} → True <$ fanIns (n, port) (node, f)
                  -- fan in should interact with all!, update later
                  _              → pure isChanged
              Erase (Primary node) →
                langToProperPort node >>= \case
                  Just x  → True <$ eraseAll (x, node) n
                  Nothing → pure isChanged
              Eq   (Primary node) _ _ → curryOnIntB ((==), n) node isChanged
              More (Primary node) _ _ → curryOnIntB ((>) , n) node isChanged
              Less (Primary node) _ _ → curryOnIntB ((<) , n) node isChanged
              Meq  (Primary node) _ _ → curryOnIntB ((>=), n) node isChanged
              Leq  (Primary node) _ _ → curryOnIntB ((<=), n) node isChanged
              Div  (Primary node) _ _ → curryOnInt  (div , n) node isChanged
              Sub  (Primary node) _ _ → curryOnInt  ((-) , n) node isChanged
              Prod (Primary node) _ _ → curryOnInt  ((*) , n) node isChanged
              Mod  (Primary node) _ _ → curryOnInt  (mod , n) node isChanged
              _ → pure isChanged

curryOnInt :: (HasState "info" Info f, HasState "net" (Net Lang) f)
           ⇒ (Int → Int → Int, Node) → Node → Bool → f Bool
curryOnInt (x, n) node isChanged = langToProperPort node >>=
  \case
    Just i@IntLit {} → True <$ curryRule (x, n) (i, node)
    _                → pure isChanged

curryOnIntB :: (HasState "info" Info f, HasState "net" (Net Lang) f)
            ⇒ (Int → Int → Bool, Node) → Node → Bool → f Bool
curryOnIntB (x, n) node isChanged = langToProperPort node >>=
  \case
    Just i@IntLit {} → True <$ curryRuleB (x, n) (i, node)
    _                → pure isChanged

propPrimary :: (Aux2 s, HasState "info" Info m, HasState "net" (Net Lang) m)
            ⇒ (Node, s) → Node → m ()
propPrimary (numDel, nodeDel) numProp = do
  relink (numDel, Aux1) (numProp, Prim)
  case auxToNode (nodeDel^.aux2) of
    Just _ -> do
      sequentalStep
      eraseNum ← newNode Erase'
      relink (numDel, Aux2) (eraseNum, Prim)
      deleteRewire [numDel] [eraseNum]
    Nothing -> do
      incGraphSizeStep (-1)
      delNodesM [numDel]

ifElseRule :: (HasState "info" Info m, HasState "net" (Net Lang) m)
           ⇒ Node → Node → Bool → m ()
ifElseRule numPrimOnly numAuxs pred = do
  incGraphSizeStep (-1)
  numErase ← newNode Erase'
  if pred
    then do
    relink (numAuxs, Aux2) (numErase, Prim)
    rewire (Aux1, numAuxs) (Aux3, numAuxs)
    else do
    relink (numAuxs, Aux3) (numErase, Prim)
    rewire (Aux1, numAuxs) (Aux2, numAuxs)
  deleteRewire [numPrimOnly, numAuxs] [numErase]


anihilateRewireAux :: (HasState "info" Info m, HasState "net" (Net a) m)
                   ⇒ Node → Node → m ()
anihilateRewireAux numPrimOnly numAuxs = do
  incGraphSizeStep (-2)
  rewire (Aux1, numAuxs) (Aux2, numAuxs)
  delNodesM [numPrimOnly, numAuxs]

-- used for app lambda!
anihilateRewireAuxTogether :: (HasState "info" Info m, HasState "net" (Net Lang) m)
                           ⇒ Node → Node → m ()
anihilateRewireAuxTogether numNode1 numNode2 = do
  incGraphSizeStep (-2)
  rewire (Aux1, numNode1) (Aux1, numNode2)
  rewire (Aux2, numNode1) (Aux2, numNode2)
  delNodesM [numNode1, numNode2]

-- o is Aux1 * is Aux2
muExpand :: (HasState "info" Info m, HasState "net" (Net Lang) m)
         ⇒ Node → m ()
muExpand muNum = do
  incGraphSizeStep 2
  fanIn  ← newNode (FanIn' 0)
  fanOut ← newNode (FanIn' 0)
  newMu  ← newNode (FanIn' 0)
  let nodeFanIn = RELAuxiliary2 { node       = fanIn
                                , primary    = ReLink muNum Aux1
                                , auxiliary1 = Link (Port Aux1 newMu)
                                , auxiliary2 = ReLink muNum Prim
                                }
      nodeFanOut = RELAuxiliary2 { node       = fanOut
                                 , primary    = ReLink muNum Aux2
                                 , auxiliary1 = Link (Port Aux2 newMu)
                                 , auxiliary2 = Link (Port Prim newMu)
                                 }
  traverse_ linkAll [nodeFanIn, nodeFanOut]
  deleteRewire [muNum] [fanIn, fanOut, newMu]

fanInAux2 :: (HasState "info" Info m, HasState "net" (Net Lang) m)
          ⇒ Node → (Node, Lang) → Int → m ()
fanInAux2 numFan (numOther, otherLang) level = do
  incGraphSizeStep 2
  other1 ← newNode otherLang
  other2 ← newNode otherLang
  fanIn1 ← newNode (FanIn' level)
  fanIn2 ← newNode (FanIn' level)
  let nodeOther1 = RELAuxiliary2 { node       = other1
                                 , primary    = ReLink numFan Aux1
                                 , auxiliary1 = Link (Port Aux1 fanIn2)
                                 , auxiliary2 = Link (Port Aux1 fanIn1)
                                 }
      nodeOther2 = RELAuxiliary2 { node       = other2
                                 , primary    = ReLink numFan Aux2
                                 , auxiliary1 = Link (Port Aux2 fanIn2)
                                 , auxiliary2 = Link (Port Aux2 fanIn1)
                                 }
      nodeFan1   = RELAuxiliary2 { node       = fanIn1
                                 , primary    = ReLink numOther Aux2
                                 , auxiliary1 = Link (Port Aux2 other1)
                                 , auxiliary2 = Link (Port Aux2 other2)
                                 }
      nodeFan2   = RELAuxiliary2 { node       = fanIn2
                                 , primary    = ReLink numOther Aux1
                                 , auxiliary1 = Link (Port Aux1 other1)
                                 , auxiliary2 = Link (Port Aux1 other2)
                                 }
  traverse_ linkAll [nodeOther1, nodeOther2, nodeFan1, nodeFan2]
  deleteRewire [numFan, numOther] [other1, other2, fanIn1, fanIn2]

-- TODO :: delete node coming in!
notExpand :: (Aux2 s, HasState "info" Info f, HasState "net" (Net Lang) f)
          ⇒ (Node, ProperPort)
          → (Node, s)
          → Bool
          → f Bool
notExpand (n, Tru {}) (notNum, notPort) _ = do
  numFals ← newNode Fals'
  delNodesM [n]
  propPrimary (notNum, notPort) numFals
  pure True

notExpand (n, Fals {}) (notNum, notPort) _ = do
  numFals ← newNode Tru'
  delNodesM [n]
  propPrimary (notNum, notPort) numFals
  pure True

notExpand _ _ updated = pure updated


-- Erase should be connected to the main port atm, change this logic later
-- when this isn't the case
eraseAll :: (Aux3 s, HasState "info" Info m, HasState "net" (Net Lang) m)
         ⇒ (s, Node) → Node → m ()
eraseAll (node, numNode) nodeErase = do
  (i,  mE)  ← auxDispatch (node^.aux1) Aux1
  (i2, mE2) ← auxDispatch (node^.aux2) Aux2
  (i3, mE3) ← auxDispatch (node^.aux3) Aux3
  incGraphSizeStep (i + i2 + i3 - 2)
  deleteRewire [numNode, nodeErase] (catMaybes [mE, mE2, mE3])
  where
    auxDispatch FreeNode      _   = pure (0, Nothing)
    auxDispatch (Auxiliary _) aux = (,) 1 <$> erase aux
    erase port = do
      numE ← newNode Erase'
      relink (numNode, port) (numE, Prim)
      return (Just numE)

consCar :: (HasState "info" Info m, HasState "net" (Net Lang) m)
        ⇒ Node → Node → m ()
consCar numCons numCar = do
  incGraphSizeStep (-1)
  erase ← newNode Erase'
  relink (numCons, Aux1) (erase, Prim)
  rewire (Aux2, numCons) (Aux1, numCar)
  deleteRewire [numCons, numCar] [erase]

consCdr :: (HasState "info" Info m, HasState "net" (Net Lang) m)
        ⇒ Node → Node → m ()
consCdr numCons numCdr = do
  incGraphSizeStep (-1)
  erase ← newNode Erase'
  relink (numCons, Aux2) (erase, Prim)
  rewire (Aux1, numCons) (Aux1, numCdr)
  deleteRewire [numCons, numCdr] [erase]

testNilNil :: (HasState "info" Info m, HasState "net" (Net Lang) m)
           ⇒ Node → Node → m ()
testNilNil numTest numNil = do
  incGraphSizeStep (-1)
  true ← newNode Tru'
  relink (numTest, Aux1) (true, Prim)
  deleteRewire [numTest, numNil] [true]

testNilCons :: (HasState "info" Info m, HasState "net" (Net Lang) m)
            ⇒ Node → Node → m ()
testNilCons numCons numTest = do
  incGraphSizeStep 1
  erase1 ← newNode Erase'
  erase2 ← newNode Erase'
  false  ← newNode Fals'
  traverse_ (uncurry relink)
            [((numCons, Aux1), (erase1, Prim))
            ,((numCons, Aux2), (erase2, Prim))
            ,((numTest, Aux1), (false , Prim))]
  deleteRewire [numCons, numTest] [erase1, erase2, false]


curryRuleGen :: (HasState "info" Info m, HasState "net" (Net a) m)
             ⇒ (t → a)
             → (Int → t, Node)
             → (ProperPort, Node)
             → m ()
curryRuleGen con (nodeF, numNode) ((IntLit _ i), numInt) = do
  incGraphSizeStep (-1)
  curr         ← newNode (con (nodeF i))
  let currNode = RELAuxiliary1 { node       = curr
                               , primary    = ReLink numNode Aux2
                               , auxiliary1 = ReLink numNode Aux1
                               }
  linkAll currNode
  deleteRewire [numNode, numInt] [curr]

curryRuleGen _ _ _ = pure ()

curryRule :: (HasState "info" Info m, HasState "net" (Net Lang) m)
         ⇒ (Int → Int → Int, Node)
         → (ProperPort, Node)
         → m ()
curryRule = curryRuleGen Curried'

curryRuleB :: (HasState "info" Info m, HasState "net" (Net Lang) m)
         ⇒ (Int → Int → Bool, Node)
         → (ProperPort, Node)
         → m ()
curryRuleB = curryRuleGen CurriedB'

fanIns :: (HasState "info" Info m, HasState "net" (Net Lang) m)
       ⇒ (Node, ProperPort) → (Node, ProperPort) → m ()
fanIns (numf1, FanIn {_lab = lab1}) (numf2, FanIn {_lab = lab2})
  | lab1 == lab2 = do
      incGraphSizeStep (-2)
      rewire (Aux1, numf1) (Aux2, numf2)
      rewire (Aux2, numf1) (Aux1, numf2)
      delNodesM [numf1, numf2]
  | otherwise =
    fanInAux2 numf1 (numf2, (FanIn' lab2)) lab1
fanIns _ _ = error "send to fanIn nodes!"
