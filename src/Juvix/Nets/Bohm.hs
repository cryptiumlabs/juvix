{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Juvix.Nets.Bohm where

import           Control.Lens
import           Prelude (Show(..), error)

import           Juvix.Library hiding (link, reduce)
import           Juvix.NodeInterface
import           Juvix.Backends.Env
import           Juvix.Backends.Interface
import qualified Juvix.Utility.Helper as H

data InfixB = Eq' | Neq' | More' | Less' | Meq' | Leq' deriving Show

data Infix = Mu' | Div' | Sub' | Add' | Prod' | Mod' deriving Show

data Lang
  = Infix Infix
  | InfixB InfixB
  | And'
  | Or'
  | Not'
  | Cons'
  | Car'
  | Cdr'
  | Nil'
  | TestNil'
  | IfElse'
  | Tru'
  | Fals'
  | IntLit' Int
  | Lambda'
  | Symbol' SomeSymbol
  | App'
  | FanIn' Int
  | Curried'  (Int → Int)
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
 | Erase   {_prim :: Primary}
   -- Lang could be a tighter bound, but that would require more boiler plate
 | Curried  {_prim :: Primary, _aux1 :: Auxiliary, _curried  :: (Int → Int) }
 | CurriedB {_prim :: Primary, _aux1 :: Auxiliary, _curriedB :: (Int → Bool) }
 deriving Show

instance Show ((->) a b) where
  show _ = "fun"

makeFieldsNoPrefix ''ProperPort

-- Graph to more typed construction---------------------------------------------

-- Find a way to fix the ugliness!
langToProperPort :: (DifferentRep net, HasState "net" (net Lang) m)
                 ⇒ Node → m (Maybe ProperPort)
langToProperPort node = langToPort node (\l -> f l node)
  where
    f (InfixB Meq')  = aux2FromGraph Meq
    f (InfixB Eq')   = aux2FromGraph Eq
    f (InfixB Neq')  = aux2FromGraph Neq
    f (InfixB More') = aux2FromGraph More
    f (InfixB Less') = aux2FromGraph Less
    f (InfixB Leq')  = aux2FromGraph Leq
    f (Infix Mu')    = aux2FromGraph Mu
    f (Infix Div')   = aux2FromGraph Div
    f (Infix Sub')   = aux2FromGraph Sub
    f (Infix Add')   = aux2FromGraph Add
    f (Infix Prod')  = aux2FromGraph Prod
    f (Infix Mod')   = aux2FromGraph Mod
    f Or'            = aux2FromGraph Or
    f And'           = aux2FromGraph And
    f Not'           = aux1FromGraph Not
    f Cons'          = aux2FromGraph Cons
    f Car'           = aux1FromGraph Car
    f Cdr'           = aux1FromGraph Cdr
    f Nil'           = aux0FromGraph Nil
    f TestNil'       = aux1FromGraph TestNil
    f IfElse'        = aux3FromGraph IfElse
    f Tru'           = aux0FromGraph Tru
    f Fals'          = aux0FromGraph Fals
    f Lambda'        = aux2FromGraph Lambda
    f App'           = aux2FromGraph App
    f Erase'         = aux0FromGraph Erase
    f (FanIn' i)     = aux2FromGraph (\p a1 a2 → FanIn p a1 a2 i)
    f (IntLit' i)    = aux0FromGraph (\x -> IntLit x i)
    f (Symbol' s)    = aux0FromGraph (\x -> Symbol x s)
    f (Curried'  c)  = aux1FromGraph (\x y -> Curried x y c)
    f (CurriedB' c)  = aux1FromGraph (\x y -> CurriedB x y c)
-- Rewrite rules----------------------------------------------------------------
reduceAll :: (InfoNetworkDiff net Lang m)
          ⇒ Int → m ()
reduceAll = H.untilNothingNTimesM reduce

reduce :: (InfoNetworkDiff net Lang m) ⇒ m Bool
reduce = do
  nodes'    ← nodes
  isChanged ← foldrM update False nodes'
  if isChanged then do
    modify @"info" (\c → c {parallelSteps = parallelSteps c + 1})
    pure isChanged
    else
    pure isChanged
  where
    update n isChanged = do
      both ← isBothPrimary n
      if not both
        then pure isChanged
        else
        langToProperPort n >>= \case
          Nothing → pure isChanged
          Just port → do
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
              App (Primary node) _ _ →
                langToProperPort node >>= \case
                  Just Lambda {} → True <$ anihilateRewireAuxTogether node n
                  _              → pure isChanged
              Cdr (Primary node) _ →
                langToProperPort node >>= \case
                  Just Nil {} → True <$ propPrimary (n, port) node
                  _           → pure isChanged
              Cons (Primary node) _ _ →
                langToProperPort node >>= \case
                  Just Cdr {} → True <$ consCdr n node
                  Just Car {} → True <$ consCar n node
                  _           → pure isChanged
              FanIn (Primary node) _ _ level →
                langToProperPort node >>= \case
                  Just App      {}          → True <$ fanInAux2 n (node, App')    level
                  Just Lambda   {}          → True <$ fanInAux2 n (node, Lambda') level
                  Just And      {}          → True <$ fanInAux2 n (node, And') level
                  Just Or       {}          → True <$ fanInAux2 n (node, Or') level
                  Just Cons     {}          → True <$ fanInAux2 n (node, Cons') level
                  Just Car      {}          → True <$ fanInAux1 n (node, Car') level
                  Just Cdr      {}          → True <$ fanInAux1 n (node, Cdr') level
                  Just Not      {}          → True <$ fanInAux1 n (node, Not') level
                  Just TestNil  {}          → True <$ fanInAux1 n (node, TestNil') level
                  Just f@FanIn  {}          → True <$ fanIns (n, port) (node, f)
                  Just Mu       {}          → True <$ fanInAux2 n (node, (Infix Mu')) level
                  Just Less     {}          → True <$ fanInAux2 n (node, (InfixB Less')) level
                  Just More     {}          → True <$ fanInAux2 n (node, (InfixB More')) level
                  Just Leq      {}          → True <$ fanInAux2 n (node, (InfixB Leq')) level
                  Just Meq      {}          → True <$ fanInAux2 n (node, (InfixB Meq')) level
                  Just Eq       {}          → True <$ fanInAux2 n (node, (InfixB Eq')) level
                  Just Neq      {}          → True <$ fanInAux2 n (node, (InfixB Neq')) level
                  Just Div      {}          → True <$ fanInAux2 n (node, (Infix Div')) level
                  Just Prod     {}          → True <$ fanInAux2 n (node, (Infix Prod')) level
                  Just Mod      {}          → True <$ fanInAux2 n (node, (Infix Mod')) level
                  Just Add      {}          → True <$ fanInAux2 n (node, (Infix Add')) level
                  Just Sub      {}          → True <$ fanInAux2 n (node, (Infix Sub')) level
                  Just Tru      {}          → True <$ fanInAux0 n (node, Tru')
                  Just Fals     {}          → True <$ fanInAux0 n (node, Fals')
                  Just Erase    {}          → True <$ fanInAux0 n (node, Erase')
                  Just Nil      {}          → True <$ fanInAux0 n (node, Nil')
                  Just Symbol   {_symb}     → True <$ fanInAux0 n (node, (Symbol' _symb))
                  Just IntLit   {_int}      → True <$ fanInAux0 n (node, (IntLit' _int))
                  Just CurriedB {_curriedB} → True <$ fanInAux1 n (node, (CurriedB' _curriedB)) level
                  Just Curried  {_curried}  → True <$ fanInAux1 n (node, (Curried' _curried))   level
                  -- Update later to be fanInAux3
                  Just IfElse  {}           → pure isChanged
                  Nothing      {}           → pure isChanged
              Erase (Primary node) →
                langToProperPort node >>= \case
                  Just x  → True <$ eraseAll (x, node) n
                  Nothing → pure isChanged
              Curried (Primary node) _ _ →
                langToProperPort node >>= \case
                  Just i@IntLit {} → True <$ curryInt (n, port) (node, i)
                  _                → pure isChanged
              CurriedB (Primary node) _ _ →
                langToProperPort node >>= \case
                  Just i@IntLit {} → True <$ curryIntB (n, port) (node, i)
                  _                → pure isChanged
              Eq   (Primary node) _ _ → curryOnIntB ((==), n) node isChanged
              Neq  (Primary node) _ _ → curryOnIntB ((/=), n) node isChanged
              More (Primary node) _ _ → curryOnIntB ((>) , n) node isChanged
              Less (Primary node) _ _ → curryOnIntB ((<) , n) node isChanged
              Meq  (Primary node) _ _ → curryOnIntB ((>=), n) node isChanged
              Leq  (Primary node) _ _ → curryOnIntB ((<=), n) node isChanged
              Add  (Primary node) _ _ → curryOnInt  ((*) , n) node isChanged
              Sub  (Primary node) _ _ → curryOnInt  ((-) , n) node isChanged
              Div  (Primary node) _ _ → curryOnInt  (div , n) node isChanged
              Prod (Primary node) _ _ → curryOnInt  ((*) , n) node isChanged
              Mod  (Primary node) _ _ → curryOnInt  (mod , n) node isChanged
              _ → pure isChanged

curryOnInt :: (InfoNetworkDiff net Lang m)
           ⇒ (Int → Int → Int, Node) → Node → Bool → m Bool
curryOnInt opInfo node isChanged = do
  langToProperPort node >>= \case
    Just i@IntLit {} → True <$ curryRule opInfo (i, node)
    _                → pure isChanged

curryOnIntB :: (InfoNetworkDiff net Lang m)
            ⇒ (Int → Int → Bool, Node) → Node → Bool → m Bool
curryOnIntB opInfo node isChanged =
  langToProperPort node >>= \case
    Just i@IntLit {} → True <$ curryRuleB opInfo (i, node)
    _                → pure isChanged

propPrimary :: (Aux2 s, InfoNetwork net Lang m)
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
      delNodes [numDel]

ifElseRule :: (InfoNetwork net Lang m)
           ⇒ Node → Node → Bool → m ()
ifElseRule numPrimOnly numAuxs pred = do
  incGraphSizeStep (-1)
  numErase ← newNode Erase'
  if pred
    then do
    relink (numAuxs, Aux2) (numErase, Prim)
    rewire (numAuxs, Aux1) (numAuxs, Aux3)
    else do
    relink (numAuxs, Aux3) (numErase, Prim)
    rewire (numAuxs, Aux1) (numAuxs, Aux2)
  deleteRewire [numPrimOnly, numAuxs] [numErase]


anihilateRewireAux :: (InfoNetwork net Lang m)
                   ⇒ Node → Node → m ()
anihilateRewireAux numPrimOnly numAuxs = do
  incGraphSizeStep (-2)
  rewire (numAuxs, Aux1) (numAuxs, Aux2)
  delNodes [numPrimOnly, numAuxs]

-- used for app lambda!
anihilateRewireAuxTogether :: (InfoNetwork net Lang m)
                           ⇒ Node → Node → m ()
anihilateRewireAuxTogether numNode1 numNode2 = do
  incGraphSizeStep (-2)
  rewire (numNode1, Aux1) (numNode2, Aux1)
  rewire (numNode1, Aux2) (numNode2, Aux2)
  delNodes [numNode1, numNode2]

-- o is Aux1 * is Aux2
muExpand :: (InfoNetwork net Lang m)
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

fanInAux2 :: (InfoNetwork net Lang m)
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

fanInAux0 :: (InfoNetwork net Lang m)
          ⇒ Node → (Node, Lang) → m ()
fanInAux0 numFan (numOther, otherLang) = do
  sequentalStep
  other1 ← newNode otherLang
  other2 ← newNode otherLang
  let nodeOther1 = RELAuxiliary0 { node       = other1
                                 , primary    = ReLink numFan Aux1
                                 }
      nodeOther2 = RELAuxiliary0 { node       = other2
                                 , primary    = ReLink numFan Aux2
                                 }
  traverse_ linkAll [nodeOther1, nodeOther2]
  deleteRewire [numFan, numOther] [other1, other2]

fanInAux1 :: (InfoNetwork net Lang m)
          ⇒ Node → (Node, Lang) → Int → m ()
fanInAux1 numFan (numOther, otherLang) level = do
  incGraphSizeStep 1
  other1 ← newNode otherLang
  other2 ← newNode otherLang
  fanIn1 ← newNode (FanIn' level)
  let nodeOther1 = RELAuxiliary1 { node       = other1
                                 , primary    = ReLink numFan Aux1
                                 , auxiliary1 = Link (Port Aux1 fanIn1)
                                 }
      nodeOther2 = RELAuxiliary1 { node       = other2
                                 , primary    = ReLink numFan Aux2
                                 , auxiliary1 = Link (Port Aux2 fanIn1)
                                 }
      nodeFan1   = RELAuxiliary2 { node       = fanIn1
                                 , primary    = ReLink numOther Aux1
                                 , auxiliary1 = Link (Port Aux1 other1)
                                 , auxiliary2 = Link (Port Aux1 other2)
                                 }
  traverse_ linkAll [nodeOther1, nodeOther2, nodeFan1]
  deleteRewire [numFan, numOther] [other1, other2, fanIn1]

-- TODO :: delete node coming in!
notExpand :: (Aux2 s, InfoNetwork net Lang m)
          ⇒ (Node, ProperPort)
          → (Node, s)
          → Bool
          → m Bool
notExpand (n, Tru {}) (notNum, notPort) _ = do
  numFals ← newNode Fals'
  delNodes [n]
  propPrimary (notNum, notPort) numFals
  pure True

notExpand (n, Fals {}) (notNum, notPort) _ = do
  numFals ← newNode Tru'
  delNodes [n]
  propPrimary (notNum, notPort) numFals
  pure True

notExpand _ _ updated = pure updated

-- Erase should be connected to the main port atm, change this logic later
-- when this isn't the case
eraseAll :: (Aux3 s, InfoNetwork net Lang m)
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

consCar :: InfoNetwork net Lang m
        ⇒ Node → Node → m ()
consCar numCons numCar = do
  incGraphSizeStep (-1)
  erase ← newNode Erase'
  relink (numCons, Aux1) (erase, Prim)
  rewire (numCons, Aux2) (numCar, Aux1)
  deleteRewire [numCons, numCar] [erase]

consCdr :: InfoNetwork net Lang m
        ⇒ Node → Node → m ()
consCdr numCons numCdr = do
  incGraphSizeStep (-1)
  erase ← newNode Erase'
  relink (numCons, Aux2) (erase, Prim)
  rewire (numCons, Aux1) (numCdr, Aux1)
  deleteRewire [numCons, numCdr] [erase]

testNilNil :: InfoNetwork net Lang m
           ⇒ Node → Node → m ()
testNilNil numTest numNil = do
  incGraphSizeStep (-1)
  true ← newNode Tru'
  relink (numTest, Aux1) (true, Prim)
  deleteRewire [numTest, numNil] [true]

testNilCons :: InfoNetwork net Lang m
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


curryRuleGen :: InfoNetwork net a m
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

curryRule :: InfoNetwork net Lang m
         ⇒ (Int → Int → Int, Node)
         → (ProperPort, Node)
         → m ()
curryRule = curryRuleGen Curried'

curryRuleB :: InfoNetwork net Lang m
         ⇒ (Int → Int → Bool, Node)
         → (ProperPort, Node)
         → m ()
curryRuleB = curryRuleGen CurriedB'

fanIns :: InfoNetwork net Lang m
       ⇒ (Node, ProperPort) → (Node, ProperPort) → m ()
fanIns (numf1, FanIn {_lab = lab1}) (numf2, FanIn {_lab = lab2})
  | lab1 == lab2 = do
      incGraphSizeStep (-2)
      rewire (numf1, Aux1) (numf2, Aux2)
      rewire (numf1, Aux2) (numf2, Aux1)
      delNodes [numf1, numf2]
  | otherwise =
    fanInAux2 numf1 (numf2, (FanIn' lab2)) lab1
fanIns _ _ = error "send to fanIn nodes!"


curryInt :: InfoNetwork net Lang m
         ⇒ (Node, ProperPort) → (Node, ProperPort) → m ()
curryInt (numCurr, Curried {_curried}) (numInt, (IntLit _ i)) = do
  incGraphSizeStep (-1)
  intLit ← newNode (IntLit' (_curried i))
  relink (numInt, Prim) (intLit, Prim)
  deleteRewire [numCurr, numInt] [intLit]

curryInt _ _ = error "sent in a non curry node"

curryIntB :: InfoNetwork net Lang m
          ⇒ (Node, ProperPort) → (Node, ProperPort) → m ()
curryIntB (numCurr, CurriedB {_curriedB}) (numInt, (IntLit _ i)) = do
  incGraphSizeStep (-1)
  let app = _curriedB i
  node ← newNode
          (case app of
            True  → Tru'
            False → Fals')
  relink (numInt, Prim) (node, Prim)
  deleteRewire [numCurr, numInt] [node]

curryIntB _ _ = error "sent in a non curry node"
