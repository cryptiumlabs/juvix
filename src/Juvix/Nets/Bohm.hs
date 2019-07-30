{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}

module Juvix.Nets.Bohm where

import           Control.Lens
import           Prelude                  (Show (..))

import           Juvix.Backends.Env
import           Juvix.Backends.Interface
import           Juvix.Library            hiding (link, reduce)
import           Juvix.NodeInterface
import qualified Juvix.Utility.Helper     as H

instance Show ((->) a b) where
  show _ = "fun"

data InfixB = Eq | Neq | More | Less | Meq | Leq deriving Show

data Infix = Div | Sub | Add | Prod | Mod deriving Show

infix2FunctionB ∷ Ord a ⇒ InfixB → a → a → Bool
infix2FunctionB Eq   = (==)
infix2FunctionB Neq  = (/=)
infix2FunctionB More = (>)
infix2FunctionB Less = (<)
infix2FunctionB Meq  = (>=)
infix2FunctionB Leq  = (<=)

infix2Function ∷ Integral a ⇒ Infix → a → a → a
infix2Function Div  = div
infix2Function Add  = (+)
infix2Function Prod = (*)
infix2Function Mod  = mod
infix2Function Sub  = (-)

data Lang
  = Auxiliary3 Auxiliary3
  | Auxiliary2 Auxiliary2
  | Auxiliary1 Auxiliary1
  | Primar     Primar
  deriving Show

data Auxiliary3 = IfElse deriving Show
data Auxiliary2 = Or
                | And
                | Cons
                | Mu
                | Lambda
                | App
                | FanIn Int
                | Infix Infix
                | InfixB InfixB
                deriving Show
data Auxiliary1 = Not
                | Car
                | Cdr
                | TestNil
                | Curried (Int → Int)
                | CurriedB (Int → Bool)
                deriving Show
data Primar = Erase
            | Nil
            | Tru
            | Fals
            | IntLit Int
            | Symbol SomeSymbol
            deriving Show


data ProperPort
  = IsAux3 {_tag3 :: Auxiliary3, _prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary
           , _aux3 :: Auxiliary }
  | IsAux2 {_tag2 :: Auxiliary2, _prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary }
  | IsAux1 {_tag1 :: Auxiliary1, _prim :: Primary, _aux1 :: Auxiliary }
  | IsPrim {_tag0 :: Primar    , _prim :: Primary }
  deriving Show

makeFieldsNoPrefix ''ProperPort

-- Graph to more typed construction---------------------------------------------

-- Find a way to fix the ugliness!
langToProperPort ∷ (DifferentRep net, HasState "net" (net Lang) m)
                 ⇒ Node → m (Maybe ProperPort)
langToProperPort node = langToPort node (\l -> f l node)
  where
    f (Auxiliary3 a) = aux3FromGraph (IsAux3 a)
    f (Auxiliary2 a) = aux2FromGraph (IsAux2 a)
    f (Auxiliary1 a) = aux1FromGraph (IsAux1 a)
    f (Primar     a) = aux0FromGraph (IsPrim a)
-- Rewrite rules----------------------------------------------------------------
reduceAll ∷ (InfoNetworkDiff net Lang m)
          ⇒ Int → m ()
reduceAll = H.untilNothingNTimesM reduce

reduce ∷ (InfoNetworkDiff net Lang m) ⇒ m Bool
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
              IsAux3 tag (Primary node) _ _ _ →
                case tag of
                  IfElse →
                    langToProperPort node >>= \case
                      Just IsPrim {_tag0 = Fals} → True <$ ifElseRule node n False
                      Just IsPrim {_tag0 = Tru}  → True <$ ifElseRule node n True
                      _                          → pure isChanged
              IsAux2 tag (Primary node) _ _ →
                case tag of
                  And →
                    langToProperPort node >>= \case
                      Just IsPrim {_tag0 = Fals} → True <$ propPrimary (n, port) node
                      Just IsPrim {_tag0 = Tru}  → True <$ anihilateRewireAux node n
                      _                          → pure isChanged
                  Or →
                    langToProperPort node >>= \case
                      Just IsPrim {_tag0 = Fals} → True <$ anihilateRewireAux node n
                      Just IsPrim {_tag0 = Tru}  → True <$ propPrimary (n, port) node
                      _                          → pure isChanged
                  Cons →
                    langToProperPort node >>= \case
                      Just IsAux1 {_tag1 = Car} → True <$ consCdr n node
                      Just IsAux1 {_tag1 = Cdr} → True <$ consCar n node
                      _                         → pure isChanged
                  App →
                    langToProperPort node >>= \case
                      Just IsAux2 {_tag2 = Lambda} → True <$ anihilateRewireAuxTogether node n
                      _                            → pure isChanged
                  FanIn level →
                    langToProperPort node >>= \case
                      Just IsAux2 {_tag2 = FanIn lv2} → True <$ fanIns (n, level) (node, lv2)
                      Just IsPrim {_tag0 = Symbol _ } → pure isChanged
                      Just IsPrim {_tag0}             → True <$ fanInAux0 n (node, _tag0)
                      Just IsAux1 {_tag1}             → True <$ fanInAux1 n (node, _tag1) level
                      Just IsAux2 {_tag2}             → True <$ fanInAux2 n (node, _tag2) level
                      -- Update later to be `True <$ fanInAux3 n (node, _tag3) level`
                      Just IsAux3 {_tag3}             → pure isChanged
                      Nothing                         → pure isChanged
                  Infix inf   → curryOnInt  (infix2Function inf, n)   node isChanged
                  InfixB infb → curryOnIntB (infix2FunctionB infb, n) node isChanged
                  _           → pure isChanged
              IsAux1 tag (Primary node) _ →
                case tag of
                  Not →
                    langToProperPort node >>= \case
                      Nothing      → pure isChanged
                      Just x       → notExpand (node, x) (n, port) isChanged
                  Cdr →
                    langToProperPort node >>= \case
                      Just IsPrim {_tag0 = Nil} → True <$ propPrimary (n, port) node
                      _                         → pure isChanged
                  Curried curr →
                    langToProperPort node >>= \case
                      Just IsPrim {_tag0 = IntLit i} → True <$ curryInt (n, curr) (node, i)
                      _                              → pure isChanged
                  CurriedB currb →
                    langToProperPort node >>= \case
                      Just IsPrim {_tag0 = IntLit i} → True <$ curryIntB (n, currb) (node, i)
                      _                              → pure isChanged
                  _ → pure isChanged
              IsPrim tag (Primary node) →
                case tag of
                  Erase →
                    langToProperPort node >>= \case
                    Just x  → True <$ eraseAll (x, node) n
                    Nothing → pure isChanged
                  _ → pure isChanged
              _ → pure isChanged

curryOnInt ∷ (InfoNetworkDiff net Lang m)
           ⇒ (Int → Int → Int, Node) → Node → Bool → m Bool
curryOnInt opInfo node isChanged = do
  langToProperPort node >>= \case
    Just IsPrim {_tag0 = IntLit i} → True <$ curryRule opInfo (i, node)
    _                              → pure isChanged

curryOnIntB ∷ (InfoNetworkDiff net Lang m)
            ⇒ (Int → Int → Bool, Node) → Node → Bool → m Bool
curryOnIntB opInfo node isChanged =
  langToProperPort node >>= \case
    Just IsPrim {_tag0 = IntLit i} → True <$ curryRuleB opInfo (i, node)
    _                              → pure isChanged

propPrimary ∷ (Aux2 s, InfoNetwork net Lang m)
            ⇒ (Node, s) → Node → m ()
propPrimary (numDel, nodeDel) numProp = do
  relink (numDel, Aux1) (numProp, Prim)
  case auxToNode (nodeDel^.aux2) of
    Just _ -> do
      sequentalStep
      eraseNum ← newNode (Primar Erase)
      relink (numDel, Aux2) (eraseNum, Prim)
      deleteRewire [numDel] [eraseNum]
    Nothing -> do
      incGraphSizeStep (-1)
      delNodes [numDel]

ifElseRule ∷ (InfoNetwork net Lang m)
           ⇒ Node → Node → Bool → m ()
ifElseRule numPrimOnly numAuxs pred = do
  incGraphSizeStep (-1)
  numErase ← newNode (Primar Erase)
  if pred
    then do
    relink (numAuxs, Aux2) (numErase, Prim)
    rewire (numAuxs, Aux1) (numAuxs, Aux3)
    else do
    relink (numAuxs, Aux3) (numErase, Prim)
    rewire (numAuxs, Aux1) (numAuxs, Aux2)
  deleteRewire [numPrimOnly, numAuxs] [numErase]


anihilateRewireAux ∷ (InfoNetwork net Lang m)
                   ⇒ Node → Node → m ()
anihilateRewireAux numPrimOnly numAuxs = do
  incGraphSizeStep (-2)
  rewire (numAuxs, Aux1) (numAuxs, Aux2)
  delNodes [numPrimOnly, numAuxs]

-- used for app lambda!
anihilateRewireAuxTogether ∷ (InfoNetwork net Lang m)
                           ⇒ Node → Node → m ()
anihilateRewireAuxTogether numNode1 numNode2 = do
  incGraphSizeStep (-2)
  rewire (numNode1, Aux1) (numNode2, Aux1)
  rewire (numNode1, Aux2) (numNode2, Aux2)
  delNodes [numNode1, numNode2]

-- o is Aux1 * is Aux2
muExpand ∷ (InfoNetwork net Lang m)
         ⇒ Node → m ()
muExpand muNum = do
  incGraphSizeStep 2
  fanIn  ← newNode (Auxiliary2 $ FanIn 0)
  fanOut ← newNode (Auxiliary2 $ FanIn 0)
  newMu  ← newNode (Auxiliary2 $ FanIn 0)
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

fanInAux2 ∷ (InfoNetwork net Lang m)
          ⇒ Node → (Node, Auxiliary2) → Int → m ()
fanInAux2 numFan (numOther, otherLang) level = do
  incGraphSizeStep 2
  other1 ← newNode (Auxiliary2 otherLang)
  other2 ← newNode (Auxiliary2 otherLang)
  fanIn1 ← newNode (Auxiliary2 (FanIn level))
  fanIn2 ← newNode (Auxiliary2 (FanIn level))
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

fanInAux0 ∷ (InfoNetwork net Lang m)
          ⇒ Node → (Node, Primar) → m ()
fanInAux0 numFan (numOther, otherLang) = do
  sequentalStep
  other1 ← newNode (Primar otherLang)
  other2 ← newNode (Primar otherLang)
  let nodeOther1 = RELAuxiliary0 { node       = other1
                                 , primary    = ReLink numFan Aux1
                                 }
      nodeOther2 = RELAuxiliary0 { node       = other2
                                 , primary    = ReLink numFan Aux2
                                 }
  traverse_ linkAll [nodeOther1, nodeOther2]
  deleteRewire [numFan, numOther] [other1, other2]

fanInAux1 ∷ (InfoNetwork net Lang m)
          ⇒ Node → (Node, Auxiliary1) → Int → m ()
fanInAux1 numFan (numOther, otherLang) level = do
  incGraphSizeStep 1
  other1 ← newNode (Auxiliary1 otherLang)
  other2 ← newNode (Auxiliary1 otherLang)
  fanIn1 ← newNode (Auxiliary2 (FanIn level))
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
notExpand ∷ (Aux2 s, InfoNetwork net Lang m)
          ⇒ (Node, ProperPort)
          → (Node, s)
          → Bool
          → m Bool
notExpand (n, IsPrim {_tag0 = Tru}) (notNum, notPort) _ = do
  numFals ← newNode (Primar Fals)
  delNodes [n]
  propPrimary (notNum, notPort) numFals
  pure True

notExpand (n, IsPrim {_tag0 = Fals}) (notNum, notPort) _ = do
  numFals ← newNode (Primar Tru)
  delNodes [n]
  propPrimary (notNum, notPort) numFals
  pure True

notExpand _ _ updated = pure updated

-- Erase should be connected to the main port atm, change this logic later
-- when this isn't the case
eraseAll ∷ (Aux3 s, InfoNetwork net Lang m)
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
      numE ← newNode (Primar Erase)
      relink (numNode, port) (numE, Prim)
      return (Just numE)

consCar ∷ InfoNetwork net Lang m
        ⇒ Node → Node → m ()
consCar numCons numCar = do
  incGraphSizeStep (-1)
  erase ← newNode (Primar Erase)
  relink (numCons, Aux1) (erase, Prim)
  rewire (numCons, Aux2) (numCar, Aux1)
  deleteRewire [numCons, numCar] [erase]

consCdr ∷ InfoNetwork net Lang m
        ⇒ Node → Node → m ()
consCdr numCons numCdr = do
  incGraphSizeStep (-1)
  erase ← newNode (Primar Erase)
  relink (numCons, Aux2) (erase, Prim)
  rewire (numCons, Aux1) (numCdr, Aux1)
  deleteRewire [numCons, numCdr] [erase]

testNilNil ∷ InfoNetwork net Lang m
           ⇒ Node → Node → m ()
testNilNil numTest numNil = do
  incGraphSizeStep (-1)
  true ← newNode (Primar Tru)
  relink (numTest, Aux1) (true, Prim)
  deleteRewire [numTest, numNil] [true]

testNilCons ∷ InfoNetwork net Lang m
            ⇒ Node → Node → m ()
testNilCons numCons numTest = do
  incGraphSizeStep 1
  erase1 ← newNode (Primar Erase)
  erase2 ← newNode (Primar Erase)
  false  ← newNode (Primar Fals)
  traverse_ (uncurry relink)
            [((numCons, Aux1), (erase1, Prim))
            ,((numCons, Aux2), (erase2, Prim))
            ,((numTest, Aux1), (false , Prim))]
  deleteRewire [numCons, numTest] [erase1, erase2, false]


curryRuleGen ∷ InfoNetwork net a m
              ⇒ (t → a)
              → (Int → t, Node)
              → (Int, Node)
              → m ()
curryRuleGen con (nodeF, numNode) (i, numInt) = do
  incGraphSizeStep (-1)
  curr         ← newNode (con (nodeF i))
  let currNode = RELAuxiliary1 { node       = curr
                               , primary    = ReLink numNode Aux2
                               , auxiliary1 = ReLink numNode Aux1
                               }
  linkAll currNode
  deleteRewire [numNode, numInt] [curr]

curryRule ∷ InfoNetwork net Lang m
         ⇒ (Int → Int → Int, Node)
         → (Int, Node)
         → m ()
curryRule = curryRuleGen (Auxiliary1 . Curried)

curryRuleB ∷ InfoNetwork net Lang m
         ⇒ (Int → Int → Bool, Node)
         → (Int, Node)
         → m ()
curryRuleB = curryRuleGen (Auxiliary1 . CurriedB)

fanIns ∷ InfoNetwork net Lang m
       ⇒ (Node, Int) → (Node, Int) → m ()
fanIns (numf1, lab1) (numf2, lab2)
  | lab1 == lab2 = do
      incGraphSizeStep (-2)
      rewire (numf1, Aux1) (numf2, Aux2)
      rewire (numf1, Aux2) (numf2, Aux1)
      delNodes [numf1, numf2]
  | otherwise =
    fanInAux2 numf1 (numf2, (FanIn lab2)) lab1


curryInt ∷ InfoNetwork net Lang m
         ⇒ (Node, (Int → Int)) → (Node, Int) → m ()
curryInt (numCurr, _curried) (numInt, i) = do
  incGraphSizeStep (-1)
  intLit ← newNode (Primar (IntLit (_curried i)))
  relink (numCurr, Aux1) (intLit, Prim)
  deleteRewire [numCurr, numInt] [intLit]

curryIntB ∷ InfoNetwork net Lang m
          ⇒ (Node, (Int → Bool)) → (Node, Int) → m ()
curryIntB (numCurr, _curriedB) (numInt, i) = do
  incGraphSizeStep (-1)
  let app = _curriedB i
  node ← newNode
          (case app of
            True  → Primar Tru
            False → Primar Fals)
  relink (numCurr, Aux1) (node, Prim)
  deleteRewire [numCurr, numInt] [node]
