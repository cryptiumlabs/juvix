{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Juvix.Nets.Bohm where

import Control.Lens
import Juvix.Backends.Env
import Juvix.Backends.Interface
import Juvix.Bohm.Shared
import Juvix.Library hiding (link, reduce)
import Juvix.NodeInterface
import qualified Juvix.Utility.Helper as H
import Prelude (Show (..))

data Lang
  = Auxiliary3 Auxiliary3
  | Auxiliary2 Auxiliary2
  | Auxiliary1 Auxiliary1
  | Primar Primar
  deriving (Show)

-- the final argument for the curries is maybe, as if the nodes don't line up
-- a final type can't be constructed. This is untyped
-- so type check at a higher level
data Auxiliary3
  = IfElse
  | Curried3 (Primitive → Primitive → Primitive → Maybe Primitive)
  deriving (Show)

data Auxiliary2
  = Or
  | And
  | Cons
  | Mu
  | Lambda
  | App
  | FanIn Int
  | Curried2 (Primitive → Primitive → Maybe Primitive)
  deriving (Show)

data Auxiliary1
  = Not
  | Car
  | Cdr
  | TestNil
  | Curried1 (Primitive → Maybe Primitive)
  deriving (Show)

data Primar
  = Erase
  | Nil
  | Tru
  | Fals
  | IntLit Int
  | Symbol Symbol
  deriving (Show)

data ProperPort
  = IsAux3
      { _tag3 ∷ Auxiliary3,
        _prim ∷ Primary,
        _aux1 ∷ Auxiliary,
        _aux2 ∷ Auxiliary,
        _aux3 ∷ Auxiliary
      }
  | IsAux2 {_tag2 ∷ Auxiliary2, _prim ∷ Primary, _aux1 ∷ Auxiliary, _aux2 ∷ Auxiliary}
  | IsAux1 {_tag1 ∷ Auxiliary1, _prim ∷ Primary, _aux1 ∷ Auxiliary}
  | IsPrim {_tag0 ∷ Primar, _prim ∷ Primary}
  deriving (Show)

makeFieldsNoPrefix ''ProperPort

-- Graph to more typed construction---------------------------------------------

-- Find a way to fix the ugliness!
langToProperPort ∷
  (DifferentRep net, HasState "net" (net Lang) m) ⇒
  Node →
  m (Maybe ProperPort)
langToProperPort node = langToPort node (\l → f l node)
  where
    f (Auxiliary3 a) = aux3FromGraph (IsAux3 a)
    f (Auxiliary2 a) = aux2FromGraph (IsAux2 a)
    f (Auxiliary1 a) = aux1FromGraph (IsAux1 a)
    f (Primar a) = aux0FromGraph (IsPrim a)

-- Rewrite rules----------------------------------------------------------------
reduceAll ∷
  (InfoNetworkDiff net Lang m) ⇒
  Int →
  m ()
reduceAll = H.untilNothingNTimesM reduce

reduce ∷ (InfoNetworkDiff net Lang m) ⇒ m Bool
reduce = do
  nodes' <- nodes
  isChanged <- foldrM update False nodes'
  if isChanged
    then do
      modify @"info" (\c → c {parallelSteps = parallelSteps c + 1})
      pure isChanged
    else pure isChanged
  where
    update n isChanged = do
      both <- isBothPrimary n
      if not both
        then pure isChanged
        else langToProperPort n >>= \case
          Nothing → pure isChanged
          Just port → do
            case port of
              IsAux3 tag (Primary node) _ _ _ →
                case tag of
                  IfElse →
                    langToProperPort node >>= \case
                      Just IsPrim {_tag0 = Fals} → True <$ ifElseRule node n False
                      Just IsPrim {_tag0 = Tru} → True <$ ifElseRule node n True
                      _ → pure isChanged
                  Curried3 f → do
                    curryMatch curry3 (f, n) node isChanged
              IsAux2 tag (Primary node) _ _ →
                case tag of
                  And →
                    langToProperPort node >>= \case
                      Just IsPrim {_tag0 = Fals} → True <$ propPrimary (n, port) node
                      Just IsPrim {_tag0 = Tru} → True <$ anihilateRewireAux node n
                      _ → pure isChanged
                  Or →
                    langToProperPort node >>= \case
                      Just IsPrim {_tag0 = Fals} → True <$ anihilateRewireAux node n
                      Just IsPrim {_tag0 = Tru} → True <$ propPrimary (n, port) node
                      _ → pure isChanged
                  Cons →
                    langToProperPort node >>= \case
                      Just IsAux1 {_tag1 = Car} → True <$ consCdr n node
                      Just IsAux1 {_tag1 = Cdr} → True <$ consCar n node
                      _ → pure isChanged
                  App →
                    langToProperPort node >>= \case
                      Just IsAux2 {_tag2 = Lambda} → True <$ anihilateRewireAuxTogether node n
                      _ → pure isChanged
                  FanIn level →
                    langToProperPort node >>= \case
                      Just IsAux2 {_tag2 = FanIn lv2} → True <$ fanIns (n, level) (node, lv2)
                      Just IsPrim {_tag0 = Symbol _} → pure isChanged
                      Just IsPrim {_tag0} → True <$ fanInAux0 n (node, _tag0)
                      Just IsAux1 {_tag1} → True <$ fanInAux1 n (node, _tag1) level
                      Just IsAux2 {_tag2} → True <$ fanInAux2 n (node, _tag2) level
                      -- Update later to be `True <$ fanInAux3 n (node, _tag3) level`
                      Just IsAux3 {_tag3} → pure isChanged
                      Nothing → pure isChanged
                  Curried2 f → curryMatch curry2 (f, n) node isChanged
                  -- Cases in which we fall through, and have the other node handle it
                  Mu → pure isChanged
                  Lambda → pure isChanged
              IsAux1 tag (Primary node) _ →
                case tag of
                  Not →
                    langToProperPort node >>= \case
                      Nothing → pure isChanged
                      Just x → notExpand (node, x) (n, port) isChanged
                  Cdr →
                    langToProperPort node >>= \case
                      Just IsPrim {_tag0 = Nil} → True <$ propPrimary (n, port) node
                      _ → pure isChanged
                  -- case is slightly different from other cases
                  -- just return what the function gives us!
                  -- TODO ∷ Unify logic with other cases
                  Curried1 f →
                    langToProperPort node >>= \case
                      Just IsPrim {_tag0 = Fals} → curry1 (f, n) (PBool False, node)
                      Just IsPrim {_tag0 = Tru} → curry1 (f, n) (PBool True, node)
                      Just IsPrim {_tag0 = IntLit i} → curry1 (f, n) (PInt i, node)
                      _ → pure isChanged
                  -- Fall through cases
                  Car → pure isChanged
                  TestNil → pure isChanged
              IsPrim tag (Primary node) →
                case tag of
                  Erase →
                    langToProperPort node >>= \case
                      Just x → True <$ eraseAll (x, node) n
                      Nothing → pure isChanged
                  -- Fall through cases
                  Nil → pure isChanged
                  Tru → pure isChanged
                  Fals → pure isChanged
                  IntLit _ → pure isChanged
                  Symbol _ → pure isChanged
              -- Fall through cases
              IsAux3 _ Free _ _ _ → pure isChanged
              IsAux2 _ Free _ _ → pure isChanged
              IsAux1 _ Free _ → pure isChanged
              IsPrim _ Free → pure isChanged

curryMatch ∷
  (InfoNetworkDiff net Lang m) ⇒
  (t → (Primitive, Node) → m b) →
  t →
  Node →
  Bool →
  m Bool
curryMatch curry currNodeInfo nodeConnected isChanged = do
  langToProperPort nodeConnected >>= \case
    Just IsPrim {_tag0 = Fals} → True <$ curry currNodeInfo (PBool False, nodeConnected)
    Just IsPrim {_tag0 = Tru} → True <$ curry currNodeInfo (PBool True, nodeConnected)
    Just IsPrim {_tag0 = IntLit i} → True <$ curry currNodeInfo (PInt i, nodeConnected)
    _ → pure isChanged

propPrimary ∷
  (Aux2 s, InfoNetwork net Lang m) ⇒
  (Node, s) →
  Node →
  m ()
propPrimary (numDel, nodeDel) numProp = do
  relink (numDel, Aux1) (numProp, Prim)
  case auxToNode (nodeDel ^. aux2) of
    Just _ → do
      sequentalStep
      eraseNum <- newNode (Primar Erase)
      relink (numDel, Aux2) (eraseNum, Prim)
      deleteRewire [numDel] [eraseNum]
    Nothing → do
      incGraphSizeStep (- 1)
      delNodes [numDel]

ifElseRule ∷
  (InfoNetwork net Lang m) ⇒
  Node →
  Node →
  Bool →
  m ()
ifElseRule numPrimOnly numAuxs pred = do
  incGraphSizeStep (- 1)
  numErase <- newNode (Primar Erase)
  if pred
    then do
      relink (numAuxs, Aux2) (numErase, Prim)
      rewire (numAuxs, Aux1) (numAuxs, Aux3)
    else do
      relink (numAuxs, Aux3) (numErase, Prim)
      rewire (numAuxs, Aux1) (numAuxs, Aux2)
  deleteRewire [numPrimOnly, numAuxs] [numErase]

anihilateRewireAux ∷
  (InfoNetwork net Lang m) ⇒
  Node →
  Node →
  m ()
anihilateRewireAux numPrimOnly numAuxs = do
  incGraphSizeStep (- 2)
  rewire (numAuxs, Aux1) (numAuxs, Aux2)
  delNodes [numPrimOnly, numAuxs]

-- used for app lambda!
anihilateRewireAuxTogether ∷
  (InfoNetwork net Lang m) ⇒
  Node →
  Node →
  m ()
anihilateRewireAuxTogether numNode1 numNode2 = do
  incGraphSizeStep (- 2)
  rewire (numNode1, Aux1) (numNode2, Aux1)
  rewire (numNode1, Aux2) (numNode2, Aux2)
  delNodes [numNode1, numNode2]

-- o is Aux1 * is Aux2
muExpand ∷
  (InfoNetwork net Lang m) ⇒
  Node →
  m ()
muExpand muNum = do
  incGraphSizeStep 2
  fanIn <- newNode (Auxiliary2 $ FanIn 0)
  fanOut <- newNode (Auxiliary2 $ FanIn 0)
  newMu <- newNode (Auxiliary2 $ FanIn 0)
  let nodeFanIn = RELAuxiliary2
        { node = fanIn,
          primary = ReLink muNum Aux1,
          auxiliary1 = Link (Port Aux1 newMu),
          auxiliary2 = ReLink muNum Prim
        }
      nodeFanOut = RELAuxiliary2
        { node = fanOut,
          primary = ReLink muNum Aux2,
          auxiliary1 = Link (Port Aux2 newMu),
          auxiliary2 = Link (Port Prim newMu)
        }
  traverse_ linkAll [nodeFanIn, nodeFanOut]
  deleteRewire [muNum] [fanIn, fanOut, newMu]

fanInAux2 ∷
  (InfoNetwork net Lang m) ⇒
  Node →
  (Node, Auxiliary2) →
  Int →
  m ()
fanInAux2 numFan (numOther, otherLang) level = do
  incGraphSizeStep 2
  other1 <- newNode (Auxiliary2 otherLang)
  other2 <- newNode (Auxiliary2 otherLang)
  fanIn1 <- newNode (Auxiliary2 (FanIn level))
  fanIn2 <- newNode (Auxiliary2 (FanIn level))
  let nodeOther1 = RELAuxiliary2
        { node = other1,
          primary = ReLink numFan Aux1,
          auxiliary1 = Link (Port Aux1 fanIn2),
          auxiliary2 = Link (Port Aux1 fanIn1)
        }
      nodeOther2 = RELAuxiliary2
        { node = other2,
          primary = ReLink numFan Aux2,
          auxiliary1 = Link (Port Aux2 fanIn2),
          auxiliary2 = Link (Port Aux2 fanIn1)
        }
      nodeFan1 = RELAuxiliary2
        { node = fanIn1,
          primary = ReLink numOther Aux2,
          auxiliary1 = Link (Port Aux2 other1),
          auxiliary2 = Link (Port Aux2 other2)
        }
      nodeFan2 = RELAuxiliary2
        { node = fanIn2,
          primary = ReLink numOther Aux1,
          auxiliary1 = Link (Port Aux1 other1),
          auxiliary2 = Link (Port Aux1 other2)
        }
  traverse_ linkAll [nodeOther1, nodeOther2, nodeFan1, nodeFan2]
  deleteRewire [numFan, numOther] [other1, other2, fanIn1, fanIn2]

fanInAux0 ∷
  (InfoNetwork net Lang m) ⇒
  Node →
  (Node, Primar) →
  m ()
fanInAux0 numFan (numOther, otherLang) = do
  sequentalStep
  other1 <- newNode (Primar otherLang)
  other2 <- newNode (Primar otherLang)
  let nodeOther1 = RELAuxiliary0
        { node = other1,
          primary = ReLink numFan Aux1
        }
      nodeOther2 = RELAuxiliary0
        { node = other2,
          primary = ReLink numFan Aux2
        }
  traverse_ linkAll [nodeOther1, nodeOther2]
  deleteRewire [numFan, numOther] [other1, other2]

fanInAux1 ∷
  (InfoNetwork net Lang m) ⇒
  Node →
  (Node, Auxiliary1) →
  Int →
  m ()
fanInAux1 numFan (numOther, otherLang) level = do
  incGraphSizeStep 1
  other1 <- newNode (Auxiliary1 otherLang)
  other2 <- newNode (Auxiliary1 otherLang)
  fanIn1 <- newNode (Auxiliary2 (FanIn level))
  let nodeOther1 = RELAuxiliary1
        { node = other1,
          primary = ReLink numFan Aux1,
          auxiliary1 = Link (Port Aux1 fanIn1)
        }
      nodeOther2 = RELAuxiliary1
        { node = other2,
          primary = ReLink numFan Aux2,
          auxiliary1 = Link (Port Aux2 fanIn1)
        }
      nodeFan1 = RELAuxiliary2
        { node = fanIn1,
          primary = ReLink numOther Aux1,
          auxiliary1 = Link (Port Aux1 other1),
          auxiliary2 = Link (Port Aux1 other2)
        }
  traverse_ linkAll [nodeOther1, nodeOther2, nodeFan1]
  deleteRewire [numFan, numOther] [other1, other2, fanIn1]

-- TODO ∷ delete node coming in!
notExpand ∷
  (Aux2 s, InfoNetwork net Lang m) ⇒
  (Node, ProperPort) →
  (Node, s) →
  Bool →
  m Bool
notExpand (n, IsPrim {_tag0 = Tru}) (notNum, notPort) _ = do
  numFals <- newNode (Primar Fals)
  delNodes [n]
  propPrimary (notNum, notPort) numFals
  pure True
notExpand (n, IsPrim {_tag0 = Fals}) (notNum, notPort) _ = do
  numFals <- newNode (Primar Tru)
  delNodes [n]
  propPrimary (notNum, notPort) numFals
  pure True
notExpand _ _ updated = pure updated

-- Erase should be connected to the main port atm, change this logic later
-- when this isn't the case
eraseAll ∷
  (Aux3 s, InfoNetwork net Lang m) ⇒
  (s, Node) →
  Node →
  m ()
eraseAll (node, numNode) nodeErase = do
  (i, mE) <- auxDispatch (node ^. aux1) Aux1
  (i2, mE2) <- auxDispatch (node ^. aux2) Aux2
  (i3, mE3) <- auxDispatch (node ^. aux3) Aux3
  incGraphSizeStep (i + i2 + i3 - 2)
  deleteRewire [numNode, nodeErase] (catMaybes [mE, mE2, mE3])
  where
    auxDispatch FreeNode _ = pure (0, Nothing)
    auxDispatch (Auxiliary _) aux = (,) 1 <$> erase aux
    erase port = do
      numE <- newNode (Primar Erase)
      relink (numNode, port) (numE, Prim)
      return (Just numE)

consCar ∷
  InfoNetwork net Lang m ⇒
  Node →
  Node →
  m ()
consCar numCons numCar = do
  incGraphSizeStep (- 1)
  erase <- newNode (Primar Erase)
  relink (numCons, Aux1) (erase, Prim)
  rewire (numCons, Aux2) (numCar, Aux1)
  deleteRewire [numCons, numCar] [erase]

consCdr ∷
  InfoNetwork net Lang m ⇒
  Node →
  Node →
  m ()
consCdr numCons numCdr = do
  incGraphSizeStep (- 1)
  erase <- newNode (Primar Erase)
  relink (numCons, Aux2) (erase, Prim)
  rewire (numCons, Aux1) (numCdr, Aux1)
  deleteRewire [numCons, numCdr] [erase]

testNilNil ∷
  InfoNetwork net Lang m ⇒
  Node →
  Node →
  m ()
testNilNil numTest numNil = do
  incGraphSizeStep (- 1)
  true <- newNode (Primar Tru)
  relink (numTest, Aux1) (true, Prim)
  deleteRewire [numTest, numNil] [true]

testNilCons ∷
  InfoNetwork net Lang m ⇒
  Node →
  Node →
  m ()
testNilCons numCons numTest = do
  incGraphSizeStep 1
  erase1 <- newNode (Primar Erase)
  erase2 <- newNode (Primar Erase)
  false <- newNode (Primar Fals)
  traverse_
    (uncurry relink)
    [ ((numCons, Aux1), (erase1, Prim)),
      ((numCons, Aux2), (erase2, Prim)),
      ((numTest, Aux1), (false, Prim))
    ]
  deleteRewire [numCons, numTest] [erase1, erase2, false]

curry3 ∷
  InfoNetwork net Lang m ⇒
  (Primitive → Primitive → Primitive → Maybe Primitive, Node) →
  (Primitive, Node) →
  m ()
curry3 (nodeF, numNode) (p, numPrim) = do
  incGraphSizeStep (- 1)
  curr <- newNode (Auxiliary2 $ Curried2 $ nodeF p)
  let currNode = RELAuxiliary2
        { node = curr,
          primary = ReLink numNode Aux3,
          auxiliary1 = ReLink numNode Aux1,
          auxiliary2 = ReLink numNode Aux2
        }
  linkAll currNode
  deleteRewire [numNode, numPrim] [curr]

curry2 ∷
  InfoNetwork net Lang m ⇒
  (Primitive → Primitive → Maybe Primitive, Node) →
  (Primitive, Node) →
  m ()
curry2 (nodeF, numNode) (p, numPrim) = do
  incGraphSizeStep (- 1)
  curr <- newNode (Auxiliary1 $ Curried1 $ nodeF p)
  let currNode = RELAuxiliary1
        { node = curr,
          primary = ReLink numNode Aux2,
          auxiliary1 = ReLink numNode Aux1
        }
  linkAll currNode
  deleteRewire [numNode, numPrim] [curr]

-- The bool represents if the graph was updated
curry1 ∷
  InfoNetwork net Lang m ⇒
  (Primitive → Maybe Primitive, Node) →
  (Primitive, Node) →
  m Bool
curry1 (nodeF, numNode) (p, numPrim) =
  case nodeF p of
    Nothing → pure False
    Just x → do
      let node = case x of
            PInt i → IntLit i
            PBool True → Tru
            PBool False → Fals
      incGraphSizeStep (- 1)
      curr <- newNode (Primar node)
      let currNode = RELAuxiliary0
            { node = curr,
              primary = ReLink numNode Aux1
            }
      linkAll currNode
      deleteRewire [numNode, numPrim] [curr]
      pure $ True

fanIns ∷
  InfoNetwork net Lang m ⇒
  (Node, Int) →
  (Node, Int) →
  m ()
fanIns (numf1, lab1) (numf2, lab2)
  | lab1 == lab2 = do
    incGraphSizeStep (- 2)
    rewire (numf1, Aux1) (numf2, Aux2)
    rewire (numf1, Aux2) (numf2, Aux1)
    delNodes [numf1, numf2]
  | otherwise =
    fanInAux2 numf1 (numf2, (FanIn lab2)) lab1
