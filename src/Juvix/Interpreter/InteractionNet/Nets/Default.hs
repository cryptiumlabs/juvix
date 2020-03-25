{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- - An evaluator for the Default Language
-- - Serves as a reference way of creating interaction nets
module Juvix.Interpreter.InteractionNet.Nets.Default where

import Control.Lens
import qualified Juvix.Interpreter.InteractionNet.Backends.Env as Env
import qualified Juvix.Interpreter.InteractionNet.Backends.Interface as Interface
import Juvix.Interpreter.InteractionNet.NodeInterface as NInterface
import qualified Juvix.Interpreter.InteractionNet.Shared as Shared
import Juvix.Library hiding (curry3, link, reduce)
import Prelude (Show (..))

data Lang primVal
  = Auxiliary3 (Auxiliary3 primVal)
  | Auxiliary2 (Auxiliary2 primVal)
  | Auxiliary1 (Auxiliary1 primVal)
  | Primar (Primar primVal)
  deriving (Show)

type Primitive = Shared.Primitive

-- the final argument for the curries is maybe, as if the nodes don't line up
-- a final type can't be constructed. This is untyped
-- so type check at a higher level
data Auxiliary3 primVal
  = IfElse
  | Curried3 (Primitive -> Primitive -> Primitive -> Maybe Primitive)
  deriving (Show)

data Auxiliary2 primVal
  = Or
  | And
  | Cons
  | Mu
  | Lambda
  | App
  | FanIn Int
  | PrimCurried2 (primVal -> primVal -> Maybe primVal)
  | Curried2 (Primitive -> Primitive -> Maybe Primitive)
  deriving (Show)

data Auxiliary1 primVal
  = Not
  | Car
  | Cdr
  | TestNil
  | PrimCurried1 (primVal -> Maybe primVal)
  | Curried1 (Primitive -> Maybe Primitive)
  deriving (Show)

data Primar primVal
  = Erase
  | Nil
  | Tru
  | Fals
  | IntLit Int
  | Symbol Symbol
  | PrimVal primVal
  deriving (Show)

data ProperPort primVal
  = IsAux3
      { _tag3 :: Auxiliary3 primVal,
        _prim :: NInterface.Primary,
        _aux1 :: NInterface.Auxiliary,
        _aux2 :: NInterface.Auxiliary,
        _aux3 :: NInterface.Auxiliary
      }
  | IsAux2
      { _tag2 :: Auxiliary2 primVal,
        _prim :: NInterface.Primary,
        _aux1 :: NInterface.Auxiliary,
        _aux2 :: NInterface.Auxiliary
      }
  | IsAux1
      { _tag1 :: Auxiliary1 primVal,
        _prim :: NInterface.Primary,
        _aux1 :: NInterface.Auxiliary
      }
  | IsPrim
      { _tag0 :: Primar primVal,
        _prim :: NInterface.Primary
      }
  deriving (Show)

makeFieldsNoPrefix ''ProperPort

-- Graph to more typed construction---------------------------------------------

-- Find a way to fix the ugliness!
langToProperPort ::
  (Interface.DifferentRep net, HasState "net" (net (Lang primVal)) m) =>
  Interface.Node ->
  m (Maybe (ProperPort primVal))
langToProperPort node = Interface.langToPort node (\l -> f l node)
  where
    f (Auxiliary3 a) = Interface.aux3FromGraph (IsAux3 a)
    f (Auxiliary2 a) = Interface.aux2FromGraph (IsAux2 a)
    f (Auxiliary1 a) = Interface.aux1FromGraph (IsAux1 a)
    f (Primar a) = Interface.aux0FromGraph (IsPrim a)

-- Rewrite rules----------------------------------------------------------------
reduceAll :: Env.InfoNetworkDiff net (Lang primVal) m => Int -> m ()
reduceAll = untilNothingNTimesM reduce

reduce :: Env.InfoNetworkDiff net (Lang primVal) m => m Bool
reduce = do
  nodes' <- Interface.nodes
  isChanged <- foldrM update False nodes'
  if isChanged
    then do
      modify@"info" (\c -> c {Env.parallelSteps = Env.parallelSteps c + 1})
      pure isChanged
    else pure isChanged
  where
    update n isChanged = do
      both <- Interface.isBothPrimary n
      if not both
        then pure isChanged
        else langToProperPort n >>= \case
          Nothing -> pure isChanged
          Just port -> do
            case port of
              IsAux3 tag (NInterface.Primary node) _ _ _ ->
                case tag of
                  IfElse ->
                    langToProperPort node >>= \case
                      Just IsPrim {_tag0 = Fals} -> True <$ ifElseRule node n False
                      Just IsPrim {_tag0 = Tru} -> True <$ ifElseRule node n True
                      _ -> pure isChanged
                  Curried3 f -> do
                    curryMatch curry3 (f, n) node isChanged
              IsAux2 tag (NInterface.Primary node) _ _ ->
                case tag of
                  And ->
                    langToProperPort node >>= \case
                      Just IsPrim {_tag0 = Fals} -> True <$ propPrimary (n, port) node
                      Just IsPrim {_tag0 = Tru} -> True <$ anihilateRewireAux node n
                      _ -> pure isChanged
                  Or ->
                    langToProperPort node >>= \case
                      Just IsPrim {_tag0 = Fals} -> True <$ anihilateRewireAux node n
                      Just IsPrim {_tag0 = Tru} -> True <$ propPrimary (n, port) node
                      _ -> pure isChanged
                  Cons ->
                    langToProperPort node >>= \case
                      Just IsAux1 {_tag1 = Car} -> True <$ consCdr n node
                      Just IsAux1 {_tag1 = Cdr} -> True <$ consCar n node
                      _ -> pure isChanged
                  App ->
                    langToProperPort node >>= \case
                      Just IsAux2 {_tag2 = Lambda} -> True <$ anihilateRewireAuxTogether node n
                      _ -> pure isChanged
                  FanIn level ->
                    langToProperPort node >>= \case
                      Just IsAux2 {_tag2 = FanIn lv2} -> True <$ fanIns (n, level) (node, lv2)
                      Just IsPrim {_tag0 = Symbol _} -> pure isChanged
                      Just IsPrim {_tag0} -> True <$ fanInAux0 n (node, _tag0)
                      Just IsAux1 {_tag1} -> True <$ fanInAux1 n (node, _tag1) level
                      Just IsAux2 {_tag2} -> True <$ fanInAux2 n (node, _tag2) level
                      Just IsAux3 {_tag3} -> True <$ fanInAux3 n (node, _tag3) level
                      Nothing -> pure isChanged
                  Curried2 f -> curryMatch curry2 (f, n) node isChanged
                  PrimCurried2 f -> curryMatchPrim curryPrim2 (f, n) node isChanged
                  -- Cases in which we fall through, and have the other node handle it
                  Mu -> pure isChanged
                  Lambda -> pure isChanged
              IsAux1 tag (NInterface.Primary node) _ ->
                case tag of
                  Not ->
                    langToProperPort node >>= \case
                      Nothing -> pure isChanged
                      Just x -> notExpand (node, x) (n, port) isChanged
                  Cdr ->
                    langToProperPort node >>= \case
                      Just IsPrim {_tag0 = Nil} -> True <$ propPrimary (n, port) node
                      _ -> pure isChanged
                  -- case is slightly different from other cases
                  -- just return what the function gives us!
                  -- TODO :: Unify logic with other cases
                  Curried1 f ->
                    langToProperPort node >>= \case
                      Just IsPrim {_tag0 = Fals} ->
                        curry1 (f, n) (Shared.PBool False, node)
                      Just IsPrim {_tag0 = Tru} ->
                        curry1 (f, n) (Shared.PBool True, node)
                      Just IsPrim {_tag0 = IntLit i} ->
                        curry1 (f, n) (Shared.PInt i, node)
                      _ -> pure isChanged
                  PrimCurried1 f ->
                    langToProperPort node >>= \case
                      Just IsPrim {_tag0 = PrimVal p} -> curryPrim1 (f, n) (p, node)
                      _ -> pure isChanged
                  -- Fall through cases
                  Car -> pure isChanged
                  TestNil -> pure isChanged
              IsPrim tag (NInterface.Primary node) ->
                case tag of
                  Erase ->
                    langToProperPort node >>= \case
                      Just x -> True <$ eraseAll (x, node) n
                      Nothing -> pure isChanged
                  -- Fall through cases
                  Nil -> pure isChanged
                  Tru -> pure isChanged
                  Fals -> pure isChanged
                  IntLit _ -> pure isChanged
                  Symbol _ -> pure isChanged
                  PrimVal _ -> pure isChanged
              -- Fall through cases
              IsAux3 _ NInterface.Free _ _ _ -> pure isChanged
              IsAux2 _ NInterface.Free _ _ -> pure isChanged
              IsAux1 _ NInterface.Free _ -> pure isChanged
              IsPrim _ NInterface.Free -> pure isChanged

curryMatch ::
  (Env.InfoNetworkDiff net (Lang primVal) m) =>
  (t -> (Primitive, Interface.Node) -> m b) ->
  t ->
  Interface.Node ->
  Bool ->
  m Bool
curryMatch curry currNodeInfo nodeConnected isChanged = do
  langToProperPort nodeConnected >>= \case
    Just IsPrim {_tag0 = Fals} ->
      True <$ curry currNodeInfo (Shared.PBool False, nodeConnected)
    Just IsPrim {_tag0 = Tru} ->
      True <$ curry currNodeInfo (Shared.PBool True, nodeConnected)
    Just IsPrim {_tag0 = IntLit i} ->
      True <$ curry currNodeInfo (Shared.PInt i, nodeConnected)
    _ -> pure isChanged

curryMatchPrim ::
  (Env.InfoNetworkDiff net (Lang primVal) m) =>
  (t -> (primVal, Interface.Node) -> m b) ->
  t ->
  Interface.Node ->
  Bool ->
  m Bool
curryMatchPrim curry currNodeInfo nodeConnected isChanged = do
  langToProperPort nodeConnected >>= \case
    Just IsPrim {_tag0 = PrimVal p} -> True <$ curry currNodeInfo (p, nodeConnected)
    _ -> pure isChanged

propPrimary ::
  (NInterface.Aux2 s, Env.InfoNetwork net (Lang primVal) m) =>
  (Interface.Node, s) ->
  Interface.Node ->
  m ()
propPrimary (numDel, nodeDel) numProp = do
  Interface.relink (numDel, Interface.Aux1) (numProp, Interface.Prim)
  case NInterface.auxToNode (nodeDel ^. NInterface.aux2) of
    Just _ -> do
      Env.sequentalStep
      eraseNum <- Interface.newNode (Primar Erase)
      Interface.relink (numDel, Interface.Aux2) (eraseNum, Interface.Prim)
      Interface.deleteRewire [numDel] [eraseNum]
    Nothing -> do
      Env.incGraphSizeStep (- 1)
      Interface.delNodes [numDel]

ifElseRule ::
  (Env.InfoNetwork net (Lang primVal) m) =>
  Interface.Node ->
  Interface.Node ->
  Bool ->
  m ()
ifElseRule numPrimOnly numAuxs pred = do
  Env.incGraphSizeStep (- 1)
  numErase <- Interface.newNode (Primar Erase)
  if pred
    then do
      Interface.relink (numAuxs, Interface.Aux2) (numErase, Interface.Prim)
      Interface.rewire (numAuxs, Interface.Aux1) (numAuxs, Interface.Aux3)
    else do
      Interface.relink (numAuxs, Interface.Aux3) (numErase, Interface.Prim)
      Interface.rewire (numAuxs, Interface.Aux1) (numAuxs, Interface.Aux2)
  Interface.deleteRewire [numPrimOnly, numAuxs] [numErase]

anihilateRewireAux ::
  (Env.InfoNetwork net (Lang primVal) m) =>
  Interface.Node ->
  Interface.Node ->
  m ()
anihilateRewireAux numPrimOnly numAuxs = do
  Env.incGraphSizeStep (- 2)
  Interface.rewire (numAuxs, Interface.Aux1) (numAuxs, Interface.Aux2)
  Interface.delNodes [numPrimOnly, numAuxs]

-- used for app lambda!
anihilateRewireAuxTogether ::
  (Env.InfoNetwork net (Lang primVal) m) =>
  Interface.Node ->
  Interface.Node ->
  m ()
anihilateRewireAuxTogether numNode1 numNode2 = do
  Env.incGraphSizeStep (- 2)
  Interface.rewire (numNode1, Interface.Aux1) (numNode2, Interface.Aux1)
  Interface.rewire (numNode1, Interface.Aux2) (numNode2, Interface.Aux2)
  Interface.delNodes [numNode1, numNode2]

-- o is Aux1 * is Aux2
muExpand ::
  (Env.InfoNetwork net (Lang primVal) m) =>
  Interface.Node ->
  m ()
muExpand muNum = do
  Env.incGraphSizeStep 2
  fanIn <- Interface.newNode (Auxiliary2 $ FanIn 0)
  fanOut <- Interface.newNode (Auxiliary2 $ FanIn 0)
  newMu <- Interface.newNode (Auxiliary2 $ FanIn 0)
  let nodeFanIn =
        Interface.RELAuxiliary2
          { Interface.node = fanIn,
            Interface.primary = Interface.ReLink muNum Interface.Aux1,
            Interface.auxiliary1 =
              Interface.Link (Interface.Port Interface.Aux1 newMu),
            Interface.auxiliary2 = Interface.ReLink muNum Interface.Prim
          }
      nodeFanOut =
        Interface.RELAuxiliary2
          { Interface.node = fanOut,
            Interface.primary = Interface.ReLink muNum Interface.Aux2,
            Interface.auxiliary1 =
              Interface.Link (Interface.Port Interface.Aux2 newMu),
            Interface.auxiliary2 =
              Interface.Link (Interface.Port Interface.Prim newMu)
          }
  traverse_ Interface.linkAll [nodeFanIn, nodeFanOut]
  Interface.deleteRewire [muNum] [fanIn, fanOut, newMu]

fanInAux0 ::
  (Env.InfoNetwork net (Lang primVal) m) =>
  Interface.Node ->
  (Interface.Node, Primar primVal) ->
  m ()
fanInAux0 numFan (numOther, otherLang) = do
  Env.sequentalStep
  other1 <- Interface.newNode (Primar otherLang)
  other2 <- Interface.newNode (Primar otherLang)
  let nodeOther1 =
        Interface.RELAuxiliary0
          { Interface.node = other1,
            Interface.primary = Interface.ReLink numFan Interface.Aux1
          }
      nodeOther2 =
        Interface.RELAuxiliary0
          { Interface.node = other2,
            Interface.primary = Interface.ReLink numFan Interface.Aux2
          }
  traverse_ Interface.linkAll [nodeOther1, nodeOther2]
  Interface.deleteRewire [numFan, numOther] [other1, other2]

fanInAux1 ::
  (Env.InfoNetwork net (Lang primVal) m) =>
  Interface.Node ->
  (Interface.Node, Auxiliary1 primVal) ->
  Int ->
  m ()
fanInAux1 numFan (numOther, otherLang) level = do
  Env.incGraphSizeStep 1
  other1 <- Interface.newNode (Auxiliary1 otherLang)
  other2 <- Interface.newNode (Auxiliary1 otherLang)
  fanIn1 <- Interface.newNode (Auxiliary2 (FanIn level))
  let nodeOther1 =
        Interface.RELAuxiliary1
          { Interface.node = other1,
            Interface.primary = Interface.ReLink numFan Interface.Aux1,
            Interface.auxiliary1 =
              Interface.Link (Interface.Port Interface.Aux1 fanIn1)
          }
      nodeOther2 =
        Interface.RELAuxiliary1
          { Interface.node = other2,
            Interface.primary = Interface.ReLink numFan Interface.Aux2,
            Interface.auxiliary1 =
              Interface.Link (Interface.Port Interface.Aux2 fanIn1)
          }
      nodeFan1 =
        Interface.RELAuxiliary2
          { Interface.node = fanIn1,
            Interface.primary = Interface.ReLink numOther Interface.Aux1,
            Interface.auxiliary1 =
              Interface.Link (Interface.Port Interface.Aux1 other1),
            Interface.auxiliary2 =
              Interface.Link (Interface.Port Interface.Aux1 other2)
          }
  traverse_ Interface.linkAll [nodeOther1, nodeOther2, nodeFan1]
  Interface.deleteRewire [numFan, numOther] [other1, other2, fanIn1]

fanInAux2 ::
  (Env.InfoNetwork net (Lang primVal) m) =>
  Interface.Node ->
  (Interface.Node, Auxiliary2 primVal) ->
  Int ->
  m ()
fanInAux2 numFan (numOther, otherLang) level = do
  Env.incGraphSizeStep 2
  other1 <- Interface.newNode (Auxiliary2 otherLang)
  other2 <- Interface.newNode (Auxiliary2 otherLang)
  fanIn1 <- Interface.newNode (Auxiliary2 (FanIn level))
  fanIn2 <- Interface.newNode (Auxiliary2 (FanIn level))
  let nodeOther1 =
        Interface.RELAuxiliary2
          { Interface.node = other1,
            Interface.primary = Interface.ReLink numFan Interface.Aux1,
            Interface.auxiliary1 =
              Interface.Link (Interface.Port Interface.Aux1 fanIn2),
            Interface.auxiliary2 =
              Interface.Link (Interface.Port Interface.Aux1 fanIn1)
          }
      nodeOther2 =
        Interface.RELAuxiliary2
          { Interface.node = other2,
            Interface.primary = Interface.ReLink numFan Interface.Aux2,
            Interface.auxiliary1 =
              Interface.Link (Interface.Port Interface.Aux2 fanIn2),
            Interface.auxiliary2 =
              Interface.Link (Interface.Port Interface.Aux2 fanIn1)
          }
      nodeFan1 =
        Interface.RELAuxiliary2
          { Interface.node = fanIn1,
            Interface.primary = Interface.ReLink numOther Interface.Aux2,
            Interface.auxiliary1 =
              Interface.Link (Interface.Port Interface.Aux2 other1),
            Interface.auxiliary2 =
              Interface.Link (Interface.Port Interface.Aux2 other2)
          }
      nodeFan2 =
        Interface.RELAuxiliary2
          { Interface.node = fanIn2,
            Interface.primary = Interface.ReLink numOther Interface.Aux1,
            Interface.auxiliary1 =
              Interface.Link (Interface.Port Interface.Aux1 other1),
            Interface.auxiliary2 =
              Interface.Link (Interface.Port Interface.Aux1 other2)
          }
  traverse_ Interface.linkAll [nodeOther1, nodeOther2, nodeFan1, nodeFan2]
  Interface.deleteRewire [numFan, numOther] [other1, other2, fanIn1, fanIn2]

fanInAux3 ::
  (Env.InfoNetwork net (Lang primVal) m) =>
  Interface.Node ->
  (Interface.Node, Auxiliary3 primVal) ->
  Int ->
  m ()
fanInAux3 numFan (numOther, otherLang) level = do
  Env.incGraphSizeStep 3
  other1 <- Interface.newNode (Auxiliary3 otherLang)
  other2 <- Interface.newNode (Auxiliary3 otherLang)
  fanIn1 <- Interface.newNode (Auxiliary2 (FanIn level))
  fanIn2 <- Interface.newNode (Auxiliary2 (FanIn level))
  fanIn3 <- Interface.newNode (Auxiliary2 (FanIn level))
  let nodeOther1 =
        Interface.RELAuxiliary3
          { Interface.node = other1,
            Interface.primary = Interface.ReLink numFan Interface.Aux1,
            Interface.auxiliary1 =
              Interface.Link (Interface.Port Interface.Aux1 fanIn1),
            Interface.auxiliary2 =
              Interface.Link (Interface.Port Interface.Aux1 fanIn2),
            Interface.auxiliary3 =
              Interface.Link (Interface.Port Interface.Aux1 fanIn3)
          }
      nodeOther2 =
        Interface.RELAuxiliary3
          { Interface.node = other2,
            Interface.primary = Interface.ReLink numFan Interface.Aux2,
            Interface.auxiliary1 =
              Interface.Link (Interface.Port Interface.Aux2 fanIn1),
            Interface.auxiliary2 =
              Interface.Link (Interface.Port Interface.Aux2 fanIn2),
            Interface.auxiliary3 =
              Interface.Link (Interface.Port Interface.Aux2 fanIn3)
          }
      nodeFan1 =
        Interface.RELAuxiliary2
          { Interface.node = fanIn1,
            Interface.primary = Interface.ReLink numOther Interface.Aux1,
            Interface.auxiliary1 =
              Interface.Link (Interface.Port Interface.Aux1 other1),
            Interface.auxiliary2 =
              Interface.Link (Interface.Port Interface.Aux1 other2)
          }
      nodeFan2 =
        Interface.RELAuxiliary2
          { Interface.node = fanIn2,
            Interface.primary = Interface.ReLink numOther Interface.Aux2,
            Interface.auxiliary1 =
              Interface.Link (Interface.Port Interface.Aux2 other1),
            Interface.auxiliary2 =
              Interface.Link (Interface.Port Interface.Aux2 other2)
          }
      nodeFan3 =
        Interface.RELAuxiliary2
          { Interface.node = fanIn3,
            Interface.primary = Interface.ReLink numOther Interface.Aux3,
            Interface.auxiliary1 =
              Interface.Link (Interface.Port Interface.Aux3 other1),
            Interface.auxiliary2 =
              Interface.Link (Interface.Port Interface.Aux3 other2)
          }
  traverse_ Interface.linkAll [nodeOther1, nodeOther2, nodeFan1, nodeFan2, nodeFan3]
  Interface.deleteRewire [numFan, numOther] [other1, other2, fanIn1, fanIn2, fanIn3]

-- TODO :: delete node coming in!
notExpand ::
  (NInterface.Aux2 s, Env.InfoNetwork net (Lang primVal) m) =>
  (Interface.Node, ProperPort primVal) ->
  (Interface.Node, s) ->
  Bool ->
  m Bool
notExpand (n, IsPrim {_tag0 = Tru}) (notNum, notPort) _ = do
  numFals <- Interface.newNode (Primar Fals)
  Interface.delNodes [n]
  propPrimary (notNum, notPort) numFals
  pure True
notExpand (n, IsPrim {_tag0 = Fals}) (notNum, notPort) _ = do
  numFals <- Interface.newNode (Primar Tru)
  Interface.delNodes [n]
  propPrimary (notNum, notPort) numFals
  pure True
notExpand _ _ updated = pure updated

-- Erase should be connected to the main port atm, change this logic later
-- when this isn't the case
eraseAll ::
  (NInterface.Aux3 s, Env.InfoNetwork net (Lang primVal) m) =>
  (s, Interface.Node) ->
  Interface.Node ->
  m ()
eraseAll (node, numNode) nodeErase = do
  (i, mE) <- auxDispatch (node ^. NInterface.aux1) Interface.Aux1
  (i2, mE2) <- auxDispatch (node ^. NInterface.aux2) Interface.Aux2
  (i3, mE3) <- auxDispatch (node ^. NInterface.aux3) Interface.Aux3
  Env.incGraphSizeStep (i + i2 + i3 - 2)
  Interface.deleteRewire [numNode, nodeErase] (catMaybes [mE, mE2, mE3])
  where
    auxDispatch NInterface.FreeNode _ = pure (0, Nothing)
    auxDispatch (NInterface.Auxiliary _) aux = (,) 1 <$> erase aux
    erase port = do
      numE <- Interface.newNode (Primar Erase)
      Interface.relink (numNode, port) (numE, Interface.Prim)
      return (Just numE)

consCar ::
  Env.InfoNetwork net (Lang primVal) m =>
  Interface.Node ->
  Interface.Node ->
  m ()
consCar numCons numCar = do
  Env.incGraphSizeStep (- 1)
  erase <- Interface.newNode (Primar Erase)
  Interface.relink (numCons, Interface.Aux1) (erase, Interface.Prim)
  Interface.rewire (numCons, Interface.Aux2) (numCar, Interface.Aux1)
  Interface.deleteRewire [numCons, numCar] [erase]

consCdr ::
  Env.InfoNetwork net (Lang primVal) m =>
  Interface.Node ->
  Interface.Node ->
  m ()
consCdr numCons numCdr = do
  Env.incGraphSizeStep (- 1)
  erase <- Interface.newNode (Primar Erase)
  Interface.relink (numCons, Interface.Aux2) (erase, Interface.Prim)
  Interface.rewire (numCons, Interface.Aux1) (numCdr, Interface.Aux1)
  Interface.deleteRewire [numCons, numCdr] [erase]

testNilNil ::
  Env.InfoNetwork net (Lang primVal) m =>
  Interface.Node ->
  Interface.Node ->
  m ()
testNilNil numTest numNil = do
  Env.incGraphSizeStep (- 1)
  true <- Interface.newNode (Primar Tru)
  Interface.relink (numTest, Interface.Aux1) (true, Interface.Prim)
  Interface.deleteRewire [numTest, numNil] [true]

testNilCons ::
  Env.InfoNetwork net (Lang primVal) m =>
  Interface.Node ->
  Interface.Node ->
  m ()
testNilCons numCons numTest = do
  Env.incGraphSizeStep 1
  erase1 <- Interface.newNode (Primar Erase)
  erase2 <- Interface.newNode (Primar Erase)
  false <- Interface.newNode (Primar Fals)
  traverse_
    (uncurry Interface.relink)
    [ ((numCons, Interface.Aux1), (erase1, Interface.Prim)),
      ((numCons, Interface.Aux2), (erase2, Interface.Prim)),
      ((numTest, Interface.Aux1), (false, Interface.Prim))
    ]
  Interface.deleteRewire [numCons, numTest] [erase1, erase2, false]

curry3 ::
  Env.InfoNetwork net (Lang primVal) m =>
  (Primitive -> Primitive -> Primitive -> Maybe Primitive, Interface.Node) ->
  (Primitive, Interface.Node) ->
  m ()
curry3 (nodeF, numNode) (p, numPrim) = do
  Env.incGraphSizeStep (- 1)
  curr <- Interface.newNode (Auxiliary2 $ Curried2 $ nodeF p)
  let currNode =
        Interface.RELAuxiliary2
          { Interface.node = curr,
            Interface.primary = Interface.ReLink numNode Interface.Aux3,
            Interface.auxiliary1 = Interface.ReLink numNode Interface.Aux1,
            Interface.auxiliary2 = Interface.ReLink numNode Interface.Aux2
          }
  Interface.linkAll currNode
  Interface.deleteRewire [numNode, numPrim] [curr]

curry2 ::
  Env.InfoNetwork net (Lang primVal) m =>
  (Primitive -> Primitive -> Maybe Primitive, Interface.Node) ->
  (Primitive, Interface.Node) ->
  m ()
curry2 (nodeF, numNode) (p, numPrim) = do
  Env.incGraphSizeStep (- 1)
  curr <- Interface.newNode (Auxiliary1 $ Curried1 $ nodeF p)
  let currNode =
        Interface.RELAuxiliary1
          { Interface.node = curr,
            Interface.primary = Interface.ReLink numNode Interface.Aux2,
            Interface.auxiliary1 = Interface.ReLink numNode Interface.Aux1
          }
  Interface.linkAll currNode
  Interface.deleteRewire [numNode, numPrim] [curr]

curryPrim2 ::
  Env.InfoNetwork net (Lang primVal) m =>
  (primVal -> primVal -> Maybe primVal, Interface.Node) ->
  (primVal, Interface.Node) ->
  m ()
curryPrim2 (nodeF, numNode) (p, numPrim) = do
  Env.incGraphSizeStep (- 1)
  curr <- Interface.newNode (Auxiliary1 $ PrimCurried1 $ nodeF p)
  let currNode =
        Interface.RELAuxiliary1
          { Interface.node = curr,
            Interface.primary = Interface.ReLink numNode Interface.Aux2,
            Interface.auxiliary1 = Interface.ReLink numNode Interface.Aux1
          }
  Interface.linkAll currNode
  Interface.deleteRewire [numNode, numPrim] [curr]

-- The bool represents if the graph was updated
curry1 ::
  Env.InfoNetwork net (Lang primVal) m =>
  (Primitive -> Maybe Primitive, Interface.Node) ->
  (Primitive, Interface.Node) ->
  m Bool
curry1 (nodeF, numNode) (p, numPrim) =
  case nodeF p of
    Nothing -> pure False
    Just x -> do
      let node = case x of
            Shared.PInt i -> IntLit i
            Shared.PBool True -> Tru
            Shared.PBool False -> Fals
      Env.incGraphSizeStep (- 1)
      curr <- Interface.newNode (Primar node)
      let currNode =
            Interface.RELAuxiliary0
              { Interface.node = curr,
                Interface.primary = Interface.ReLink numNode Interface.Aux1
              }
      Interface.linkAll currNode
      Interface.deleteRewire [numNode, numPrim] [curr]
      pure $ True

curryPrim1 ::
  Env.InfoNetwork net (Lang primVal) m =>
  (primVal -> Maybe primVal, Interface.Node) ->
  (primVal, Interface.Node) ->
  m Bool
curryPrim1 (nodeF, numNode) (p, numPrim) =
  case nodeF p of
    Nothing -> pure False
    Just x -> do
      let node = PrimVal x
      Env.incGraphSizeStep (- 1)
      curr <- Interface.newNode (Primar node)
      let currNode =
            Interface.RELAuxiliary0
              { Interface.node = curr,
                Interface.primary = Interface.ReLink numNode Interface.Aux1
              }
      Interface.linkAll currNode
      Interface.deleteRewire [numNode, numPrim] [curr]
      pure $ True

fanIns ::
  Env.InfoNetwork net (Lang primVal) m =>
  (Interface.Node, Int) ->
  (Interface.Node, Int) ->
  m ()
fanIns (numf1, lab1) (numf2, lab2)
  | lab1 == lab2 = do
    Env.incGraphSizeStep (- 2)
    Interface.rewire (numf1, Interface.Aux1) (numf2, Interface.Aux2)
    Interface.rewire (numf1, Interface.Aux2) (numf2, Interface.Aux1)
    Interface.delNodes [numf1, numf2]
  | otherwise =
    fanInAux2 numf1 (numf2, (FanIn lab2)) lab1
