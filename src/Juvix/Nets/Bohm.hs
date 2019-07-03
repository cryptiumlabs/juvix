{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Juvix.Nets.Bohm where

import           Control.Lens
import           Protolude

import           Juvix.Interaction
import           Juvix.NodeInterface

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
 | FanIn   {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 | FanOut  {_prim :: Primary, _aux1 :: Auxiliary, _aux2 :: Auxiliary}
 deriving Show

makeFieldsNoPrefix ''ProperPort

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
 | FanIn'
 | FanOut'
 deriving Show

-- Graph to more typed construction---------------------------------------------

-- Find a way to fix the ugliness!
langToProperPort :: Net Lang -> Node -> Maybe ProperPort
langToProperPort net node = langToPort net node f
  where
    f Or'      = aux2FromGraph Or      net node
    f And'     = aux2FromGraph And     net node
    f Not'     = aux1FromGraph Not     net node
    f Meq'     = aux2FromGraph Meq     net node
    f Eq'      = aux2FromGraph Eq      net node
    f Neq'     = aux2FromGraph Neq     net node
    f More'    = aux2FromGraph More    net node
    f Less'    = aux2FromGraph Less    net node
    f Cons'    = aux2FromGraph Cons    net node
    f Car'     = aux1FromGraph Car     net node
    f Cdr'     = aux1FromGraph Cdr     net node
    f Nil'     = aux0FromGraph Nil     net node
    f TestNil' = aux1FromGraph TestNil net node
    f IfElse'  = aux3FromGraph IfElse  net node
    f Mu'      = aux2FromGraph Mu      net node
    f Div'     = aux2FromGraph Div     net node
    f Sub'     = aux2FromGraph Sub     net node
    f Add'     = aux2FromGraph Add     net node
    f Prod'    = aux2FromGraph Prod    net node
    f Mod'     = aux2FromGraph Mod     net node
    f Tru'     = aux0FromGraph Tru     net node
    f Fals'    = aux0FromGraph Fals    net node
    f Leq'     = aux2FromGraph Leq     net node
    f Lambda'  = aux2FromGraph Lambda  net node
    f App'     = aux2FromGraph App     net node
    f FanIn'   = aux2FromGraph FanIn   net node
    f FanOut'  = aux2FromGraph FanOut  net node
    f (IntLit' i) = aux0FromGraph (\x -> IntLit x i) net node
    f (Symbol' s) = aux0FromGraph (\x -> Symbol x s) net node
-- Rewrite rules----------------------------------------------------------------
reduce net = update undefined
  where
    netNodes = nodes net
    updated c = (c, True)
    update n (net, isChanged)
      | isBothPrimary net n = return (net, isChanged)
      | otherwise =
        case langToProperPort net n of
          Nothing → pure (net, isChanged)
          Just port →
            case port of
              And (Primary node) _ _ →
                case langToProperPort net node of
                  Just Fals {} → updated <$> propPrimary net (n, port) node
                  Just Tru  {} → updated <$> anihilateRewireAux net node (n, port)
                  _            → pure (net, isChanged)
              Lambda (Primary node) _ _ →
                case langToProperPort net node of
                  Just app@(App {}) → updated <$> anihilateRewireAuxTogether net (n, port) (node, app)
                  Just FanIn {}     → updated <$> fanInAux2 net node (n, Lambda')
                  _                 → pure (net, isChanged)
              App (Primary node) _ _ →
                case langToProperPort net node of
                  Just lam@(App {}) → updated <$> anihilateRewireAuxTogether net (n, port) (node, lam)
                  Just FanIn {}     → updated <$> fanInAux2 net node (n, App')
                  _                 → pure (net, isChanged)
              FanIn (Primary node) _ _ →
                case langToProperPort net node of
                  Just App {}    → updated <$> fanInAux2 net n (node, App')
                  Just Lambda {} → updated <$> fanInAux2 net n (node, Lambda')
                  -- fan in should interact with all!, update later
                  _              → pure (net, isChanged)
              _ → pure (net, isChanged)

propPrimary ::
  (MonadState StateInfo m, Aux2 s) ⇒ Net a → (Node, s) → Node → m (Net a)
propPrimary net (numDel, nodeDel) numProp =
  let wire = rewire net (Aux1, nodeDel^.aux1) (Prim, Auxiliary numProp) in
  case auxToNode (nodeDel^.aux2) of
    Just n -> do
      incGraphSizeStep (-2)
      return $ delNodes [numDel, n] $ wire
    Nothing -> do
      incGraphSizeStep (-1)
      return $ delNodes [numDel] $ wire

anihilateRewireAux ::
  (MonadState StateInfo m, Aux2 s) ⇒ Net a → Node → (Node, s) → m (Net a)
anihilateRewireAux net numPrimOnly (numAuxs, auxs) = do
  incGraphSizeStep (-2)
  return $ delNodes [numPrimOnly, numAuxs]
         $ rewire net (Aux1, auxs^.aux1) (Aux2, auxs^.aux2)


-- used for app lambda!
anihilateRewireAuxTogether ::
  (MonadState StateInfo m, Aux2 s) ⇒ Net a → (Node, s) → (Node, s) → m (Net a)
anihilateRewireAuxTogether net (numNode1, node1) (numNode2, node2) = do
  incGraphSizeStep (-2)
  return $ delNodes [numNode1, numNode2]
         $ rewire (rewire net (Aux1, node1^.aux1) (Aux1, node2^.aux1))
                  (Aux2, node1^.aux2)
                  (Aux2, node2^.aux2)

-- o is Aux1 * is Aux2
muExpand :: MonadState StateInfo m ⇒ Net Lang → Node → m (Net Lang)
muExpand net muNum = do
  incGraphSizeStep 2
  return $ deleteRewire [muNum] [fanIn, fanOut, newMu]
         $ foldr (flip linkAll)
                 net'''
                 [nodeFanIn, nodeFanOut]
  where
    (fanIn, net')   = newNode net FanIn'
    (fanOut, net'') = newNode net' FanOut'
    (newMu, net''') = newNode net'' FanOut'
    nodeFanIn = RELAuxiliary2 { node       = fanIn
                              , primary    = ReLink muNum Aux1
                              , auxiliary1 = Link (Port Aux1 newMu)
                              , auxiliary2 = ReLink muNum Prim
                              }
    nodeFanOut = RELAuxiliary2 { node       = fanOut
                               , primary    = ReLink muNum Aux2
                               , auxiliary1 = Link (Port Aux2 newMu)
                               , auxiliary2 = Link (Port Prim newMu)
                               }




fanInAux2 :: MonadState StateInfo m ⇒ Net Lang → Node → (Node, Lang) → m (Net Lang)
fanInAux2 net numFan (numOther, otherLang) = do
  incGraphSizeStep 2
  return $ deleteRewire [numFan, numOther] [other1, other2, fanIn1, fanIn2]
         $ foldr (flip linkAll)
                 net''''
                 [nodeOther1, nodeOther2, nodeFan1, nodeFan2]
  where
    (other1, net')    = newNode net    otherLang
    (other2, net'')   = newNode net'   otherLang
    (fanIn1, net''')  = newNode net''  FanIn'
    (fanIn2, net'''') = newNode net''' FanIn'
    nodeOther1 = RELAuxiliary2 { node       = other1
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
