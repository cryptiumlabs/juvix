{-# LANGUAGE NamedFieldPuns #-}

module Juvix.Bohm.Translation(astToNet, netToAst) where

import qualified Data.Map.Strict      as Map
-- eventually abstract out this import
import qualified Data.Graph.Inductive as G

import           Juvix.Library        hiding (link)
import           Juvix.Interaction
import qualified Juvix.Nets.Bohm      as B
import qualified Juvix.Bohm.Type      as BT

data Env = Env {level :: Int, net' :: Net B.Lang}
         deriving (Show, Generic)

newtype EnvState a = EnvS (State Env a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "level" Int) via
    Rename "level" (Field "level" () (MonadState (State Env)))
  deriving (HasState "net" (Net B.Lang)) via
    Rename "net'" (Field "net'" () (MonadState (State Env)))

execEnvState :: EnvState a -> Env -> Env
execEnvState (EnvS m) e = execState m e

astToNet :: BT.Bohm → Net B.Lang
astToNet bohm = net'
  where
    Env {net'} = execEnvState (recursive bohm Map.empty) (Env 0 G.empty)
    -- we return the port which the node above it in the AST connects to!
    recursive (BT.IntLit x) _context = (,) <$> newNode (B.IntLit' x) <*> pure Prim
    recursive BT.False'     _context = (,) <$> newNode B.Fals'       <*> pure Prim
    recursive BT.True'      _context = (,) <$> newNode B.Tru'        <*> pure Prim
    recursive BT.Nil        _context = (,) <$> newNode B.Nil'        <*> pure Prim
    recursive (BT.Car b)     context = genericAux1PrimArg b B.Car'     context
    recursive (BT.Cdr b)     context = genericAux1PrimArg b B.Cdr'     context
    recursive (BT.IsNil b)   context = genericAux1PrimArg b B.TestNil' context
    recursive (BT.Not b)     context = genericAux1PrimArg b B.Not'     context
    recursive (BT.Application b1 b2)    c = genericAux2PrimArg b1 b2 B.App'  c
    recursive (BT.Infix' BT.Mult b1 b2) c = genericAux2PrimArg b1 b2 B.Prod' c
    recursive (BT.Infix' BT.Plus b1 b2) c = genericAux2PrimArg b1 b2 B.Add'  c
    recursive (BT.Infix' BT.Sub b1 b2)  c = genericAux2PrimArg b1 b2 B.Sub'  c
    recursive (BT.Infix' BT.Mod b1 b2)  c = genericAux2PrimArg b1 b2 B.Mod'  c
    recursive (BT.Infix' BT.Or b1 b2)   c = genericAux2PrimArg b1 b2 B.Or'   c
    recursive (BT.Infix' BT.And b1 b2)  c = genericAux2PrimArg b1 b2 B.And'  c
    recursive (BT.Infix' BT.Eq b1 b2)   c = genericAux2PrimArg b1 b2 B.Eq'   c
    recursive (BT.Infix' BT.Neq b1 b2)  c = genericAux2PrimArg b1 b2 B.Neq'  c
    recursive (BT.Infix' BT.Lt b1 b2)   c = genericAux2PrimArg b1 b2 B.Less' c
    recursive (BT.Infix' BT.Gt b1 b2)   c = genericAux2PrimArg b1 b2 B.More' c
    recursive (BT.Infix' BT.Ge b1 b2)   c = genericAux2PrimArg b1 b2 B.Meq'  c
    recursive (BT.Infix' BT.Le b1 b2)   c = genericAux2PrimArg b1 b2 B.Leq'  c
    recursive (BT.Infix' BT.Division b1 b2) c = genericAux2PrimArg b1 b2 B.Div' c
    recursive (BT.Cons b1 b2) c = genericAux2 (b1, Aux2) (b2, Aux1) (B.Cons', Prim) c
    recursive (BT.Symbol' s) context =
      case context Map.!? s of
        -- The symbol is bound, and thus we have a port and its number
        Just portInfo → chaseAndCreateFan portInfo
        -- The symbol is Free, just stash it in a symbol with no rewrite rules
        -- Note, that multiple instances of this symbol will create multiple
        -- Different symbols instead of sharing, as it is unbound, we can't do
        -- any processing anyways
        Nothing → (,) <$> (newNode (B.Symbol' s)) <*> pure Prim
    recursive (BT.Lambda s body) context = do
      numLam          ← newNode B.Lambda'
      (bNode,  bPort) ← recursive body (Map.insert s (numLam, Aux2) context)
      link (numLam, Aux1) (bNode, bPort)
      pure (numLam, Prim)
    recursive (BT.Let sym bound body) context = do
      (numBound, portBound) ← recursive bound context
      recursive body (Map.insert sym (numBound, portBound) context)
    recursive (BT.Letrec sym body) context = do
      numMu          ← newNode B.Mu'
      (bNode, bPort) ← recursive body (Map.insert sym (numMu, Aux2) context)
      link (numMu, Aux1) (bNode, bPort)
      pure (numMu, Prim)
    recursive (BT.If b1 b2 b3) c = do
      (numIf, retPort) ← genericAux2 (b1, Prim) (b2, Aux3) (B.IfElse', Aux1) c
      (b3Num, b3Port)  ← recursive b3 c
      link (numIf, Aux2) (b3Num, b3Port)
      pure (numIf, retPort)
    genericAux1 (b1, pb1) (langToCreate, portToReturn) context = do
      numCar        ← newNode langToCreate
      (bNum, bPort) ← recursive b1 context
      link (bNum, bPort) (numCar, pb1)
      pure (numCar, portToReturn)
    genericAux2 (b1, pb1) (b2, pb2) retInfo context = do
      (numApp, retPort) ← genericAux1 (b1, pb1) retInfo context
      (b2Num, b2Port)   ← recursive b2 context
      link (numApp, pb2) (b2Num, b2Port)
      pure (numApp, retPort)
    genericAux2PrimArg b1 b2 lc        = genericAux2 (b1, Prim) (b2, Aux2) (lc, Aux1)
    genericAux1PrimArg b1 langToCreate = genericAux1 (b1, Prim) (langToCreate, Aux1)

netToAst :: Net B.Lang → BT.Bohm
netToAst = undefined

-- Helper Functions-------------------------------------------------------------

-- | Creates a fan if the port is taken, and prepares to be connected
chaseAndCreateFan :: (HasState "level" Int m, HasState "net" (Net B.Lang) m)
                  ⇒ (Node, PortType)
                  → m (Node, PortType)
chaseAndCreateFan (num,port) = do
  net ← get @"net"
  lev ← get @"level"
  case findEdge net num port of
    Nothing → pure (num, port)
    Just t1@(nConnected, connectedPort) → do
      put @"level" (succ lev)
      numFan ← newNode (B.FanIn' lev)
      let nodeFan = RELAuxiliary2 { node       = numFan
                                  , primary    = Link (Port port          num)
                                  , auxiliary1 = Link (Port connectedPort nConnected)
                                  , auxiliary2 = Link FreePort
                                  }
      linkAll nodeFan
      deleteEdge t1 (num,port)
      pure (numFan, Aux2)
