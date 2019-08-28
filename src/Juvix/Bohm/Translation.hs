{-# LANGUAGE NamedFieldPuns #-}

module Juvix.Bohm.Translation(astToNet, netToAst) where

import           Data.List                ((!!))
import qualified Data.Map.Strict          as Map
import           Prelude                  (error)

import           Juvix.Backends.Interface
import qualified Juvix.Bohm.Type          as BT
import           Juvix.Library            hiding (empty, link)
import qualified Juvix.Nets.Bohm          as B
import           Juvix.NodeInterface

data Env net = Env {level :: Int
                   , net' :: net B.Lang
                   , free :: Map SomeSymbol (Node, PortType)
                   } deriving (Generic)

newtype EnvState net a = EnvS (State (Env net) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "level" Int) via
    Rename "level" (Field "level" () (MonadState (State (Env net))))
  deriving (HasState "free" (Map SomeSymbol (Node, PortType))) via
    (Field "free" () (MonadState (State (Env net))))
  deriving (HasState "net" (net B.Lang)) via
    Rename "net'" (Field "net'" () (MonadState (State (Env net))))

execEnvState ∷ Network net ⇒ EnvState net a → Env net → Env net
execEnvState (EnvS m) = execState m

evalEnvState ∷ Network net ⇒ EnvState net a → Env net → a
evalEnvState (EnvS m) = evalState m

astToNet ∷ Network net ⇒ BT.Bohm → net B.Lang
astToNet bohm = net'
  where
    Env {net'} = execEnvState (recursive bohm Map.empty) (Env 0 empty mempty)
    -- we return the port which the node above it in the AST connects to!
    recursive (BT.IntLit x) _context = (,) <$> newNode (B.Primar $ B.IntLit x) <*> pure Prim
    recursive BT.False'     _context = (,) <$> newNode (B.Primar B.Fals)       <*> pure Prim
    recursive BT.True'      _context = (,) <$> newNode (B.Primar B.Tru)        <*> pure Prim
    recursive BT.Nil        _context = (,) <$> newNode (B.Primar B.Nil)        <*> pure Prim
    recursive (BT.Erase)    _context = (,) <$> newNode (B.Primar B.Erase)      <*> pure Prim
    recursive (BT.Car b)     context    = genericAux1PrimArg b (B.Auxiliary1 B.Car)     context
    recursive (BT.Cdr b)     context    = genericAux1PrimArg b (B.Auxiliary1 B.Cdr)     context
    recursive (BT.IsNil b)   context    = genericAux1PrimArg b (B.Auxiliary1 B.TestNil) context
    recursive (BT.Not b)     context    = genericAux1PrimArg b (B.Auxiliary1 B.Not)     context
    recursive (BT.Curried f b) context  = genericAux1PrimArg b (B.Auxiliary1 $ B.Curried f) context
    recursive (BT.CurriedB f b) context = genericAux1PrimArg b (B.Auxiliary1 $ B.CurriedB f) context
    recursive (BT.Infix' BT.Mult b1 b2) c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.Infix B.Prod)  c
    recursive (BT.Infix' BT.Plus b1 b2) c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.Infix B.Add)   c
    recursive (BT.Infix' BT.Sub b1 b2)  c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.Infix B.Sub)   c
    recursive (BT.Infix' BT.Mod b1 b2)  c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.Infix B.Mod)   c
    recursive (BT.Infix' BT.Eq b1 b2)   c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.InfixB B.Eq)   c
    recursive (BT.Infix' BT.Neq b1 b2)  c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.InfixB B.Neq)  c
    recursive (BT.Infix' BT.Lt b1 b2)   c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.InfixB B.Less) c
    recursive (BT.Infix' BT.Gt b1 b2)   c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.InfixB B.More) c
    recursive (BT.Infix' BT.Ge b1 b2)   c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.InfixB B.Meq)  c
    recursive (BT.Infix' BT.Le b1 b2)   c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.InfixB B.Leq)  c
    recursive (BT.Infix' BT.Division b1 b2) c = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.Infix B.Div)   c
    recursive (BT.Application b1 b2)    c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 B.App) c
    recursive (BT.Infix' BT.Or b1 b2)   c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 B.Or)  c
    recursive (BT.Infix' BT.And b1 b2)  c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 B.And) c
    recursive (BT.Cons b1 b2)           c     = genericAux2 (b1, Aux2) (b2, Aux1) (B.Auxiliary2 B.Cons, Prim) c
    recursive (BT.Symbol' s) context = do
      frees ← get @"free"
      case (context Map.!? s, frees Map.!? s) of
        -- The symbol is bound, and thus we have a port and its number
        (Just portInfo, _) → chaseAndCreateFan portInfo
        -- The symbol is Free, but used already, create a sharing node for it
        (Nothing, Just portInfo) → chaseAndCreateFan portInfo
        -- The symbol is Free, just stash it in a symbol with no rewrite rules
        (Nothing, Nothing) → do
          nodeInfo ← (,) <$> (newNode (B.Primar $ B.Symbol s)) <*> pure Prim
          put @"free" (Map.insert s nodeInfo frees)
          pure nodeInfo
    recursive (BT.Letrec sym body) context = do
      numMu          ← newNode (B.Auxiliary2 B.Mu)
      (bNode, bPort) ← recursive body (Map.insert sym (numMu, Aux2) context)
      link (numMu, Aux1) (bNode, bPort)
      pure (numMu, Prim)
    recursive (BT.Lambda s body) context = do
      numLam          ← newNode (B.Auxiliary2 B.Lambda)
      (bNode,  bPort) ← recursive body (Map.insert s (numLam, Aux2) context)
      link (numLam, Aux1) (bNode, bPort)
      aux2Filled ← findEdge (numLam, Aux2)
      case aux2Filled of
        Nothing → do
          numErase ← newNode (B.Primar B.Erase)
          link (numLam, Aux2) (numErase, Prim)
        Just _  → pure ()
      pure (numLam, Prim)
    recursive (BT.Let sym bound body) context = do
      (numBound, portBound) ← recursive bound context
      recursive body (Map.insert sym (numBound, portBound) context)
    recursive (BT.If b1 b2 b3) c = do
      (numIf, retPort) ← genericAux2 (b1, Prim) (b2, Aux3) (B.Auxiliary3 B.IfElse, Aux1) c
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

data FanPorts = Circle | Star deriving Show

fanPortsToAux ∷ FanPorts → PortType
fanPortsToAux Circle = Aux1
fanPortsToAux Star   = Aux2

auxToFanPorts ∷ PortType → FanPorts
auxToFanPorts Aux1 = Circle
auxToFanPorts Aux2 = Star
auxToFanPorts _    = error " only send an Aux1 or Aux2 to auxToFanPorts"

data FanStatus = In FanPorts
               | Completed FanPorts
               deriving Show

netToAst ∷ DifferentRep net ⇒ net B.Lang → Maybe BT.Bohm
netToAst net = evalEnvState run (Env 0 net mempty)
  where
    run = do
      -- rec' assumes that we are given a statement at the top of the graphical AST
      let rec' n comeFrom fanMap nodeVarMap = do
            port ← B.langToProperPort n
            case port of
              Nothing → pure Nothing
              Just port →
                case port of
                  B.IsAux3 {B._tag3 = tag, B._prim = prim, B._aux2 = aux2, B._aux3 = aux3} →
                    case tag of
                      B.IfElse → do
                        case (prim, aux2, aux3) of
                          (Primary p, Auxiliary a2, Auxiliary a3) → do
                            p  ← rec' p  (Just (n, Prim)) fanMap nodeVarMap
                            a2 ← rec' a2 (Just (n, Aux2)) fanMap nodeVarMap
                            a3 ← rec' a3 (Just (n, Aux3)) fanMap nodeVarMap
                            pure (BT.If <$> p <*> a2 <*> a3)
                          _ → pure Nothing
                  B.IsAux2 {B._tag2 = tag, B._prim = prim, B._aux1 = aux1, B._aux2 = aux2} →
                    let parentAux1 con = do
                          case (prim, aux2) of
                            (Primary p, Auxiliary a2) → do
                              p  ← rec' p  (Just (n, Prim)) fanMap nodeVarMap
                              a2 ← rec' a2 (Just (n, Aux2)) fanMap nodeVarMap
                              pure (con <$> p <*> a2)
                            _ → pure Nothing
                        parentPrim con = do
                          case (aux1, aux2) of
                            (Auxiliary a1, Auxiliary a2) → do
                              a1 ← rec' a1 (Just (n, Aux1)) fanMap nodeVarMap
                              a2 ← rec' a2 (Just (n, Aux2)) fanMap nodeVarMap
                              pure (con <$> a1 <*> a2)
                            _ → pure Nothing
                        -- Case for Lambda and mu
                        lamMu lamOrMu = do
                          let fullLamCase lamOrMu =
                                let num           = newMapNum nodeVarMap
                                    symb          = numToSymbol num
                                    newNodeVarMap = Map.insert n symb nodeVarMap
                                in case aux1 of
                                  Auxiliary a1 → do
                                    a1 ← rec' a1 (Just (n, Aux1)) fanMap newNodeVarMap
                                    pure (lamOrMu symb <$> a1)
                                  FreeNode → pure Nothing
                          mEdge ← traverseM findEdge comeFrom
                          case mEdge of
                            -- We are pointing to the symbol of this lambda
                            Just (_, Aux2) →
                              -- The symbol has to be here, or else, there is an issue
                              -- in the AST!
                              pure (Just (BT.Symbol' (nodeVarMap Map.! n)))
                            -- We must be starting with Lambda, thus make a full lambda!
                            Nothing → fullLamCase lamOrMu
                            -- This case it has to Prim, so construct a full lambda
                            _ → fullLamCase lamOrMu
                    in case tag of
                      B.Infix b  → parentAux1 (BT.Infix' (inFixMap b))
                      B.InfixB b → parentAux1 (BT.Infix' (inFixMapB b))
                      B.Or       → parentAux1 (BT.Infix' BT.Or)
                      B.And      → parentAux1 (BT.Infix' BT.And)
                      B.App      → parentAux1 BT.Application
                      B.Cons     → parentPrim BT.Cons
                      -- Lambda may be the lambda node
                      -- Or the symbol the Lambda contains
                      B.Lambda   → lamMu BT.Lambda
                      -- same with mu
                      B.Mu       → lamMu BT.Letrec
                      B.FanIn i  → do
                        -- First we are going to look up if we came from the fan
                        -- in of this fan out note that we don't cover the impossible
                        -- case where we enter through Aux1 in both the same fan in/out
                        case fanMap Map.!? i of
                          -- we haven't visited or completed a fan in
                          -- Check if we came through the main port or Circle or Star
                          Nothing → do
                            mPort ← traverseM findEdge comeFrom
                            let freeChoice =
                                  let newFanMap = Map.insert i [In Circle] fanMap
                                      cameFrom  = (Just (n, fanPortsToAux Circle))
                                  in case aux1 of
                                    Auxiliary aux1 →
                                      rec' aux1 cameFrom newFanMap nodeVarMap
                                    -- Never happens by precondition!
                                    FreeNode → pure Nothing
                                fromCircleOrStar con =
                                  let newFanMap = Map.insert i [In con] fanMap
                                      cameFrom = Just (n, Prim)
                                  in case prim of
                                    Primary prim →
                                      rec' prim cameFrom newFanMap nodeVarMap
                                    _ → pure Nothing -- doesn't happen!
                            case mPort of
                              -- We are starting at a FanIn or the graph is invalid!
                              -- So pick the circle direction to go with!
                              Nothing        → freeChoice
                              Just (_, Prim) → freeChoice              -- from fan-In, pick a direction to go!
                              Just (_, Aux2) → fromCircleOrStar Star   -- from ★, mark it; go through prim!
                              Just (_, Aux1) → fromCircleOrStar Circle -- from ●, mark it; go through prim!
                              -- This case should never happen
                              _              → pure Nothing
                          -- We have been in a FanIn Before, figure out what state of the world we are in!
                          Just status → do
                            mPort ← traverseM findEdge comeFrom
                            let through con =
                                  let newFanMap = Map.insert i (In con : status) fanMap
                                      cameFrom  = Just (n, Prim)
                                  in case prim of
                                    Primary prim →
                                      rec' prim cameFrom newFanMap nodeVarMap
                                    Free → pure Nothing -- never happens
                            case mPort of
                              -- Shouldn't happen, as the previous node *must* exist
                              Nothing        → pure Nothing
                              Just (_, Aux1) → through (auxToFanPorts Aux1)
                              Just (_, Aux2) → through (auxToFanPorts Aux2)
                              Just (_, Aux3) → pure Nothing -- Shouldn't happen!
                              Just (_, Aux4) → pure Nothing -- Shouldn't happen!
                              Just (_, Aux5) → pure Nothing -- Shouldn't happen!
                              Just (_, Prim) → do
                                case status of
                                  In port : xs →
                                    -- adding the completed at the end keeps the precondition that In's are in front
                                    let newFanMap = Map.insert i (xs <> [Completed port]) fanMap
                                        cameFrom  = (Just (n, fanPortsToAux port))
                                        aux Circle = aux1
                                        aux Star   = aux2
                                    in case aux port of
                                      Auxiliary aux → rec' aux cameFrom newFanMap nodeVarMap
                                      FreeNode      → pure Nothing -- doesn't happen
                                  -- We have already completed a port, but have not yet gone through another
                                  -- so go through the other port
                                  [Completed port] → do
                                    let newFanMap Star   = Map.insert i ([In Circle, Completed port]) fanMap
                                        newFanMap Circle = Map.insert i ([In Star, Completed port]) fanMap
                                        cameFrom       = (Just (n, fanPortsToAux port))
                                        auxFlip Circle = aux2
                                        auxFlip Star   = aux1
                                    case auxFlip port of
                                      Auxiliary aux → rec' aux cameFrom (newFanMap port) nodeVarMap
                                      FreeNode      → pure Nothing -- doesn't happen
                                  [Completed Star, Completed Circle] → pure Nothing -- going back through both ports, odd!
                                  [Completed Circle, Completed Star] → pure Nothing -- going back through both ports, odd!
                                  [Completed _, Completed _]         → error "going through the same node twice!?!?"
                                  []                                 → error "doesn't happen"
                                  _ : _                              → error "doesn't happen"
                  B.IsAux1 {B._tag1 = tag, B._prim = prim} →
                    let parentAux con =
                          case prim of
                            Primary prim → do
                              prim ← rec' prim (Just (n, Prim)) fanMap nodeVarMap
                              pure (con <$> prim)
                            Free → pure Nothing -- doesn't happen
                    in case tag of
                      B.Not        → parentAux BT.Not
                      B.Cdr        → parentAux BT.Cdr
                      B.Car        → parentAux BT.Car
                      B.TestNil    → parentAux BT.IsNil
                      B.Curried f  → parentAux (BT.Curried f)
                      B.CurriedB f → parentAux (BT.CurriedB f)
                  B.IsPrim {B._tag0 = tag} →
                    pure $ Just $
                      case tag of
                        B.Erase    → BT.Erase
                        B.Nil      → BT.Nil
                        B.Tru      → BT.True'
                        B.Fals     → BT.False'
                        B.IntLit i → BT.IntLit i
                        B.Symbol s → BT.Symbol' s
          isFree (B.IsAux3 _ (Primary _) (Auxiliary _) (Auxiliary _) (Auxiliary _)) = False
          isFree (B.IsAux2 _ (Primary _) (Auxiliary _) (Auxiliary _))               = False
          isFree (B.IsAux1 _ (Primary _) (Auxiliary _))                             = False
          isFree (B.IsPrim _ (Primary _))                                           = False
          isFree B.IsPrim {} = True
          isFree B.IsAux1 {} = True
          isFree B.IsAux2 {} = True
          isFree B.IsAux3 {} = True
      nodes ← nodes
      frees ← filterM (\n → do
                          n ← B.langToProperPort n
                          case n of
                            Nothing → pure False
                            Just x  → pure (isFree x))
                      nodes
      case frees of
        -- This case should only happen when the code is in a irreducible state
        []    → pure Nothing
        -- This case should happen when the graph is properly typed
        [n]   → rec' n Nothing Map.empty Map.empty
        -- This case should only happen if the graph is incomplete or has multiple
        -- dijoint sets of nodes in the graph
        _ : _ → pure Nothing

-- Helper Functions-------------------------------------------------------------

-- | Creates a fan if the port is taken, and prepares to be connected
chaseAndCreateFan ∷ (Network net, HasState "level" Int m, HasState "net" (net B.Lang) m)
                  ⇒ (Node, PortType)
                  → m (Node, PortType)
chaseAndCreateFan (num,port) = do
  lev  ← get @"level"
  edge ← findEdge (num, port)
  case edge of
    Nothing → pure (num, port)
    Just t1@(nConnected, connectedPort) → do
      put @"level" (succ lev)
      numFan ← newNode (B.Auxiliary2 $ B.FanIn lev)
      let nodeFan = RELAuxiliary2 { node       = numFan
                                  , primary    = Link (Port port          num)
                                  , auxiliary1 = Link (Port connectedPort nConnected)
                                  , auxiliary2 = Link FreePort
                                  }
      linkAll nodeFan
      deleteEdge t1 (num,port)
      pure (numFan, Aux2)

inFixMap ∷ B.Infix → BT.Op
inFixMap B.Prod = BT.Mult
inFixMap B.Add  = BT.Plus
inFixMap B.Sub  = BT.Sub
inFixMap B.Mod  = BT.Mod
inFixMap B.Div  = BT.Division

inFixMapB ∷ B.InfixB → BT.Op
inFixMapB B.Eq   = BT.Eq
inFixMapB B.Neq  = BT.Neq
inFixMapB B.Less = BT.Lt
inFixMapB B.More = BT.Gt
inFixMapB B.Meq  = BT.Ge
inFixMapB B.Leq  = BT.Le

-- | numToSymbol generates a symbol from a number
numToSymbol ∷ Int → SomeSymbol
numToSymbol x
  | x < 26    = someSymbolVal (return ((['x'..'z'] <> ['a'..'w']) !! x))
  | otherwise = someSymbolVal ("%gen" <> show x)


-- | generate a new number based on the map size
-- Only gives an unique if the key has an isomorphism with the size, and no data is deleted.
newMapNum ∷ Map k a → Int
newMapNum = Map.size
