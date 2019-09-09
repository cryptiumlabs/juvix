{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Juvix.Backends.Env where

import qualified Data.Map.Strict as Map

import           Juvix.Backends.Interface
import           Juvix.Library


-- | Info Stores diagnostic data on how much memory a particular graph reduction uses
data Info = Info { memoryAllocated  :: Integer
                 , sequentalSteps   :: Integer
                 , parallelSteps    :: Integer
                 , biggestGraphSize :: Integer
                 , currentGraphSize :: Integer
                 } deriving Show

-- | Constructs a Function from a primitive
-- the final argument is maybe, as if the nodes don't line up
-- a final type can't be constructed. This is untyped
-- so type check at a higher level
data Fn prim = Arg0 prim
             | Arg1 (prim → Maybe prim)
             | Arg2 (prim → prim → Maybe prim)
             | Arg3 (prim → prim → prim → Maybe prim)
             deriving (Show, Generic)

data InfoNet net prim = InfoNet { net :: net
                                , info :: Info
                                , fnMap :: Map.Map SomeSymbol (Fn prim)
                                } deriving (Show, Generic)

-- TODO :: make generic type to remove repeat here!
-- TODO :: replace HasState with HasReader
type InfoNetwork     net prim a m = ( HasState  "info"  Info      m
                                    , HasState  "net"   (net a)   m
                                    , HasState "fnMap" (Map.Map SomeSymbol (Fn prim)) m
                                    , Network net
                                    )
type InfoNetworkDiff net prim a m = ( HasState "info" Info m
                                    , HasState "net" (net a) m
                                    , HasState "fnMap" (Map.Map SomeSymbol (Fn prim)) m
                                    , DifferentRep net
                                    )

-- TODO :: replace HasState with HasReader
newtype EnvNetInfo net prim a = EnvI (State (InfoNet net prim) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "info" Info) via
    Field "info" () (MonadState (State (InfoNet net prim)))
  deriving (HasState "net" net) via
    Field "net" () (MonadState (State (InfoNet net prim)))
  deriving (HasState "fnMap" (Map.Map SomeSymbol (Fn prim))) via
    Field "fnMap" () (MonadState (State (InfoNet net prim)))

newtype EnvNetInfoIO net prim a = EnvIO (StateT (InfoNet net prim) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving (HasState "info" Info) via
    Field "info" () (MonadState (StateT (InfoNet net prim) IO))
  deriving (HasState "net" net) via
    Field "net" () (MonadState (StateT (InfoNet net prim) IO))
  deriving (HasState"fnMap" (Map.Map SomeSymbol (Fn prim))) via
    Field "fnMap" () (MonadState (StateT (InfoNet net prim) IO))

execInfoNet ∷ EnvNetInfo net prim a → InfoNet net prim → (InfoNet net prim)
execInfoNet (EnvI m) = execState m

runInfoNet ∷ EnvNetInfo net prim a → InfoNet net prim → (a, InfoNet net prim)
runInfoNet (EnvI m) = runState m

runNetEmpt ∷ EnvNetInfo net prim a → net → Integer → InfoNet net prim
runNetEmpt f net size = execInfoNet f (InfoNet net (Info size 0 0 size size) Map.empty)

runNet ∷ EnvNetInfo net prim a → net → Integer → InfoNet net prim
runNet f net size = execInfoNet f (InfoNet net (Info size 0 0 size size) Map.empty)

runNet' ∷ EnvNetInfo net prim a → net → Integer → (a, InfoNet net prim)
runNet' f net size = runInfoNet f (InfoNet net (Info size 0 0 size  size) Map.empty)

runInfoNetIO ∷ EnvNetInfoIO net prim a → InfoNet net prim → IO (InfoNet net prim)
runInfoNetIO (EnvIO m) = execStateT m

runNetIO ∷ EnvNetInfoIO net prim a → net → Integer → IO (InfoNet net prim)
runNetIO f net size = runInfoNetIO f (InfoNet net (Info size 0 0 size  size) Map.empty)

sequentalStep ∷ HasState "info" Info m ⇒ m ()
sequentalStep = modify' @"info" (\c → c {sequentalSteps = sequentalSteps c + 1})

incGraphSizeStep ∷ HasState "info" Info m ⇒ Integer → m ()
incGraphSizeStep n = do
  Info memAlloced seqStep parallelSteps largestGraph currGraph ← get @"info"
  let memoryAllocated | n > 0 = memAlloced + n
                      | otherwise = memAlloced
      currentGraphSize = n + currGraph
      biggestGraphSize = max currentGraphSize largestGraph
      sequentalSteps = succ seqStep
  put @"info" Info { memoryAllocated, currentGraphSize
                   , biggestGraphSize, sequentalSteps, parallelSteps }
