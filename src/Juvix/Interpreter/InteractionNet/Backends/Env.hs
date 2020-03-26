{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- - Env serves as the environment for running the back-ends
--   + This includes the diagnostic information about how the code is ran
--   + Also includes the effect handler types and the runner functions
--     for them
-- - Also gives functions needed to increment diagnostic information
--   + see =incGraphStepSize= and =sequentalStep=
--   + File will also be imported in [[Nets]] due to these two functions
module Juvix.Interpreter.InteractionNet.Backends.Env where

import qualified Juvix.Interpreter.InteractionNet.Backends.Interface as Interface
import Juvix.Library

-- | Info Stores diagnostic data on how much memory
-- a particular graph reduction uses
data Info
  = Info
      { memoryAllocated :: Integer,
        sequentalSteps :: Integer,
        parallelSteps :: Integer,
        biggestGraphSize :: Integer,
        currentGraphSize :: Integer
      }
  deriving (Show)

data InfoNet net
  = InfoNet
      { net :: net,
        info :: Info
      }
  deriving (Show, Generic)

-- TODO ∷ make generic type to remove repeat here!
type InfoNetwork net a m =
  ( HasState "info" Info m,
    HasState "net" (net a) m,
    Interface.Network net
  )

type InfoNetworkDiff net a m =
  ( HasState "info" Info m,
    HasState "net" (net a) m,
    Interface.DifferentRep net
  )

newtype EnvNetInfo net a = EnvI (State (InfoNet net) a)
  deriving (Functor, Applicative, Monad)
  deriving
    (HasState "info" Info)
    via Field "info" () (MonadState (State (InfoNet net)))
  deriving
    (HasState "net" net)
    via Field "net" () (MonadState (State (InfoNet net)))

newtype EnvNetInfoIO net a = EnvIO (StateT (InfoNet net) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving
    (HasState "info" Info)
    via Field "info" () (MonadState (StateT (InfoNet net) IO))
  deriving
    (HasState "net" net)
    via Field "net" () (MonadState (StateT (InfoNet net) IO))

execInfoNet :: EnvNetInfo net a -> InfoNet net -> InfoNet net
execInfoNet (EnvI m) = execState m

runInfoNet :: EnvNetInfo net a -> InfoNet net -> (a, InfoNet net)
runInfoNet (EnvI m) = runState m

runNet :: EnvNetInfo net a -> net -> Integer -> InfoNet net
runNet f net size = execInfoNet f (InfoNet net (Info size 0 0 size size))

runNet' :: EnvNetInfo net a -> net -> Integer -> (a, InfoNet net)
runNet' f net size = runInfoNet f (InfoNet net (Info size 0 0 size size))

runInfoNetIO :: EnvNetInfoIO net a -> InfoNet net -> IO (InfoNet net)
runInfoNetIO (EnvIO m) = execStateT m

runNetIO :: EnvNetInfoIO net a -> net -> Integer -> IO (InfoNet net)
runNetIO f net size = runInfoNetIO f (InfoNet net (Info size 0 0 size size))

sequentalStep :: HasState "info" Info m => m ()
sequentalStep = modify' @"info" (\c -> c {sequentalSteps = sequentalSteps c + 1})

incGraphSizeStep :: HasState "info" Info m => Integer -> m ()
incGraphSizeStep n = do
  Info memAlloced seqStep parallelSteps largestGraph currGraph <- get @"info"
  let memoryAllocated
        | n > 0 = memAlloced + n
        | otherwise = memAlloced
      currentGraphSize = n + currGraph
      biggestGraphSize = max currentGraphSize largestGraph
      sequentalSteps = succ seqStep
  put @"info"
    Info
      { memoryAllocated,
        currentGraphSize,
        biggestGraphSize,
        sequentalSteps,
        parallelSteps
      }
