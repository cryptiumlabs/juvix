module Juvix.Core.Erasure where

import           Data.Map.Strict
import qualified Juvix.Core.MainLang as Core
import qualified Juvix.EAL.Types     as EAL
import           Juvix.Library       hiding (empty)
import           Juvix.Utility

erase' ∷ Core.CTerm → (EAL.Term, Env)
erase' = exec . erase

exec ∷ EnvErasure a → (a, Env)
exec (EnvEra env) = runState env (Env empty 0)

erase ∷ (HasState "typeAssignment" EAL.TypeAssignment m,
         HasState "nextName" Int m)
  ⇒ Core.CTerm → m EAL.Term
erase term =
  case term of
    Core.Lam _ body -> undefined
    _               -> undefined

newName ∷ (HasState "nextName" Int m)
  ⇒ m SomeSymbol
newName = do
  name <- get @"nextName"
  modify @"nextName" (+ 1)
  return (someSymbolVal (show name))

data Env = Env {
  typeAssignment :: EAL.TypeAssignment,
  nextName       :: Int
} deriving (Show, Eq, Generic)

newtype EnvErasure a = EnvEra (State Env a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "typeAssignment" EAL.TypeAssignment) via
    Field "typeAssignment" () (MonadState (State Env))
  deriving (HasState "nextName" Int) via
    Field "nextName" () (MonadState (State Env))
