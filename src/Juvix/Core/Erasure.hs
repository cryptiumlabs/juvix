module Juvix.Core.Erasure (
  erase'
) where

import           Data.Map.Strict
import qualified Juvix.Core.MainLang as Core
import qualified Juvix.EAL.Types     as EAL
import           Juvix.Library       hiding (empty)
import           Juvix.Utility
import           Prelude             ((!!))

erase' ∷ Core.CTerm → (EAL.Term, EAL.TypeAssignment)
erase' cterm =
  let (term, env) = exec (erase cterm)
  in (term, typeAssignment env)

exec ∷ EnvErasure a → (a, Env)
exec (EnvEra env) = runState env (Env empty 0 [])

erase ∷ (HasState "typeAssignment" EAL.TypeAssignment m,
         HasState "nextName" Int m,
         HasState "nameStack" [Int] m)
  ⇒ Core.CTerm → m EAL.Term
erase term =
  case term of
    Core.Lam body -> do
      name <- newName
      let ty = EAL.SymT name
      -- TODO :: replace map here with unordered map
      -- the remove the Ord deriving from the Symbol type.
      modify @"typeAssignment" (insert name ty)
      body <- erase body
      pure (EAL.Lam name body)
    Core.Conv iterm -> do
      case iterm of
        Core.Bound n -> do
          name <- unDeBruijin (fromIntegral n)
          pure (EAL.Var name)
        Core.Free n  ->
          case n of
            Core.Global s -> pure (EAL.Var (intern s))
            Core.Local _s -> undefined
            Core.Quote _s -> undefined
        Core.App a b -> do
          a <- erase b
          b <- erase b
          pure (EAL.App a b)
        Core.Ann _ _ a -> do
          erase a
        Core.Nat _nat -> undefined
    _               -> undefined

unDeBruijin ∷ (HasState "nextName" Int m,
                HasState "nameStack" [Int] m)
 ⇒ Int → m Symbol
unDeBruijin ind = do
  stack <- get @"nameStack"
  pure (intern $ show $ stack !! ind)

newName ∷ (HasState "nextName" Int m,
           HasState "nameStack" [Int] m)
  ⇒ m Symbol
newName = do
  name <- get @"nextName"
  modify @"nextName" (+ 1)
  modify @"nameStack" ((:) name)
  return (intern (show name))

data Env = Env {
  typeAssignment :: EAL.TypeAssignment,
  nextName       :: Int,
  nameStack      :: [Int]
} deriving (Show, Eq, Generic)

newtype EnvErasure a = EnvEra (State Env a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "typeAssignment" EAL.TypeAssignment) via
    Field "typeAssignment" () (MonadState (State Env))
  deriving (HasState "nextName" Int) via
    Field "nextName" () (MonadState (State Env))
  deriving (HasState "nameStack" [Int]) via
    Field "nameStack" () (MonadState (State Env))
