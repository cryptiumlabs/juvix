module Juvix.Core.Erasure
  ( erase',
  )
where

import qualified Juvix.Core.MainLang as Core
import qualified Juvix.EAC.Types as EAC
import Juvix.Library hiding (empty)
import Juvix.Utility
import Juvix.Utility.HashMap
import Prelude ((!!))

erase' ∷ Core.CTerm → (EAC.Term, EAC.TypeAssignment)

erase' cterm =
  let (term, env) = exec (erase cterm)
   in (term, typeAssignment env)

exec ∷ EnvErasure a → (a, Env)

exec (EnvEra env) = runState env (Env empty 0 [])

erase
  ∷ ( HasState "typeAssignment" EAC.TypeAssignment m,
      HasState "nextName" Int m,
      HasState "nameStack" [Int] m
    )
  ⇒ Core.CTerm → m EAC.Term

erase term =
  case term of
    Core.Lam body -> do
      name <- newName
      -- TODO: Instead calculate type of this lambda-bound variable.
      let ty = EAC.SymT name
      -- TODO :: replace map here with unordered map
      -- then remove the Ord deriving from the Symbol type.
      stk <- get@"nameStack"
      modify@"typeAssignment" (insert name ty)
      body <- erase body
      pure (EAC.Lam name body)
    Core.Conv iterm -> do
      case iterm of
        Core.Bound n -> do
          name <- unDeBruijin (fromIntegral n)
          pure (EAC.Var name)
        Core.Free n ->
          case n of
            Core.Global s -> pure (EAC.Var (intern s))
            Core.Local _s -> undefined
            Core.Quote _s -> undefined
        Core.App a b -> do
          a <- erase (Core.Conv a)
          b <- erase b
          pure (EAC.App a b)
        Core.Ann _ _ a -> do
          erase a
        Core.Nat n -> pure (EAC.Prim (EAC.Nat n))
    _ -> undefined

unDeBruijin
  ∷ ( HasState "nextName" Int m,
      HasState "nameStack" [Int] m
    )
  ⇒ Int → m Symbol

unDeBruijin ind = do
  stack <- get@"nameStack"
  pure (intern $ show $ stack !! ind)

newName
  ∷ ( HasState "nextName" Int m,
      HasState "nameStack" [Int] m
    )
  ⇒ m Symbol

newName = do
  name <- get@"nextName"
  modify@"nextName" (+ 1)
  modify@"nameStack" ((:) name)
  return (intern (show name))

data Env
  = Env
      { typeAssignment :: EAC.TypeAssignment,
        nextName :: Int,
        nameStack :: [Int]
      }
  deriving (Show, Eq, Generic)

newtype EnvErasure a = EnvEra (State Env a)
  deriving (Functor, Applicative, Monad)
  deriving
    (HasState "typeAssignment" EAC.TypeAssignment)
    via Field "typeAssignment" () (MonadState (State Env))
  deriving
    (HasState "nextName" Int)
    via Field "nextName" () (MonadState (State Env))
  deriving
    (HasState "nameStack" [Int])
    via Field "nameStack" () (MonadState (State Env))
