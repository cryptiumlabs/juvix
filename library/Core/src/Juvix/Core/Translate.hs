module Juvix.Core.Translate where

import qualified Juvix.Core.HR.Types as HR
import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.Utility
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

-- contract: no shadowing
-- TODO - handle this automatically by renaming shadowed vars
hrToIR :: HR.Term primTy primVal -> IR.Term primTy primVal
hrToIR = fst . exec . hrToIR'

hrToIR' ::
  (HasState "symbolStack" [NameSymbol.T] m) =>
  HR.Term primTy primVal ->
  m (IR.Term primTy primVal)
hrToIR' term =
  case term of
    HR.Star n -> pure (IR.Star n)
    HR.PrimTy p -> pure (IR.PrimTy p)
    HR.Prim p -> pure (IR.Prim p)
    HR.Pi u n a b -> do
      a <- hrToIR' a
      b <- withName n $ hrToIR' b
      pure (IR.Pi u a b)
    HR.Lam n b -> do
      b <- withName n $ hrToIR' b
      pure (IR.Lam b)
    HR.Sig π n a b -> do
      a <- hrToIR' a
      b <- withName n $ hrToIR' b
      pure (IR.Sig π a b)
    HR.Pair s t -> do
      HR.Pair <$> hrToIR' s <*> hrToIR' t
    HR.UnitTy -> pure IR.UnitTy
    HR.Unit -> pure IR.Unit
    HR.Let π n l b -> do
      l <- hrElimToIR' l
      b <- withName n $ hrToIR' b
      pure (IR.Let π l b)
    HR.Elim e -> IR.Elim |<< hrElimToIR' e

hrElimToIR' ::
  (HasState "symbolStack" [NameSymbol.T] m) =>
  HR.Elim primTy primVal ->
  m (IR.Elim primTy primVal)
hrElimToIR' elim =
  case elim of
    HR.Var n -> do
      maybeIndex <- lookupName n
      pure $ case maybeIndex of
        Just ind -> IR.Bound (fromIntegral ind)
        Nothing -> IR.Free (IR.Global n)
    HR.App f x -> do
      f <- hrElimToIR' f
      x <- hrToIR' x
      pure (IR.App f x)
    HR.Ann u t x l -> do
      t <- hrToIR' t
      x <- hrToIR' x
      pure (IR.Ann u t x l)

irToHR :: IR.Term primTy primVal -> HR.Term primTy primVal
irToHR = fst . exec . irToHR'

irToHR' ::
  ( HasState "nextName" Int m,
    HasState "nameStack" [Int] m
  ) =>
  IR.Term primTy primVal ->
  m (HR.Term primTy primVal)
irToHR' term =
  case term of
    IR.Star n -> pure (HR.Star n)
    IR.PrimTy p -> pure (HR.PrimTy p)
    IR.Prim p -> pure (HR.Prim p)
    IR.Pi u a b -> do
      a <- irToHR' a
      n <- newName
      b <- irToHR' b
      pure (HR.Pi u n a b)
    IR.Lam t -> do
      n <- newName
      t <- irToHR' t
      pure (HR.Lam n t)
    IR.Sig π a b -> do
      a <- irToHR' a
      n <- newName
      b <- irToHR' b
      pure $ HR.Sig π n a b
    IR.Pair s t -> do
      HR.Pair <$> irToHR' s <*> irToHR' t
    IR.UnitTy -> pure HR.UnitTy
    IR.Unit -> pure HR.Unit
    IR.Let π l b -> do
      l <- irElimToHR' l
      n <- newName
      b <- irToHR' b
      pure (HR.Let π n l b)
    IR.Elim e -> HR.Elim |<< irElimToHR' e

irElimToHR' ::
  ( HasState "nextName" Int m,
    HasState "nameStack" [Int] m
  ) =>
  IR.Elim primTy primVal ->
  m (HR.Elim primTy primVal)
irElimToHR' elim =
  case elim of
    IR.Free (IR.Global n) -> pure $ HR.Var n
    IR.Free (IR.Pattern p) ->
      pure $ HR.Var $ NameSymbol.fromSymbol $ intern $ "pat" <> show p
    IR.Bound i -> do
      v <- unDeBruijn (fromIntegral i)
      pure (HR.Var v)
    IR.App f x -> do
      f <- irElimToHR' f
      x <- irToHR' x
      pure (HR.App f x)
    IR.Ann u t x l -> do
      t <- irToHR' t
      x <- irToHR' x
      pure (HR.Ann u t x l)

exec :: EnvElim a -> (a, Env)
exec (EnvCon env) = runState env (Env 0 [] [])

data Env = Env
  { nextName :: Int,
    nameStack :: [Int],
    symbolStack :: [NameSymbol.T]
  }
  deriving (Show, Eq, Generic)

newtype EnvElim a = EnvCon (State Env a)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "nextName" Int,
      HasSink "nextName" Int,
      HasSource "nextName" Int
    )
    via StateField "nextName" (State Env)
  deriving
    ( HasState "nameStack" [Int],
      HasSink "nameStack" [Int],
      HasSource "nameStack" [Int]
    )
    via StateField "nameStack" (State Env)
  deriving
    ( HasState "symbolStack" [NameSymbol.T],
      HasSink "symbolStack" [NameSymbol.T],
      HasSource "symbolStack" [NameSymbol.T]
    )
    via StateField "symbolStack" (State Env)
