{-# LANGUAGE OverloadedLists #-}

module Juvix.Core.Translate
  ( hrToIR,
    hrToIRWith,
    irToHR,
    irToHRWith,
    hrPatternToIR,
    hrPatternToIRWith,
    irPatternToHR,
    irPatternToHRWith,
  )
where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.IntMap.Strict as PM
import qualified Juvix.Core.HR.Types as HR
import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.Utility
import Juvix.Library hiding (filter)
import qualified Juvix.Library.NameSymbol as NameSymbol

hrToIR :: HR.Term primTy primVal -> IR.Term primTy primVal
hrToIR = hrToIRWith mempty

-- contract: no shadowing
-- TODO - handle this automatically by renaming shadowed vars
hrToIRWith ::
  -- | pattern var <-> name mapping from outer scopes
  IR.PatternMap NameSymbol.T ->
  HR.Term primTy primVal ->
  IR.Term primTy primVal
hrToIRWith pats = fst . exec pats mempty . hrToIR'

hrToIR' ::
  HasNameStack m =>
  HR.Term primTy primVal ->
  m (IR.Term primTy primVal)
hrToIR' = \case
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
  HasNameStack m =>
  HR.Elim primTy primVal ->
  m (IR.Elim primTy primVal)
hrElimToIR' = \case
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
irToHR = irToHRWith mempty

irToHRWith ::
  -- | pattern var <-> name mapping from outer scopes
  IR.PatternMap NameSymbol.T ->
  IR.Term primTy primVal ->
  HR.Term primTy primVal
irToHRWith pats t = fst $ exec pats (varsTerm t) $ irToHR' t

irToHR' ::
  (HasNames m, HasPatToSym m) =>
  IR.Term primTy primVal ->
  m (HR.Term primTy primVal)
irToHR' = \case
  IR.Star n -> pure (HR.Star n)
  IR.PrimTy p -> pure (HR.PrimTy p)
  IR.Prim p -> pure (HR.Prim p)
  IR.Pi u a b -> do
    a <- irToHR' a
    withFresh \n -> do
      b <- irToHR' b
      pure (HR.Pi u n a b)
  IR.Lam t ->
    withFresh \n -> HR.Lam n <$> irToHR' t
  IR.Sig π a b -> do
    a <- irToHR' a
    withFresh \n -> do
      b <- irToHR' b
      pure $ HR.Sig π n a b
  IR.Pair s t -> do
    HR.Pair <$> irToHR' s <*> irToHR' t
  IR.UnitTy -> pure HR.UnitTy
  IR.Unit -> pure HR.Unit
  IR.Let π l b -> do
    l <- irElimToHR' l
    withFresh \n -> do
      b <- irToHR' b
      pure (HR.Let π n l b)
  IR.Elim e -> HR.Elim |<< irElimToHR' e

irElimToHR' ::
  (HasNames m, HasPatToSym m) =>
  IR.Elim primTy primVal ->
  m (HR.Elim primTy primVal)
irElimToHR' = \case
  IR.Free (IR.Global n) -> pure $ HR.Var n
  IR.Free (IR.Pattern p) ->
    -- FIXME maybe an error for a failed lookup?
    -- but hrToIR is mostly for printing so maybe it's better to get /something/
    HR.Var . fromMaybe def <$> getPatToSym p
    where
      def = NameSymbol.fromText $ "pat" <> show p
  IR.Bound i -> do
    v <- lookupIndex (fromIntegral i)
    pure (HR.Var v)
  IR.App f x -> do
    f <- irElimToHR' f
    x <- irToHR' x
    pure (HR.App f x)
  IR.Ann u t x l -> do
    t <- irToHR' t
    x <- irToHR' x
    pure (HR.Ann u t x l)

hrPatternToIR ::
  HR.Pattern primTy primVal ->
  (IR.Pattern primTy primVal, HashMap NameSymbol.T IR.PatternVar)
hrPatternToIR = hrPatternToIRWith mempty

hrPatternToIRWith ::
  IR.PatternMap NameSymbol.T ->
  HR.Pattern primTy primVal ->
  (IR.Pattern primTy primVal, HashMap NameSymbol.T IR.PatternVar)
hrPatternToIRWith pats pat =
  hrPatternToIR' pat
    |> exec pats mempty
    |> second symToPat

hrPatternToIR' ::
  (HasNameStack m, HasSymToPat m, HasNextPatVar m) =>
  HR.Pattern primTy primVal ->
  m (IR.Pattern primTy primVal)
hrPatternToIR' = \case
  HR.PCon k ps -> IR.PCon k <$> traverse hrPatternToIR' ps
  HR.PPair p q -> IR.PPair <$> hrPatternToIR' p <*> hrPatternToIR' q
  HR.PUnit -> pure IR.PUnit
  HR.PVar x -> withNextPatVar \i -> IR.PVar i <$ setSymToPat x i
  HR.PDot e -> IR.PDot <$> hrToIR' e
  HR.PPrim p -> pure $ IR.PPrim p

irPatternToHR ::
  IR.Pattern primTy primVal ->
  (HR.Pattern primTy primVal, IR.PatternMap NameSymbol.T)
irPatternToHR = irPatternToHRWith mempty

irPatternToHRWith ::
  IR.PatternMap NameSymbol.T ->
  IR.Pattern primTy primVal ->
  (HR.Pattern primTy primVal, IR.PatternMap NameSymbol.T)
irPatternToHRWith pats pat =
  irPatternToHR' pat
    |> exec pats (varsPattern pat)
    |> second patToSym

irPatternToHR' ::
  (HasNames m, HasPatToSym m) =>
  IR.Pattern primTy primVal ->
  m (HR.Pattern primTy primVal)
irPatternToHR' = \case
  IR.PCon k ps -> HR.PCon k <$> traverse irPatternToHR' ps
  IR.PPair p q -> HR.PPair <$> irPatternToHR' p <*> irPatternToHR' q
  IR.PUnit -> pure HR.PUnit
  IR.PVar i -> withFresh \x -> HR.PVar x <$ setPatToSym i x
  IR.PDot e -> HR.PDot <$> irToHR' e
  IR.PPrim p -> pure $ HR.PPrim p

varsTerm :: IR.Term primTy primVal -> HashSet NameSymbol.T
varsTerm = \case
  IR.Star _ -> mempty
  IR.PrimTy _ -> mempty
  IR.Prim _ -> mempty
  IR.Pi _ s t -> varsTerm s <> varsTerm t
  IR.Lam t -> varsTerm t
  IR.Sig _ s t -> varsTerm s <> varsTerm t
  IR.Pair s t -> varsTerm s <> varsTerm t
  IR.Let _ e t -> varsElim e <> varsTerm t
  IR.UnitTy -> mempty
  IR.Unit -> mempty
  IR.Elim e -> varsElim e

varsElim :: IR.Elim primTy primVal -> HashSet NameSymbol.T
varsElim = \case
  IR.Bound _ -> mempty
  IR.Free (IR.Global x) -> [x]
  IR.Free (IR.Pattern _) -> mempty
  IR.App f s -> varsElim f <> varsTerm s
  IR.Ann _ t a _ -> varsTerm t <> varsTerm a

varsPattern :: IR.Pattern primTy primVal -> HashSet NameSymbol.T
varsPattern = \case
  IR.PCon k ps -> [k] <> foldMap varsPattern ps
  IR.PPair s t -> varsPattern s <> varsPattern t
  IR.PUnit -> mempty
  IR.PVar _ -> mempty
  IR.PDot t -> varsTerm t
  IR.PPrim _ -> mempty

exec ::
  -- | Existing mapping of names to pattern variables, if any
  IR.PatternMap NameSymbol.T ->
  -- | Names/pattern vars to avoid.
  HashSet NameSymbol.T ->
  M a ->
  (a, Env)
exec pats avoid (M env) =
  runState env $
    Env
      { nameSupply = filter (`notElem` avoid) names,
        nextPatVar = 0,
        nameStack = [],
        patToSym = pats,
        symToPat = PM.toList pats |> map swap |> HM.fromList
      }

-- TODO separate states for h→i and i→h maybe??
data Env
  = Env
      { nameSupply :: Stream NameSymbol.T,
        nameStack :: [NameSymbol.T],
        nextPatVar :: IR.PatternVar,
        symToPat :: HashMap NameSymbol.T IR.PatternVar,
        patToSym :: IR.PatternMap NameSymbol.T
      }
  deriving (Generic)

newtype M a = M (State Env a)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasSource "nameSupply" (Stream NameSymbol.T),
      HasSink "nameSupply" (Stream NameSymbol.T),
      HasState "nameSupply" (Stream NameSymbol.T)
    )
    via StateField "nameSupply" (State Env)
  deriving
    ( HasSource "nameStack" [NameSymbol.T],
      HasReader "nameStack" [NameSymbol.T]
    )
    via ReaderField "nameStack" (State Env)
  deriving
    ( HasSource "nextPatVar" IR.PatternVar,
      HasSink "nextPatVar" IR.PatternVar,
      HasState "nextPatVar" IR.PatternVar
    )
    via StateField "nextPatVar" (State Env)
  deriving
    ( HasSource "symToPat" (HashMap NameSymbol.T IR.PatternVar),
      HasSink "symToPat" (HashMap NameSymbol.T IR.PatternVar),
      HasState "symToPat" (HashMap NameSymbol.T IR.PatternVar)
    )
    via StateField "symToPat" (State Env)
  deriving
    ( HasSource "patToSym" (IR.PatternMap NameSymbol.T),
      HasSink "patToSym" (IR.PatternMap NameSymbol.T),
      HasState "patToSym" (IR.PatternMap NameSymbol.T)
    )
    via StateField "patToSym" (State Env)
