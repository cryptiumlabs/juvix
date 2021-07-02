{-# LANGUAGE OverloadedLists #-}

module Juvix.Core.Translate
  ( hrToIR,
    hrToIRWith,
    irToHR,
    irToHRWith,
    hrPatternToIR,
    hrPatternsToIR,
    hrPatternToIRWith,
    irPatternToHR,
    irPatternToHRWith,
  )
where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.IntMap.Strict as PM
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.HR.Types as HR
import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.Utility
import Juvix.Library hiding (filter)
import qualified Juvix.Library.NameSymbol as NameSymbol

-- | @hrToIR@ runs @hrToIR'@ with an empty stack, see that function for
-- more documentation
hrToIR :: HR.Term primTy primVal -> IR.Term primTy primVal
hrToIR = hrToIRWith mempty

-- contract: no shadowing
-- TODO - handle this automatically by renaming shadowed vars

-- | @hrToIRWith@ runs @hrToIR'@ with given @pats@. These @pats@ are
-- used as extra arguments to the algorithm, serving as the global
-- functions argument to the term itself.
hrToIRWith ::
  -- | name <-> pattern var mapping from outer scopes
  HashMap NameSymbol.T Core.PatternVar ->
  HR.Term primTy primVal ->
  IR.Term primTy primVal
hrToIRWith pats term =
  hrToIR' term
    |> execSymToPat pats mempty
    |> fst

-- | @hrToIR'@ transforms an HR term into an IR term. The is achieved
-- by pushing binder variables to the stack. Inside @hrToIR'@ we call
-- @hrElimToIR'@ for @Elim@'s that lookup the index of stack to get the
-- correct de Bruijn index.
hrToIR' ::
  (HasNameStack m, HasSymToPat m) =>
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

-- | @hrElimToIR'@ is the @Elim@ form of the algorithm
-- @hrToIR'@. Namely this is responsible for looking up the index of
-- the given bound variable to be the de Bruijn index. Names which are
-- not found are either treated as @Global@ or @Pattern@ variables,
-- with the latter referring to the function arguments to the term
hrElimToIR' ::
  (HasNameStack m, HasSymToPat m) =>
  HR.Elim primTy primVal ->
  m (IR.Elim primTy primVal)
hrElimToIR' = \case
  HR.Var n -> do
    maybeIndex <- lookupName n
    case maybeIndex of
      Just ind -> pure $ IR.Bound (fromIntegral ind)
      Nothing -> do
        symTable <- get @"symToPat"
        maybe (Core.Global n) Core.Pattern (symTable HM.!? n)
          |> IR.Free
          |> pure
  HR.App f x -> do
    f <- hrElimToIR' f
    x <- hrToIR' x
    pure (IR.App f x)
  HR.Ann u t x l -> do
    t <- hrToIR' t
    x <- hrToIR' x
    pure (IR.Ann u t x l)

-- | @irToHR@ runs @irToHR'@ with an empty stack, see that function for
-- more documentation
irToHR :: IR.Term primTy primVal -> HR.Term primTy primVal
irToHR = irToHRWith mempty

-- | @irToHRWith@ runs @irToHR'@ with given @pats@ These @pats@ are
-- used as extra arguments to the algorithm, serving as the global
-- functions argument to the term itself.
irToHRWith ::
  -- | pattern var <-> name mapping from outer scopes
  Core.PatternMap NameSymbol.T ->
  IR.Term primTy primVal ->
  HR.Term primTy primVal
irToHRWith pats t = fst $ exec pats (varsTerm t) $ irToHR' t

-- | @irToHR'@ transforms an IR term into an HR one. works like
-- @hrToIR'@ but in reverse. Namely we have binders introduce a newly
-- generated name to the stack. Inside of @irToHR'@ we then call
-- @irElimToHR'@ for @Elim's@ that lookup the de Bruijn index to get
-- the correct generated name
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

-- | @irElimToHR'@ is the @Elim@ form of the algoirthm
-- @irToHR'@. Namely this is responsible for relating the de Bruijn
-- index with the generated name.
irElimToHR' ::
  (HasNames m, HasPatToSym m) =>
  IR.Elim primTy primVal ->
  m (HR.Elim primTy primVal)
irElimToHR' = \case
  IR.Free (Core.Global n) -> pure $ HR.Var n
  IR.Free (Core.Pattern p) ->
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

-- | @hrPatternsToIR@ works like @hrPatternToIR@ but for a list of variables
hrPatternsToIR ::
  Traversable t =>
  t (HR.Pattern primTy primVal) ->
  (t (IR.Pattern primTy primVal), HashMap NameSymbol.T Core.PatternVar)
hrPatternsToIR pats =
  mapAccumL (swap ... hrPatternToIRWith) mempty pats |> swap

hrPatternToIR ::
  HR.Pattern primTy primVal ->
  (IR.Pattern primTy primVal, HashMap NameSymbol.T Core.PatternVar)
hrPatternToIR = hrPatternToIRWith mempty

hrPatternToIRWith ::
  HashMap NameSymbol.T Core.PatternVar ->
  HR.Pattern primTy primVal ->
  (IR.Pattern primTy primVal, HashMap NameSymbol.T Core.PatternVar)
hrPatternToIRWith pats pat =
  hrPatternToIR' pat
    |> execSymToPat pats mempty
    |> second symToPat

hrPatternToIR' ::
  (HasNameStack m, HasSymToPat m, HasNextPatVar m) =>
  HR.Pattern primTy primVal ->
  m (IR.Pattern primTy primVal)
hrPatternToIR' = \case
  HR.PCon k ps -> IR.PCon k <$> traverse hrPatternToIR' ps
  HR.PPair p q -> IR.PPair <$> hrPatternToIR' p <*> hrPatternToIR' q
  HR.PUnit -> pure IR.PUnit
  HR.PVar x -> withNextPatVar (\i -> IR.PVar i <$ setSymToPat x i)
  HR.PDot e -> IR.PDot <$> hrToIR' e
  HR.PPrim p -> pure $ IR.PPrim p

irPatternToHR ::
  IR.Pattern primTy primVal ->
  (HR.Pattern primTy primVal, Core.PatternMap NameSymbol.T)
irPatternToHR = irPatternToHRWith mempty

irPatternToHRWith ::
  Core.PatternMap NameSymbol.T ->
  IR.Pattern primTy primVal ->
  (HR.Pattern primTy primVal, Core.PatternMap NameSymbol.T)
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
  IR.Free (Core.Global x) -> [x]
  IR.Free (Core.Pattern _) -> mempty
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

-- TODO ∷ the patterns are nice, however this doesn't reflect in the
-- nextPatVar. If we add a max key function that could fix it
exec ::
  -- | Existing mapping of names to pattern variables, if any
  Core.PatternMap NameSymbol.T ->
  -- | Names/pattern vars to avoid.
  HashSet NameSymbol.T ->
  M a ->
  (a, Env)
exec pats avoid (M env) =
  runState env $
    Env
      { nameSupply = filter (`notElem` avoid) names,
        nextPatVar =
          case PM.lookupMax pats of
            Nothing -> 0
            Just (k, _) -> succ k,
        nameStack = [],
        patToSym = pats,
        symToPat = PM.toList pats |> map swap |> HM.fromList
      }

-- | @execSymToPat@ works like exec but takes the symtoPat map instead of the patToSym map
execSymToPat ::
  HashMap NameSymbol.T Core.PatternVar -> HashSet NameSymbol.T -> M a -> (a, Env)
execSymToPat pats = exec (HM.toList pats |> map swap |> PM.fromList)

-- TODO separate states for h→i and i→h maybe??
data Env = Env
  { nameSupply :: Stream NameSymbol.T,
    nameStack :: [NameSymbol.T],
    nextPatVar :: Core.PatternVar,
    symToPat :: HashMap NameSymbol.T Core.PatternVar,
    patToSym :: Core.PatternMap NameSymbol.T
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
    ( HasSource "nextPatVar" Core.PatternVar,
      HasSink "nextPatVar" Core.PatternVar,
      HasState "nextPatVar" Core.PatternVar
    )
    via StateField "nextPatVar" (State Env)
  deriving
    ( HasSource "symToPat" (HashMap NameSymbol.T Core.PatternVar),
      HasSink "symToPat" (HashMap NameSymbol.T Core.PatternVar),
      HasState "symToPat" (HashMap NameSymbol.T Core.PatternVar)
    )
    via StateField "symToPat" (State Env)
  deriving
    ( HasSource "patToSym" (Core.PatternMap NameSymbol.T),
      HasSink "patToSym" (Core.PatternMap NameSymbol.T),
      HasState "patToSym" (Core.PatternMap NameSymbol.T)
    )
    via StateField "patToSym" (State Env)
