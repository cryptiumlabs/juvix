-- |
-- Hardeing S-expressions into a more readable form. Here we use a
-- mixture of record structures and aliases. Each cover a form that we
-- wish to talk about rather than just match away at
--
-- - _The form for transformation follows this structure_
-- #+begin_src haskell
--   -- the data type
--   data Form = ... deriving (Show)
--   is<Form>   :: Sexp.T -> Bool
--   to<Form>   :: Sexp.T -> Maybe <Form>
--   from<Form> :: <Form> -> Sexp.T
-- #+end_src
-- + With the following properties of the forms
--   #+begin_src haskell
--     ∀ s : Sexp.T. is<Form> s = True ⟷ is-just (to<Form> s)
--
--     to<Form> 。 from<Form> = Just
--   #+end_src
-- _TODO_
--  1. Figure out if we can even express a spec system in
--     Haskell... =to<Form>= and =From<From>= have the exact same signature
--  2. replace the repeat code with the =to<Form>= with an abstraction
--  3. put the meta data with the form so we don't have to do it by
--     hand in the code that uses this
--     1. Use =Juvix.Library.LineNum=
--     2. append the =Form= With this
--     3. have to<Form> fill this
--     4. Have extra smart consturctors that are =<form>=, so that we
--        can automatically fill in this meta data
module Juvix.Sexp.Structure.CoreNamed where

import Juvix.Library hiding (Meta, fromInteger, toInteger)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp
import Juvix.Sexp.Structure.Helpers

newtype Star = Star {starUniverse :: Integer} deriving (Show)

newtype PrimTy = PrimTy {primTyPrim :: Sexp.T} deriving (Show)

newtype Prim = Prim {primPrim :: Sexp.T} deriving (Show)

data Pi = Pi
  { piBinder :: Binder,
    piBody :: Sexp.T
  }
  deriving (Show)

data Binder = Binder
  { binderName :: NameSymbol.T,
    binderUsage :: Sexp.T,
    binderTerm :: Sexp.T
  }
  deriving (Show)

data Lam = Lam
  { lamName :: NameSymbol.T,
    lamBody :: Sexp.T
  }
  deriving (Show)

data Sigma = Sigma
  { sigmaBinder :: Binder,
    sigmaUsage :: Sexp.T
  }
  deriving (Show)

data Pair = Pair
  { pairFst :: Sexp.T,
    pairSnd :: Sexp.T
  }
  deriving (Show)

data Let = Let
  { letBinder :: Binder,
    letBody :: Sexp.T
  }
  deriving (Show)

-- Elim

newtype Var = Var {varName :: NameSymbol.T} deriving (Show)

data App = App
  { appFun :: Sexp.T,
    appArg :: Sexp.T
  }
  deriving (Show)

data Ann = Ann
  { annTerm :: Sexp.T,
    annMeta :: Meta,
    annTy :: Sexp.T
  }
  deriving (Show)

data Meta = Meta
  { metaUsage :: Sexp.T,
    metaUniverse :: Integer
  }
  deriving (Show)

-- Pattern

newtype Dot = Dot {dotBody :: Sexp.T} deriving (Show)

data RawFunClause = RawFunClause
  { rawFunClauseTele :: Sexp.T,
    rawFunClausePats :: Sexp.T,
    rawFunClauseBody :: Sexp.T,
    rawFunClauseCatchAll :: Sexp.T
  }
  deriving (Show)

--------------------------------------------------------------------------------
-- Automatically Generated code
--------------------------------------------------------------------------------

----------------------------------------
-- Star
----------------------------------------

nameStar :: NameSymbol.T
nameStar = ":star"

isStar :: Sexp.T -> Bool
isStar (Sexp.Cons form _) = Sexp.isAtomNamed form nameStar
isStar _ = False

toStar :: Sexp.T -> Maybe Star
toStar form
  | isStar form =
    case form of
      _nameStar Sexp.:> integer1 Sexp.:> Sexp.Nil
        | Just integer1 <- toInteger integer1 ->
          Star integer1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromStar :: Star -> Sexp.T
fromStar (Star integer1) =
  Sexp.list [Sexp.atom nameStar, fromInteger integer1]

----------------------------------------
-- PrimTy
----------------------------------------

namePrimTy :: NameSymbol.T
namePrimTy = ":prim-ty"

isPrimTy :: Sexp.T -> Bool
isPrimTy (Sexp.Cons form _) = Sexp.isAtomNamed form namePrimTy
isPrimTy _ = False

toPrimTy :: Sexp.T -> Maybe PrimTy
toPrimTy form
  | isPrimTy form =
    case form of
      _namePrimTy Sexp.:> sexp1 Sexp.:> Sexp.Nil ->
        PrimTy sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromPrimTy :: PrimTy -> Sexp.T
fromPrimTy (PrimTy sexp1) =
  Sexp.list [Sexp.atom namePrimTy, sexp1]

----------------------------------------
-- Prim
----------------------------------------

namePrim :: NameSymbol.T
namePrim = ":prim"

isPrim :: Sexp.T -> Bool
isPrim (Sexp.Cons form _) = Sexp.isAtomNamed form namePrim
isPrim _ = False

toPrim :: Sexp.T -> Maybe Prim
toPrim form
  | isPrim form =
    case form of
      _namePrim Sexp.:> sexp1 Sexp.:> Sexp.Nil ->
        Prim sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromPrim :: Prim -> Sexp.T
fromPrim (Prim sexp1) =
  Sexp.list [Sexp.atom namePrim, sexp1]

----------------------------------------
-- Pi
----------------------------------------

namePi :: NameSymbol.T
namePi = ":pi"

isPi :: Sexp.T -> Bool
isPi (Sexp.Cons form _) = Sexp.isAtomNamed form namePi
isPi _ = False

toPi :: Sexp.T -> Maybe Pi
toPi form
  | isPi form =
    case form of
      _namePi Sexp.:> binder1 Sexp.:> sexp2 Sexp.:> Sexp.Nil
        | Just binder1 <- toBinder binder1 ->
          Pi binder1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromPi :: Pi -> Sexp.T
fromPi (Pi binder1 sexp2) =
  Sexp.list [Sexp.atom namePi, fromBinder binder1, sexp2]

----------------------------------------
-- Binder
----------------------------------------

toBinder :: Sexp.T -> Maybe Binder
toBinder form =
  case form of
    nameSymbol1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil
      | Just nameSymbol1 <- toNameSymbol nameSymbol1 ->
        Binder nameSymbol1 sexp2 sexp3 |> Just
    _ ->
      Nothing

fromBinder :: Binder -> Sexp.T
fromBinder (Binder nameSymbol1 sexp2 sexp3) =
  Sexp.list [fromNameSymbol nameSymbol1, sexp2, sexp3]

----------------------------------------
-- Lam
----------------------------------------

nameLam :: NameSymbol.T
nameLam = ":named-lambda"

isLam :: Sexp.T -> Bool
isLam (Sexp.Cons form _) = Sexp.isAtomNamed form nameLam
isLam _ = False

toLam :: Sexp.T -> Maybe Lam
toLam form
  | isLam form =
    case form of
      _nameLam Sexp.:> nameSymbol1 Sexp.:> sexp2 Sexp.:> Sexp.Nil
        | Just nameSymbol1 <- toNameSymbol nameSymbol1 ->
          Lam nameSymbol1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLam :: Lam -> Sexp.T
fromLam (Lam nameSymbol1 sexp2) =
  Sexp.list [Sexp.atom nameLam, fromNameSymbol nameSymbol1, sexp2]

----------------------------------------
-- Sigma
----------------------------------------

nameSigma :: NameSymbol.T
nameSigma = ":sigma"

isSigma :: Sexp.T -> Bool
isSigma (Sexp.Cons form _) = Sexp.isAtomNamed form nameSigma
isSigma _ = False

toSigma :: Sexp.T -> Maybe Sigma
toSigma form
  | isSigma form =
    case form of
      _nameSigma Sexp.:> binder1 Sexp.:> sexp2 Sexp.:> Sexp.Nil
        | Just binder1 <- toBinder binder1 ->
          Sigma binder1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromSigma :: Sigma -> Sexp.T
fromSigma (Sigma binder1 sexp2) =
  Sexp.list [Sexp.atom nameSigma, fromBinder binder1, sexp2]

----------------------------------------
-- Pair
----------------------------------------

namePair :: NameSymbol.T
namePair = ":pair"

isPair :: Sexp.T -> Bool
isPair (Sexp.Cons form _) = Sexp.isAtomNamed form namePair
isPair _ = False

toPair :: Sexp.T -> Maybe Pair
toPair form
  | isPair form =
    case form of
      _namePair Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        Pair sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromPair :: Pair -> Sexp.T
fromPair (Pair sexp1 sexp2) =
  Sexp.list [Sexp.atom namePair, sexp1, sexp2]

----------------------------------------
-- Let
----------------------------------------

nameLet :: NameSymbol.T
nameLet = ":named-let"

isLet :: Sexp.T -> Bool
isLet (Sexp.Cons form _) = Sexp.isAtomNamed form nameLet
isLet _ = False

toLet :: Sexp.T -> Maybe Let
toLet form
  | isLet form =
    case form of
      _nameLet Sexp.:> binder1 Sexp.:> sexp2 Sexp.:> Sexp.Nil
        | Just binder1 <- toBinder binder1 ->
          Let binder1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLet :: Let -> Sexp.T
fromLet (Let binder1 sexp2) =
  Sexp.list [Sexp.atom nameLet, fromBinder binder1, sexp2]

----------------------------------------
-- Var
----------------------------------------

toVar :: Sexp.T -> Maybe Var
toVar form =
  case form of
    nameSymbol1 Sexp.:> Sexp.Nil
      | Just nameSymbol1 <- toNameSymbol nameSymbol1 ->
        Var nameSymbol1 |> Just
    _ ->
      Nothing

fromVar :: Var -> Sexp.T
fromVar (Var nameSymbol1) =
  Sexp.list [fromNameSymbol nameSymbol1]

----------------------------------------
-- App
----------------------------------------

toApp :: Sexp.T -> Maybe App
toApp form =
  case form of
    sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
      App sexp1 sexp2 |> Just
    _ ->
      Nothing

fromApp :: App -> Sexp.T
fromApp (App sexp1 sexp2) =
  Sexp.list [sexp1, sexp2]

----------------------------------------
-- Ann
----------------------------------------

nameAnn :: NameSymbol.T
nameAnn = ":"

isAnn :: Sexp.T -> Bool
isAnn (Sexp.Cons form _) = Sexp.isAtomNamed form nameAnn
isAnn _ = False

toAnn :: Sexp.T -> Maybe Ann
toAnn form
  | isAnn form =
    case form of
      _nameAnn Sexp.:> sexp1 Sexp.:> meta2 Sexp.:> sexp3 Sexp.:> Sexp.Nil
        | Just meta2 <- toMeta meta2 ->
          Ann sexp1 meta2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromAnn :: Ann -> Sexp.T
fromAnn (Ann sexp1 meta2 sexp3) =
  Sexp.list [Sexp.atom nameAnn, sexp1, fromMeta meta2, sexp3]

----------------------------------------
-- Meta
----------------------------------------

toMeta :: Sexp.T -> Maybe Meta
toMeta form =
  case form of
    sexp1 Sexp.:> integer2 Sexp.:> Sexp.Nil
      | Just integer2 <- toInteger integer2 ->
        Meta sexp1 integer2 |> Just
    _ ->
      Nothing

fromMeta :: Meta -> Sexp.T
fromMeta (Meta sexp1 integer2) =
  Sexp.list [sexp1, fromInteger integer2]

----------------------------------------
-- RawFunClause
----------------------------------------

toRawFunClause :: Sexp.T -> Maybe RawFunClause
toRawFunClause form =
  case form of
    sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> sexp4 Sexp.:> Sexp.Nil ->
      RawFunClause sexp1 sexp2 sexp3 sexp4 |> Just
    _ ->
      Nothing

fromRawFunClause :: RawFunClause -> Sexp.T
fromRawFunClause (RawFunClause sexp1 sexp2 sexp3 sexp4) =
  Sexp.list [sexp1, sexp2, sexp3, sexp4]
