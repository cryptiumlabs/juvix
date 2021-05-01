{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

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
module Juvix.Sexp.Structure where

import qualified Control.Lens as Lens hiding ((|>))
import Juvix.Library
import qualified Juvix.Library.LineNum as LineNum
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp

-- | @Defun@ is the base defun structure
-- currently it does not have matching
data Defun = Defun
  { defunName :: Sexp.T,
    defunArgs :: Sexp.T,
    defunBody :: Sexp.T
  }
  deriving (Show)

-- | @Defun-match@ is a matching defun structure
data DefunMatch = DefunMatch
  { defunMatchName :: Sexp.T,
    defunMatchArgs :: [ArgBody]
  }
  deriving (Show)

data ArgBody = ArgBody {argsBodyArgs :: Sexp.T, argsBodyBody :: Sexp.T} deriving (Show)

data DefunSigMatch = DefunSigMatch
  { defunSigMatchName :: Sexp.T,
    defunSigMatchSig :: Sexp.T,
    defunSigMatchArgs :: [ArgBody]
  }
  deriving (Show)

-- TODO ∷ have a property sexp form!?

-- | @signature@ is the signature of the term
data Signature = Signature
  { signatureName :: Sexp.T,
    signatureSig :: Sexp.T
  }
  deriving (Show)

data Let = Let
  { letName :: Sexp.T,
    letArgs :: Sexp.T,
    letBody :: Sexp.T,
    letRest :: Sexp.T
  }
  deriving (Show)

Lens.makeLensesWith Lens.camelCaseFields ''Defun
Lens.makeLensesWith Lens.camelCaseFields ''DefunMatch
Lens.makeLensesWith Lens.camelCaseFields ''ArgBody
Lens.makeLensesWith Lens.camelCaseFields ''Signature
Lens.makeLensesWith Lens.camelCaseFields ''DefunSigMatch
Lens.makeLensesWith Lens.camelCaseFields ''Let

--------------------------------------------------------------------------------
-- Converter functions
-- The format for these are
-- name<Form> :: NameSymbol.T
-- is<Form>   :: Sexp.T -> Bool
-- to<Form>   :: Sexp.T -> Maybe <Form>
-- from<Form> :: <Form> -> Sexp.T
--------------------------------------------------------------------------------
----------------------------------------
-- ArgBody
----------------------------------------

toArgBody :: Sexp.T -> Maybe ArgBody
toArgBody form =
    case form of
      sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
          ArgBody sexp1 sexp2 |> Just
      _ -> Nothing

fromArgBody :: ArgBody -> Sexp.T
fromArgBody (ArgBody sexp1 sexp2) =
  Sexp.list [sexp1, sexp2]

----------------------------------------
-- Defun
----------------------------------------

nameDefun :: NameSymbol.T
nameDefun = ":defun"

isDefun :: Sexp.T -> Bool
isDefun (form Sexp.:> _) = Sexp.isAtomNamed form nameDefun
isDefun _ = False

toDefun :: Sexp.T -> Maybe Defun
toDefun form
  | isDefun form =
    case form of
      _Defun Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil ->
          Defun sexp1 sexp2 sexp3 |> Just
      _ -> Nothing
  | otherwise = Nothing

fromDefun :: Defun -> Sexp.T
fromDefun (Defun sexp1 sexp2 sexp3) =
  Sexp.list [Sexp.atom nameDefun, sexp1, sexp2, sexp3]

----------------------------------------
-- DefunMatch
----------------------------------------

nameDefunMatch :: NameSymbol.T
nameDefunMatch = ":defun-match"

isDefunMatch :: Sexp.T -> Bool
isDefunMatch (form Sexp.:> _) = Sexp.isAtomNamed form nameDefunMatch
isDefunMatch _ = False

toDefunMatch :: Sexp.T -> Maybe DefunMatch
toDefunMatch form
  | isDefunMatch form =
    case form of
      _DefunMatch Sexp.:> sexp1 Sexp.:> argBody2
        | Just argBody2 <- toArgBody `fromStarList` argBody2 ->
          DefunMatch sexp1 argBody2 |> Just
      _ -> Nothing
  | otherwise = Nothing

fromDefunMatch :: DefunMatch -> Sexp.T
fromDefunMatch (DefunMatch sexp1 argBody2) =
  Sexp.listStar [Sexp.atom nameDefunMatch, sexp1, fromArgBody `toStarList` argBody2]

----------------------------------------
-- DefunSigMatch
----------------------------------------

nameDefunSigMatch :: NameSymbol.T
nameDefunSigMatch = ":defsig-match"

isDefunSigMatch :: Sexp.T -> Bool
isDefunSigMatch (form Sexp.:> _) = Sexp.isAtomNamed form nameDefunSigMatch
isDefunSigMatch _ = False

toDefunSigMatch :: Sexp.T -> Maybe DefunSigMatch
toDefunSigMatch form
  | isDefunSigMatch form =
    case form of
      _DefunSigMatch Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> argBody3
        | Just argBody3 <- toArgBody `fromStarList` argBody3 ->
          DefunSigMatch sexp1 sexp2 argBody3 |> Just
      _ -> Nothing
  | otherwise = Nothing

fromDefunSigMatch :: DefunSigMatch -> Sexp.T
fromDefunSigMatch (DefunSigMatch sexp1 sexp2 argBody3) =
  Sexp.listStar [Sexp.atom nameDefunSigMatch, sexp1, sexp2, fromArgBody `toStarList` argBody3]

----------------------------------------
-- Signature
----------------------------------------

nameSignature :: NameSymbol.T
nameSignature = ":defsig"

isSignature :: Sexp.T -> Bool
isSignature (form Sexp.:> _) = Sexp.isAtomNamed form nameSignature
isSignature _ = False

toSignature :: Sexp.T -> Maybe Signature
toSignature form
  | isSignature form =
    case form of
      _Signature Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
          Signature sexp1 sexp2 |> Just
      _ -> Nothing
  | otherwise = Nothing

fromSignature :: Signature -> Sexp.T
fromSignature (Signature sexp1 sexp2) =
  Sexp.list [Sexp.atom nameSignature, sexp1, sexp2]

----------------------------------------
-- Let
----------------------------------------

nameLet :: NameSymbol.T
nameLet = "let"

isLet :: Sexp.T -> Bool
isLet (form Sexp.:> _) = Sexp.isAtomNamed form nameLet
isLet _ = False

toLet :: Sexp.T -> Maybe Let
toLet form
  | isLet form =
    case form of
      _Let Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> sexp4 Sexp.:> Sexp.Nil ->
          Let sexp1 sexp2 sexp3 sexp4 |> Just
      _ -> Nothing
  | otherwise = Nothing

fromLet :: Let -> Sexp.T
fromLet (Let sexp1 sexp2 sexp3 sexp4) =
  Sexp.list [Sexp.atom nameLet, sexp1, sexp2, sexp3, sexp4]

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

fromGen :: (t -> Bool) -> (t -> Maybe a) -> t -> Maybe a
fromGen pred func form
  | pred form = func form
  | otherwise = Nothing

toStarList :: (t -> Sexp.T) -> [t] -> Sexp.T
toStarList f (x : xs) =
  f x Sexp.:> toStarList f xs
toStarList _ [] = Sexp.Nil

fromStarList :: (Sexp.T -> Maybe a) -> Sexp.T -> Maybe [a]
fromStarList f (x Sexp.:> xs) =
  (:) <$> f x <*> fromStarList f xs
fromStarList _ Sexp.Nil = Just []
fromStarList _ _ = Nothing
