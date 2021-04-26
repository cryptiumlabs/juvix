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
--  1. replace the repeat code with the =to<Form>= with an abstraction
--  2. put the meta data with the form so we don't have to do it by
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

-- | @Defun@ is the base defun strcuture
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

data ArgBody = AB {argsBodyArgs :: Sexp.T, argsBodyBody :: Sexp.T} deriving (Show)

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

Lens.makeLensesWith Lens.camelCaseFields ''Defun
Lens.makeLensesWith Lens.camelCaseFields ''DefunMatch
Lens.makeLensesWith Lens.camelCaseFields ''ArgBody
Lens.makeLensesWith Lens.camelCaseFields ''Signature
Lens.makeLensesWith Lens.camelCaseFields ''DefunSigMatch

--------------------------------------------------------------------------------
-- Converter functions
-- The format for these are
-- <Form>Name :: NameSymbol.T
-- is<Form>   :: Sexp.T -> Bool
-- to<Form>   :: Sexp.T -> Maybe <Form>
-- from<Form> :: <Form> -> Sexp.T
--------------------------------------------------------------------------------

----------------------------------------
-- Defun
----------------------------------------

defName :: NameSymbol.T
defName = ":defun"

isDefun :: Sexp.T -> Bool
isDefun (defun1 Sexp.:> _) = Sexp.isAtomNamed defun1 defName
isDefun _ = False

toDefun :: Sexp.T -> Maybe Defun
toDefun form
  | isDefun form =
    case form of
      Sexp.List [_defun, name, args, body] ->
        Defun name args body |> Just
      _ -> Nothing
  | otherwise = Nothing

----------------------------------------
-- def-Match
----------------------------------------

defmatchName :: NameSymbol.T
defmatchName = ":defun-match"

isDefunMatch :: Sexp.T -> Bool
isDefunMatch (defun Sexp.:> _) = Sexp.isAtomNamed defun defmatchName
isDefunMatch _ = False

toDefunMatch :: Sexp.T -> Maybe DefunMatch
toDefunMatch form
  | isDefunMatch form =
    case form of
      _defmatch Sexp.:> name Sexp.:> argsBody
        | Just ab <- toArgsBody argsBody ->
          DefunMatch name ab |> Just
      _ ->
        Nothing
  | otherwise = Nothing

fromDefunMatch :: DefunMatch -> Sexp.T
fromDefunMatch (DefunMatch name ab) =
  Sexp.listStar [Sexp.atom defmatchName, name, fromArgBodys ab]

----------------------------------------
-- Args Body
----------------------------------------

toArgsBody :: Sexp.T -> Maybe [ArgBody]
toArgsBody (x Sexp.:> xs) =
  case x of
    -- this will need updating
    Sexp.List [args, body] -> (AB args body :) <$> toArgsBody xs
    _ -> Nothing
toArgsBody Sexp.Nil = Just []
toArgsBody _ = Nothing

fromArgBodys :: [ArgBody] -> Sexp.T
fromArgBodys =
  -- this will also need updating
  foldr (\ab acc -> fromArgBody ab Sexp.:> acc) Sexp.Nil

fromArgBody :: ArgBody -> Sexp.T
fromArgBody (AB args body) = Sexp.list [args, body]

----------------------------------------
-- Signature
----------------------------------------

sigName :: NameSymbol.T
sigName = ":defsig"

isSignature :: Sexp.T -> Bool
isSignature (sig Sexp.:> _) = Sexp.isAtomNamed sig sigName
isSignature _ = False

toSignature :: Sexp.T -> Maybe Signature
toSignature form
  | isSignature form =
    case form of
      Sexp.List [_defSig, name, sig] ->
        Signature name sig |> Just
      _ -> Nothing
  | otherwise = Nothing

fromSignature :: Signature -> Sexp.T
fromSignature (Signature name sig) = Sexp.list [Sexp.atom sigName, name, sig]

----------------------------------------
-- DefMatchSig
----------------------------------------

defSigMatchName :: NameSymbol.T
defSigMatchName = ":defsig-match"

isDefunSigMatch :: Sexp.T -> Bool
isDefunSigMatch (def Sexp.:> _) = Sexp.isAtomNamed def defSigMatchName
isDefunSigMatch _ = False

fromDefunSigMatch :: DefunSigMatch -> Sexp.T
fromDefunSigMatch (DefunSigMatch name sig argsBody) =
  Sexp.listStar [Sexp.atom defSigMatchName, name, sig, fromArgBodys argsBody]

toDefunSigMatch :: Sexp.T -> Maybe DefunSigMatch
toDefunSigMatch form
  | isDefunSigMatch form =
    case form of
      _def Sexp.:> name Sexp.:> sig Sexp.:> arsMatch
        | Just ab <- toArgsBody arsMatch ->
          DefunSigMatch name sig ab |> Just
      _ -> Nothing
  | otherwise = Nothing
