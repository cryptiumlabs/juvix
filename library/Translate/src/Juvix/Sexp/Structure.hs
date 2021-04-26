{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Hardeing S-expressions into a more readable form. Here we use a
-- mixture of record structures and aliases. Each cover a form that we
-- wish to talk about rather than just match away at
module Juvix.Sexp.Structure where

import qualified Control.Lens as Lens hiding ((|>))
import Juvix.Library
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

Lens.makeLensesWith Lens.camelCaseFields ''Defun
Lens.makeLensesWith Lens.camelCaseFields ''DefunMatch
Lens.makeLensesWith Lens.camelCaseFields ''ArgBody

--------------------------------------------------------------------------------
-- Converter functions
--------------------------------------------------------------------------------
isDefun :: Sexp.T -> Bool
isDefun (defun1 Sexp.:> _) = Sexp.isAtomNamed defun1 ":defun"
isDefun _ = False

toDefun :: Sexp.T -> Maybe Defun
toDefun form
  | isDefun form =
    case form of
      Sexp.List [_defun, name, args, body] ->
        Defun name args body |> Just
      _ -> Nothing
  | otherwise = Nothing
