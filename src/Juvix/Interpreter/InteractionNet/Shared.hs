{-# LANGUAGE StandaloneDeriving #-}

module Juvix.Interpreter.InteractionNet.Shared where

import Juvix.Library
import Text.Parsec.Expr
import Prelude (String)

data Primitive
  = PInt Int
  | PBool Bool
  deriving (Show)

deriving instance Show Assoc

data Precedence
  = Precedence
      { level ∷ Int,
        symbol ∷ String,
        assoc ∷ Assoc
      }
  deriving (Show)
