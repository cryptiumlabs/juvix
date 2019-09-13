{-# LANGUAGE StandaloneDeriving #-}
module Juvix.Bohm.Shared where

import           Juvix.Library

import           Prelude          (String)
import           Text.Parsec.Expr

data Primitive = PInt Int
               | PBool Bool
               deriving Show

deriving instance Show Assoc

data Precedence = Precedence {
  level  :: Int,
  symbol :: String,
  assoc  :: Assoc
} deriving Show

