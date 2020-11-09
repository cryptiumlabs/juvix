{-# LANGUAGE DeriveFunctor #-}

module Juvix.Core.Parameterisation where

import qualified Juvix.Core.Application as App
import Juvix.Library
import Juvix.Library.HashMap (HashMap)
import qualified Juvix.Library.NameSymbol as NameSymbol
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

-- | @[A, B, ..., Z]@ represents the type
-- @π A -> ρ B -> ... -> Z@ for any usages @π@, @ρ@
type PrimType primTy = NonEmpty primTy

type Builtins p = HashMap NameSymbol.T p

data Parameterisation primTy primVal
  = Parameterisation
      { hasType :: primVal -> PrimType primTy -> Bool,
        builtinTypes :: Builtins primTy,
        builtinValues :: Builtins primVal,
        parseTy :: Token.GenTokenParser String () Identity -> Parser primTy,
        parseVal :: Token.GenTokenParser String () Identity -> Parser primVal,
        reservedNames :: [String],
        reservedOpNames :: [String],
        stringTy :: Text -> primTy -> Bool,
        stringVal :: Text -> Maybe primVal,
        intTy :: Integer -> primTy -> Bool,
        intVal :: Integer -> Maybe primVal,
        floatTy :: Double -> primTy -> Bool,
        floatVal :: Double -> Maybe primVal
      }
  deriving (Generic)

{-# DEPRECATED
  parseTy,
  parseVal,
  reservedNames,
  reservedOpNames
  "TODO: update parser to not use these"
  #-}

data ApplyError' e a
  = ExtraArguments a (NonEmpty a)
  | InvalidArguments a (NonEmpty a)
  | Extra e
  deriving (Eq, Show, Functor)

type ApplyError a = ApplyError' (ApplyErrorExtra a) a

class CanApply a where
  type ApplyErrorExtra a
  type ApplyErrorExtra a = Void

  arity :: a -> Natural
  apply :: a -> NonEmpty a -> Either (ApplyError a) a

mapApplyErr ::
  (ApplyErrorExtra a ~ ApplyErrorExtra b) =>
  (a -> b) ->
  Either (ApplyError a) a ->
  Either (ApplyError b) b
mapApplyErr f = bimap (fmap f) f

applyMaybe :: CanApply a => a -> NonEmpty a -> Maybe a
applyMaybe f xs = either (const Nothing) Just $ apply f xs

apply1 :: CanApply a => a -> a -> Either (ApplyError a) a
apply1 f x = apply f (x :| [])

apply1Maybe :: CanApply a => a -> a -> Maybe a
apply1Maybe f x = applyMaybe f (x :| [])

type TypedPrim ty val = App.Return (PrimType ty) val
