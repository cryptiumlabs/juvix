{-# LANGUAGE ViewPatterns #-}
module Juvix.Core.Parameterisations.All where

import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Core.Parameterisations.Naturals as Naturals
import qualified Juvix.Core.Parameterisations.Unit as Unit
import Juvix.Core.Types hiding
  ( apply,
    parseTy,
    parseVal,
    reservedNames,
    reservedOpNames,
    hasType,
    arity,
  )
import Juvix.Library hiding ((<|>))
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

-- all primitive types
data Ty
  = NatTy Naturals.Ty
  | UnitTy Unit.Ty
  deriving (Show, Eq)

-- c: primitive constant and f: functions
data Val
  = NatVal Naturals.Val
  | UnitVal Unit.Val
  deriving (Show, Eq)

natTyToAll :: Naturals.Ty -> Ty
natTyToAll = NatTy

natValToAll :: Naturals.Val -> Val
natValToAll = NatVal

unitTyToAll :: Unit.Ty -> Ty
unitTyToAll = UnitTy

unitValToAll :: Unit.Val -> Val
unitValToAll = UnitVal

unNatTy :: Ty -> Maybe Naturals.Ty
unNatTy (NatTy t) = pure t
unNatTy _         = empty

unUnitTy :: Ty -> Maybe Unit.Ty
unUnitTy (UnitTy t) = pure t
unUnitTy _         = empty

hasType :: Val -> PrimType Ty -> Bool
hasType (NatVal x)  (traverse unNatTy  -> Just tys) = Naturals.hasType x tys
hasType (UnitVal x) (traverse unUnitTy -> Just tys) = Unit.hasType x tys
hasType _           _                               = False

arity :: Val -> Int
arity (NatVal x)  = Naturals.arity x
arity (UnitVal x) = Unit.arity x

apply :: Val -> Val -> Maybe Val
apply (NatVal nat1) (NatVal nat2) =
  fmap natValToAll (Naturals.apply nat1 nat2)
apply _ _ = Nothing

parseTy :: Token.GenTokenParser String () Identity -> Parser Ty
parseTy lexer =
  (natTyToAll <$> Naturals.parseTy lexer) <|> (unitTyToAll <$> Unit.parseTy lexer)

parseVal :: Token.GenTokenParser String () Identity -> Parser Val
parseVal lexer =
  (natValToAll <$> Naturals.parseVal lexer)
    <|> fmap unitValToAll (Unit.parseVal lexer)

reservedNames :: [String]
reservedNames = Naturals.reservedNames <> Unit.reservedNames

reservedOpNames :: [String]
reservedOpNames = Naturals.reservedOpNames <> Unit.reservedOpNames

t :: Parameterisation Ty Val
t =
  Parameterisation {
    hasType, arity, apply, parseTy, parseVal, reservedNames, reservedOpNames,
    stringTy = \_ _ -> False,
    stringVal = const Nothing,
    intTy = \i _ -> Naturals.isNat i,
    intVal = fmap NatVal . Naturals.natVal,
    floatTy = \_ _ -> False,
    floatVal = const Nothing
  }
