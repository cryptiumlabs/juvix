{-# LANGUAGE ViewPatterns #-}

module Juvix.Core.Parameterisations.All where

import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Core.Parameterisations.Naturals as Naturals
import qualified Juvix.Core.Parameterisations.Unit as Unit
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
unNatTy _ = empty

unUnitTy :: Ty -> Maybe Unit.Ty
unUnitTy (UnitTy t) = pure t
unUnitTy _ = empty

hasType :: Val -> P.PrimType Ty -> Bool
hasType (NatVal x) (traverse unNatTy -> Just tys) = Naturals.hasType x tys
hasType (UnitVal x) (traverse unUnitTy -> Just tys) = Unit.hasType x tys
hasType _ _ = False

arityT :: Ty -> Natural
arityT _ = 0

applyT :: Ty -> NonEmpty Ty -> Maybe Ty
applyT _ _ = Nothing

arityV :: Val -> Natural
arityV (NatVal x) = Naturals.arityV x
arityV (UnitVal x) = Unit.arityV x

arity :: Val -> Natural
arity = arityV
{-# DEPRECATED arity "use arityV" #-}

unNatVal :: Val -> Maybe Naturals.Val
unNatVal (NatVal n) = Just n
unNatVal _ = Nothing

applyV :: Val -> NonEmpty Val -> Maybe Val
applyV (NatVal f) (traverse unNatVal -> Just xs) =
  NatVal <$> Naturals.applyV f xs
applyV _ _ = Nothing

apply :: Val -> Val -> Maybe Val
apply f x = applyV f (x :| [])
{-# DEPRECATED apply "use applyV" #-}

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

builtinTypes :: P.Builtins Ty
builtinTypes =
  fmap NatTy Naturals.builtinTypes
    <> fmap UnitTy Unit.builtinTypes

builtinValues :: P.Builtins Val
builtinValues =
  fmap NatVal Naturals.builtinValues
    <> fmap UnitVal Unit.builtinValues

t :: P.Parameterisation Ty Val
t =
  P.Parameterisation
    { hasType,
      builtinTypes,
      arityT,
      applyT,
      builtinValues,
      arityV,
      applyV,
      parseTy,
      parseVal,
      reservedNames,
      reservedOpNames,
      stringTy = \_ _ -> False,
      stringVal = const Nothing,
      intTy = \i _ -> Naturals.isNat i,
      intVal = fmap NatVal . Naturals.natVal,
      floatTy = \_ _ -> False,
      floatVal = const Nothing
    }
