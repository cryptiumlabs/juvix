module Juvix.Core.Parameterisations.All where

import qualified Juvix.Core.Parameterisations.Naturals as Naturals
import qualified Juvix.Core.Parameterisations.Unit as Unit
import Juvix.Core.Types hiding
  ( apply,
    parseTy,
    parseVal,
    reservedNames,
    reservedOpNames,
    typeOf,
  )
import Juvix.Library hiding ((<|>))
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

-- all primitive types
data AllTy
  = NatTy Naturals.NatTy
  | UnitTy Unit.UnitTy
  deriving (Show, Eq)

-- c: primitive constant and f: functions
data AllVal
  = NatVal Naturals.NatVal
  | UnitVal Unit.UnitVal
  deriving (Show, Eq)

natTyToAll ∷ Naturals.NatTy → AllTy
natTyToAll = NatTy

natValToAll ∷ Naturals.NatVal → AllVal
natValToAll = NatVal

unitTyToAll ∷ Unit.UnitTy → AllTy
unitTyToAll = UnitTy

unitValToAll ∷ Unit.UnitVal → AllVal
unitValToAll = UnitVal

typeOf ∷ AllVal → NonEmpty AllTy
typeOf (NatVal nat) =
  fmap natTyToAll (Naturals.typeOf nat)
typeOf (UnitVal unit) = fmap unitTyToAll (Unit.typeOf unit)

apply ∷ AllVal → AllVal → Maybe AllVal
apply (NatVal nat1) (NatVal nat2) =
  fmap natValToAll (Naturals.apply nat1 nat2)
apply _ _ = Nothing

parseTy ∷ Token.GenTokenParser String () Identity → Parser AllTy
parseTy lexer =
  (natTyToAll <$> Naturals.parseTy lexer) <|> (unitTyToAll <$> Unit.parseTy lexer)

parseVal ∷ Token.GenTokenParser String () Identity → Parser AllVal
parseVal lexer =
  (natValToAll <$> Naturals.parseVal lexer)
    <|> fmap unitValToAll (Unit.parseVal lexer)

reservedNames ∷ [String]
reservedNames = Naturals.reservedNames <> Unit.reservedNames

reservedOpNames ∷ [String]
reservedOpNames = Naturals.reservedOpNames <> Unit.reservedOpNames

all ∷ Parameterisation AllTy AllVal
all =
  Parameterisation typeOf apply parseTy parseVal reservedNames reservedOpNames
