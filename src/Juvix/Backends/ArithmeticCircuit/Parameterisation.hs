module Juvix.Backends.ArithmeticCircuit.Parameterisation where

import qualified Juvix.Core.Erased.Types as J
import qualified Juvix.Core.Types as J
import Juvix.Library
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

type Term = J.Term PrimVal

type Type = J.Type PrimTy

type PrimTy = ()

type PrimVal = ()

typeOf ∷ PrimVal → [PrimTy]
typeOf () = [()]

apply ∷ PrimVal → PrimVal → Maybe PrimVal
apply _ _ = Nothing

parseTy ∷ Token.GenTokenParser String () Identity → Parser PrimTy
parseTy lexer = undefined

parseVal ∷ Token.GenTokenParser String () Identity → Parser PrimVal
parseVal lexer = undefined

reservedNames ∷ [String]
reservedNames = []

reservedOpNames ∷ [String]
reservedOpNames = []

arithmeticCircuit ∷ J.Parameterisation PrimTy PrimVal
arithmeticCircuit = J.Parameterisation typeOf apply parseTy parseVal reservedNames reservedOpNames
