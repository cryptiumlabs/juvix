module Juvix.Backends.Michelson.Parameterisation where

import qualified Juvix.Core.Erased.Types as C
import qualified Juvix.Core.Types as C
import Juvix.Library
import qualified Michelson.Untyped as M
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

type Term = C.Term PrimVal

type Type = C.Type PrimTy

type Value = M.Value' M.ExpandedOp

type Op = M.ExpandedOp

data PrimTy
  = PrimTy M.Type
  deriving (Show, Eq, Generic)

data PrimVal
  = PrimConst (M.Value' Op)
  | PrimPair
  | PrimFst
  | PrimSnd
  -- TODO: Add all Michelson instructions which are functions.

  deriving (Show, Eq, Generic)

typeOf ∷ PrimVal → [PrimTy]
typeOf = undefined

apply ∷ PrimVal → PrimVal → Maybe PrimVal
apply = undefined

parseTy ∷ Token.GenTokenParser String () Identity → Parser PrimTy
parseTy = undefined

parseVal ∷ Token.GenTokenParser String () Identity → Parser PrimVal
parseVal = undefined

reservedNames ∷ [String]
reservedNames = []

reservedOpNames ∷ [String]
reservedOpNames = []

michelson ∷ C.Parameterisation PrimTy PrimVal
michelson = C.Parameterisation typeOf apply parseTy parseVal reservedNames reservedOpNames
