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

-- TODO: Add rest of primitive values.
typeOf ∷ PrimVal → NonEmpty PrimTy
typeOf (PrimConst v) = PrimTy (M.Type (constType v) "") :| []

constType ∷ M.Value' Op → M.T
constType v =
  case v of
    M.ValueInt _ → M.Tc M.CInt
    M.ValueUnit → M.TUnit

-- TODO: Use interpreter for this.
apply ∷ PrimVal → PrimVal → Maybe PrimVal
apply _ _ = Nothing

-- TODO: parse all types.
parseTy ∷ Token.GenTokenParser String () Identity → Parser PrimTy
parseTy lexer = do
  Token.reserved lexer "Unit"
  pure (PrimTy (M.Type M.TUnit ""))

-- TODO: parse all values.
parseVal ∷ Token.GenTokenParser String () Identity → Parser PrimVal
parseVal lexer = do
  Token.reserved lexer "()"
  pure (PrimConst (M.ValueUnit))

reservedNames ∷ [String]
reservedNames = ["Unit", "()"]

reservedOpNames ∷ [String]
reservedOpNames = []

michelson ∷ C.Parameterisation PrimTy PrimVal
michelson = C.Parameterisation typeOf apply parseTy parseVal reservedNames reservedOpNames
