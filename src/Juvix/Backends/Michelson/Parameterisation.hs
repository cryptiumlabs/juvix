module Juvix.Backends.Michelson.Parameterisation where

import Control.Monad.Fail (fail)
import qualified Data.Text as Text
import qualified Juvix.Core.ErasedAnn.Types as C
import qualified Juvix.Core.Types as C
import Juvix.Library hiding (many, try)
import qualified Michelson.Macro as M
import qualified Michelson.Parser as M
import qualified Michelson.Untyped as M
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

type Term = C.AnnTerm PrimTy PrimVal

type Type = C.Type PrimTy PrimVal

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
-- TODO: Add dependent functions for pair, fst, snd, etc.
typeOf ∷ PrimVal → NonEmpty PrimTy
typeOf (PrimConst v) = PrimTy (M.Type (constType v) "") :| []

constType ∷ M.Value' Op → M.T
constType v =
  case v of
    M.ValueInt _ → M.Tc M.CInt
    M.ValueUnit → M.TUnit
    M.ValueTrue → M.Tc M.CBool
    M.ValueFalse → M.Tc M.CBool

arity :: PrimVal -> Int
arity = flip ((-)) 1 . length . typeOf 

-- TODO: Use interpreter for this, or just write it (simple enough).
-- Might need to add curried versions of built-in functions.
apply ∷ PrimVal → PrimVal → Maybe PrimVal
apply _ _ = Nothing

parseTy ∷ Token.GenTokenParser String () Identity → Parser PrimTy
parseTy lexer =
  try
    ( do
        ty ← wrapParser lexer M.type_
        pure (PrimTy ty)
    )

-- TODO: parse all values.
parseVal ∷ Token.GenTokenParser String () Identity → Parser PrimVal
parseVal lexer =
  try
    ( do
        val ← wrapParser lexer M.value
        pure (PrimConst (M.expandValue val))
    )

wrapParser ∷ Token.GenTokenParser String () Identity → M.Parser a → Parser a
wrapParser lexer p = do
  str ← many (anyChar)
  Token.whiteSpace lexer
  case M.parseNoEnv p "" (Text.pack str) of
    Right r → pure r
    Left _ → fail ""

reservedNames ∷ [String]
reservedNames = []

reservedOpNames ∷ [String]
reservedOpNames = []

michelson ∷ C.Parameterisation PrimTy PrimVal
michelson = C.Parameterisation typeOf apply parseTy parseVal reservedNames reservedOpNames
