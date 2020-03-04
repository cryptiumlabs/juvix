module Juvix.Backends.Michelson.Parameterisation
  ( module Juvix.Backends.Michelson.Parameterisation,
    module Juvix.Backends.Michelson.Compilation.Types,
  )
where

import Control.Monad.Fail (fail)
import qualified Data.Text as Text
import Juvix.Backends.Michelson.Compilation.Types
import qualified Juvix.Backends.Michelson.Contract as Contract ()
import qualified Juvix.Core.ErasedAnn.Types as CoreErased
import qualified Juvix.Core.Types as Core
import Juvix.Library hiding (many, try)
import qualified Michelson.Macro as M
import qualified Michelson.Parser as M
import qualified Michelson.Untyped as M
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

-- TODO: Add rest of primitive values.
-- TODO: Add dependent functions for pair, fst, snd, etc.
typeOf ∷ PrimVal → NonEmpty PrimTy
typeOf (Constant v) = PrimTy (M.Type (constType v) "") :| []

-- constructTerm ∷ PrimVal → PrimTy
-- constructTerm (PrimConst v) = (v, Usage.Omega, PrimTy (M.Type (constType v) ""))

constType ∷ M.Value' Op → M.T
constType v =
  case v of
    M.ValueInt _ → M.Tc M.CInt
    M.ValueUnit → M.TUnit
    M.ValueTrue → M.Tc M.CBool
    M.ValueFalse → M.Tc M.CBool

arity ∷ PrimVal → Int
arity = pred . length . typeOf

-- TODO: Use interpreter for this, or just write it (simple enough).
-- Might need to add curried versions of built-in functions.
apply ∷ PrimVal → PrimVal → Maybe PrimVal
apply t1 _t2 = Nothing
  where
    primTy :| _ = typeOf t1

{-
runPrim = Env.execWithStack mempty $ do
  --Prim.primToInstr t1 (CoreErased.PrimTy primTy)
  undefined
-}

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
        pure (Constant (M.expandValue val))
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

michelson ∷ Core.Parameterisation PrimTy PrimVal
michelson =
  Core.Parameterisation
    typeOf
    apply
    parseTy
    parseVal
    reservedNames
    reservedOpNames
