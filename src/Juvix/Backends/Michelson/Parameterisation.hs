module Juvix.Backends.Michelson.Parameterisation
  ( module Juvix.Backends.Michelson.Parameterisation,
    module Juvix.Backends.Michelson.Compilation.Types,
  )
where

import Control.Monad.Fail (fail)
import qualified Data.Text as Text
import Juvix.Backends.Michelson.Compilation.Types
import qualified Juvix.Backends.Michelson.Contract as Contract ()
import qualified Juvix.Backends.Michelson.DSL.Environment as DSL
import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions
import qualified Juvix.Backends.Michelson.DSL.InstructionsEff as Run
import qualified Juvix.Core.ErasedAnn.Types as ErasedCoreTypes
import qualified Juvix.Core.Types as Core
import qualified Juvix.Core.Types as CoreTypes
import Juvix.Library hiding (many, try)
import qualified Michelson.Macro as M
import qualified Michelson.Parser as M
import qualified Michelson.Untyped as M
import qualified Michelson.Untyped.Type as Untyped
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

-- TODO: Add rest of primitive values.
-- TODO: Add dependent functions for pair, fst, snd, etc.
typeOf :: PrimVal -> NonEmpty PrimTy
typeOf (Constant v) = PrimTy (M.Type (constType v) "") :| []

-- constructTerm ∷ PrimVal → PrimTy
-- constructTerm (PrimConst v) = (v, Usage.Omega, PrimTy (M.Type (constType v) ""))
constType :: M.Value' Op -> M.T
constType v =
  case v of
    M.ValueInt _ -> Untyped.tint
    M.ValueUnit -> Untyped.TUnit
    M.ValueTrue -> Untyped.tbool
    M.ValueFalse -> Untyped.tbool

arity :: PrimVal -> Int
arity = pred . length . typeOf

applyProper ::
  PrimVal ->
  [PrimVal] ->
  Either
    (CoreTypes.PipelineError PrimTy PrimVal)
    (ErasedCoreTypes.PrimRetrun PrimVal)
applyProper fun args =
  case fun of
    Constant _i ->
      case length args of
        0 ->
          Right (ErasedCoreTypes.PrimRet fun)
        _x ->
          Left (CoreTypes.TypecheckerError "Applied a constant to argument")
    Inst instruction ->
      let inst = Instructions.toNumArgs instruction
       in case inst `compare` fromIntegral (length args) of
            -- we should never take more arguments than primitve could handle
            GT ->
              "applied too many arguments to a Michelson Primitive"
                |> CoreTypes.TypecheckerError
                |> Left
            LT ->
              inst - fromIntegral (length args)
                |> ErasedCoreTypes.Cont fun args
                |> Right
            -- we have exactly the right number of arguments, call the interpreter!
            EQ ->
              undefined

-- can't call it this way need to go through the top level :(
-- let (f, _) = Run.primToFargs fun undefined in
--   undefined (DSL.unFun f (undefined args))

-- TODO: Use interpreter for this, or just write it (simple enough).
-- Might need to add curried versions of built-in functions.
-- We should finish this, then we can use it in the tests.
apply :: PrimVal -> PrimVal -> Maybe PrimVal
apply t1 _t2 = Nothing
  where
    primTy :| _ = typeOf t1
    runPrim =
      DSL.execMichelson $
        --Prim.primToInstr t1 (CoreErased.PrimTy primTy)
        do undefined

parseTy :: Token.GenTokenParser String () Identity -> Parser PrimTy
parseTy lexer =
  try
    ( do
        ty <- wrapParser lexer M.type_
        pure (PrimTy ty)
    )

-- TODO: parse all values.
parseVal :: Token.GenTokenParser String () Identity -> Parser PrimVal
parseVal lexer =
  try
    ( do
        val <- wrapParser lexer M.value
        pure (Constant (M.expandValue val))
    )

wrapParser :: Token.GenTokenParser String () Identity -> M.Parser a -> Parser a
wrapParser lexer p = do
  str <- many anyChar
  Token.whiteSpace lexer
  case M.parseNoEnv p "" (Text.pack str) of
    Right r -> pure r
    Left _ -> fail ""

reservedNames :: [String]
reservedNames = []

reservedOpNames :: [String]
reservedOpNames = []

-- TODO: Figure out what the parser ought to do.
michelson :: Core.Parameterisation PrimTy PrimVal
michelson =
  Core.Parameterisation
    typeOf
    apply
    parseTy
    parseVal
    reservedNames
    reservedOpNames
