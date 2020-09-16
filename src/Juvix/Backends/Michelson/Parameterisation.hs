{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}

module Juvix.Backends.Michelson.Parameterisation
  ( module Juvix.Backends.Michelson.Parameterisation,
    module Juvix.Backends.Michelson.Compilation.Types,
  )
where

import qualified Control.Arrow as Arr
import Control.Monad.Fail (fail)
import qualified Data.Text as Text
import qualified Juvix.Backends.Michelson.Compilation as Compilation
import qualified Juvix.Backends.Michelson.Compilation.Types as CompTypes
import Juvix.Backends.Michelson.Compilation.Types
import qualified Juvix.Backends.Michelson.Contract as Contract ()
import qualified Juvix.Backends.Michelson.DSL.Environment as DSL
import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions
import qualified Juvix.Backends.Michelson.DSL.InstructionsEff as Run
import qualified Juvix.Backends.Michelson.DSL.Interpret as Interpreter
import qualified Juvix.Core.Common.NameSymbol as NameSymbol
import qualified Juvix.Core.ErasedAnn.Prim as Prim
import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Core.Types as Core
import Juvix.Library hiding (many, try)
import qualified Juvix.Library.HashMap as Map
import qualified Michelson.Macro as M
import qualified Michelson.Parser as M
import qualified Michelson.Text as M
import qualified Michelson.Untyped as M
import qualified Michelson.Untyped.Type as Untyped
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

-- TODO: Add rest of primitive values.
-- TODO: Add dependent functions for pair, fst, snd, etc.
typeOf :: PrimVal -> NonEmpty PrimTy
typeOf (Constant v) = PrimTy (M.Type (constType v) "") :| []
typeOf AddI = PrimTy (M.Type M.TInt "") :| [PrimTy (M.Type M.TInt ""), PrimTy (M.Type M.TInt "")]

hasType :: PrimVal -> P.PrimType PrimTy -> Bool
hasType x ty = ty == typeOf x

-- constructTerm ∷ PrimVal → PrimTy
-- constructTerm (PrimConst v) = (v, Usage.Omega, PrimTy (M.Type (constType v) ""))
constType :: M.Value' Op -> M.T
constType v =
  case v of
    M.ValueInt _ -> Untyped.tint
    M.ValueUnit -> Untyped.TUnit
    M.ValueTrue -> Untyped.tbool
    M.ValueFalse -> Untyped.tbool

-- the arity elsewhere lacks this 'pred'?
arity :: PrimVal -> Int
arity = pred . length . typeOf

applyProper ::
  Prim.Take PrimTy PrimVal ->
  [Prim.Take PrimTy PrimVal] ->
  Either
    (Core.PipelineError PrimTy PrimVal CompilationError)
    (Prim.Return PrimTy PrimVal)
applyProper fun args =
  case Prim.term fun of
    Constant _i ->
      case length args of
        0 ->
          Right (Prim.Return (Prim.term fun))
        _x ->
          Left (Core.PrimError AppliedConstantToArgument)
    Inst instruction ->
      let inst = Instructions.toNumArgs instruction
       in case inst `compare` fromIntegral (length args) of
            -- we should never take more arguments than primitve could handle
            GT ->
              Left (Core.PrimError TooManyArguments)
            LT ->
              inst - fromIntegral (length args)
                |> Prim.Cont fun args
                |> Right
            -- we have exactly the right number of arguments, call the interpreter!
            EQ ->
              let newTerm =
                    Run.applyPrimOnArgs (Prim.toAnn fun) (Prim.toAnn <$> args)
                  -- TODO ∷ do something with the logs!?
                  (compd, _log) = Compilation.compileExpr newTerm
               in case compd >>= Interpreter.dummyInterpret of
                    Right x ->
                      Constant x
                        |> Prim.Return
                        |> Right
                    -- TODO :: promote this error
                    Left err -> Core.PrimError err |> Left
    x ->
      applyProper (fun {Prim.term = Run.newPrimToInstrErr x}) args

-- translate our code into a valid form

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

integerToPrimVal :: Integer -> Maybe PrimVal
integerToPrimVal x
  | x >= toInteger (minBound @Int),
    x <= toInteger (maxBound @Int) =
    Just $ Constant $ M.ValueInt $ fromInteger x
  | otherwise =
    Nothing

checkStringType :: Text -> PrimTy -> Bool
checkStringType val (PrimTy (M.Type ty _)) = case ty of
  M.TString -> Text.all M.isMChar val
  _ -> False

checkIntType :: Integer -> PrimTy -> Bool
checkIntType val (PrimTy (M.Type ty _)) = case ty of
  M.TNat -> val >= 0 -- TODO max bound
  M.TInt -> True -- TODO bounds?
  _ -> False

primify :: Untyped.T -> PrimTy
primify t = PrimTy (Untyped.Type t "")

builtinTypes :: P.Builtins PrimTy
builtinTypes =
  [ ("Michelson.unit-t", Untyped.TUnit),
    ("Michelson.key", Untyped.TKey),
    ("Michelson.signature", Untyped.TSignature),
    ("Michelson.chain-id", Untyped.TChainId),
    ("Michelson.int", Untyped.TInt),
    ("Michelson.nat", Untyped.TNat),
    ("Michelson.string", Untyped.TString),
    ("Michelson.string", Untyped.TBytes),
    ("Michelson.mutez", Untyped.TMutez),
    ("Michelson.bool", Untyped.TBool),
    ("Michelson.key-hash", Untyped.TKeyHash),
    ("Michelson.timestamp", Untyped.TTimestamp),
    ("Michelson.address", Untyped.TAddress)
  ]
    |> fmap (NameSymbol.fromSymbol Arr.*** primify)
    |> Map.fromList

builtinValues :: P.Builtins PrimVal
builtinValues =
  [ ("Michelson.add", AddI),
    ("Michelson.sub", SubI),
    ("Michelson.mul", MulI),
    ("Michelson.div", EDivI),
    ("Michelson.now", Inst (M.NOW "")),
    ("Michelson.cons", Inst (M.CONS "")),
    ("Michelson.car", Inst (M.CAR "" "")),
    ("Michelson.cdr", Inst (M.CDR "" "")),
    ("Michelson.some", Inst (M.SOME "" "")),
    ("Michelson.sha256", Inst (M.SHA256 "")),
    ("Michelson.sha512", Inst (M.SHA512 "")),
    ("Michelson.source", Inst (M.SOURCE "")),
    ("Michelson.get", Inst (M.GET "")),
    ("Michelson.update", Inst (M.UPDATE "")),
    ("Michelson.size", SizeS),
    ("Michelson.blake2b", Inst (M.BLAKE2B "")),
    ("Michelson.abs", Inst (M.ABS "")),
    ("Michelson.now", Inst (M.NOW "")),
    ("Michelson.source", Inst (M.SOURCE "")),
    ("Michelson.sender", Inst (M.SENDER "")),
    ("Michelson.set-delegate", Inst (M.SET_DELEGATE "")),
    ("Michelson.transfer-tokens", Inst (M.TRANSFER_TOKENS "")),
    ("Michelson.compare", CompareI),
    ("Michelson.amount", Inst (M.AMOUNT "")),
    ("Michelson.balance", Inst (M.BALANCE "")),
    ("Michelson.hash-key", Inst (M.HASH_KEY "")),
    ("Michelson.and", AndI),
    ("Michelson.xor", XorI),
    ("Michelson.or", OrB),
    ("Michelson.mem", MemMap),
    ("Michelson.concat", Inst (M.CONCAT "")),
    ("Michelson.slice", Inst (M.SLICE "")),
    ("Michelson.lsl", Inst (M.LSL "")),
    ("Michelson.lsr", Inst (M.LSR "")),
    ("Michelson.fail-with", Inst M.FAILWITH),
    ("Michelson.self", Inst (M.SELF "" "")),
    ("Michelson.self", Inst (M.UNIT "" ""))
  ]
    |> fmap (first NameSymbol.fromSymbol)
    |> Map.fromList

-- TODO: Figure out what the parser ought to do.
michelson :: P.Parameterisation PrimTy PrimVal
michelson =
  P.Parameterisation
    { hasType,
      builtinTypes,
      builtinValues,
      arity,
      apply,
      parseTy,
      parseVal,
      reservedNames,
      reservedOpNames,
      stringTy = checkStringType,
      stringVal = Just . Constant . M.ValueString . M.mkMTextUnsafe, -- TODO ?
      intTy = checkIntType,
      intVal = integerToPrimVal,
      floatTy = \_ _ -> False, -- Michelson does not support floats
      floatVal = const Nothing
    }

type CompErr = CompTypes.CompilationError
