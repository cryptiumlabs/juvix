{-# OPTIONS_GHC -fdefer-typed-holes #-}

{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}

module Juvix.Backends.Michelson.Parameterisation
  ( module Juvix.Backends.Michelson.Parameterisation,
    module Juvix.Backends.Michelson.Compilation.Types,
  )
where

import qualified Control.Arrow as Arr
import Control.Monad.Fail (fail)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Juvix.Backends.Michelson.Compilation as Compilation
import Juvix.Backends.Michelson.Compilation.Types
import qualified Juvix.Backends.Michelson.Compilation.Types as CompTypes
import qualified Juvix.Backends.Michelson.Contract as Contract ()
import qualified Juvix.Backends.Michelson.DSL.Environment as DSL
import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions
import qualified Juvix.Backends.Michelson.DSL.InstructionsEff as Run
import qualified Juvix.Backends.Michelson.DSL.Interpret as Interpreter
import qualified Juvix.Core.ErasedAnn.Prim as Prim
import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Core.Types as Core
import Juvix.Library hiding (many, try)
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Michelson.Macro as M
import qualified Michelson.Parser as M
import qualified Michelson.Text as M
import qualified Michelson.Untyped as M
import qualified Michelson.Untyped.Type as Untyped
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

-- TODO ∷ refactor this all to not be so bad
-- DO EXTRA CHECKS
check3Equal :: Eq a => NonEmpty a -> Bool
check3Equal (x :| [y, z])
  | x == y && x == z = True
  | otherwise = False
check3Equal (_ :| _) = False

check2Equal :: Eq a => NonEmpty a -> Bool
check2Equal (x :| [y])
  | x == y = True
  | otherwise = False
check2Equal (_ :| _) = False

isBool :: PrimTy -> Bool
isBool (PrimTy (M.Type M.TBool _)) = True
isBool _ = False

checkFirst2AndLast :: Eq t => NonEmpty t -> (t -> Bool) -> Bool
checkFirst2AndLast (x :| [y, last]) check
  | check2Equal (x :| [y]) && check last = True
  | otherwise = False
checkFirst2AndLast (_ :| _) _ = False

hasType :: RawPrimVal -> P.PrimType PrimTy -> Bool
hasType AddTimeStamp ty = check3Equal ty
hasType AddI ty = check3Equal ty
hasType AddN ty = check3Equal ty
hasType SubI ty = check3Equal ty
hasType SubN ty = check3Equal ty
hasType SubTimeStamp ty = check3Equal ty
hasType MulI ty = check3Equal ty
hasType MulN ty = check3Equal ty
hasType MulMutez ty = check3Equal ty
hasType ORI ty = check3Equal ty
hasType OrB ty = check3Equal ty
hasType AndI ty = check3Equal ty
hasType AndB ty = check3Equal ty
hasType XorI ty = check3Equal ty
hasType XorB ty = check3Equal ty
hasType NotI ty = check2Equal ty
hasType NotB ty = check2Equal ty
hasType CompareI ty = checkFirst2AndLast ty isBool
hasType CompareS ty = checkFirst2AndLast ty isBool
hasType CompareP ty = checkFirst2AndLast ty isBool
hasType CompareTimeStamp ty = checkFirst2AndLast ty isBool
hasType CompareMutez ty = checkFirst2AndLast ty isBool
hasType CompareHash ty = checkFirst2AndLast ty isBool
hasType (Inst (M.IF _ _)) (bool :| rest)
  | empty == rest = False
  | otherwise = isBool bool && check2Equal (NonEmpty.fromList rest)
hasType (Constant _v) ty
  | length ty == 1 = True
  | otherwise = False
hasType x ty = ty == undefined

-- constructTerm ∷ PrimVal → PrimTy
-- constructTerm (PrimConst v) = (v, Usage.Omega, PrimTy (M.Type (constType v) ""))

-- Arity for constant is BAD, refactor to handle lambda
arityV :: PrimVal -> Int
arityV (Inst inst) = fromIntegral (Instructions.toNumArgs inst)
arityV (Constant _) = 0
arityV prim =
  Run.instructionOf prim |> Instructions.toNewPrimErr |> arity

applyV :: PrimVal -> NonEmpty PrimVal -> Maybe PrimVal
applyV fun' args
  | (fun, args1, ar) <- toTakes fun',
    ar == fromIntegral (length args),
    Just args2 <- traverse toTake1 args
  = either (const Nothing) (Just . fromReturn) $
    applyProper fun (args1 <> toList args2)
    -- FIXME keep errors
applyV _ _ = Nothing

arityT :: PrimTy -> Natural
arityT (PrimTy {}) = 0

applyT :: PrimTy -> NonEmpty PrimTy -> Maybe PrimTy
applyT (PrimTy {}) _ = Nothing

type ApplyError = Core.PipelineError PrimTy PrimVal CompilationError

applyProper ::
  Prim.Take PrimTy PrimVal ->
  [Prim.Take PrimTy PrimVal] ->
  Either ApplyError (Prim.Return PrimTy PrimVal)
applyProper fun@(Prim.Take {type'=funTy, term=funTm}) args =
  case Prim.term fun of
    Constant _i ->
      case length args of
        0 -> Right $ Prim.Return {retType = funTy, retTerm = funTm}
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
                    Right x -> Right $ Prim.Return {
                      retType = ErasedAnn.type' newTerm,
                      retTerm = Constant x
                    }
                    -- TODO :: promote this error
                    Left err -> Core.PrimError err |> Left
    x ->
      applyProper (fun {Prim.term = Run.newPrimToInstrErr x}) args

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
    ("Michelson.unit", Inst (M.UNIT "" "")),
    -- added symbols to not take values
    ("Michelson.if", Inst (M.IF [] []))
  ]
    |> fmap (first NameSymbol.fromSymbol)
    |> Map.fromList

-- TODO: Figure out what the parser ought to do.
michelson :: P.Parameterisation PrimTy PrimVal
michelson =
  P.Parameterisation
    { hasType,
      builtinTypes,
      arityT,
      applyT,
      builtinValues,
      arityV = arityRaw,
      applyV = _,
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
