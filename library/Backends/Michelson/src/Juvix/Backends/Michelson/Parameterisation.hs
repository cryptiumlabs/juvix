{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}

module Juvix.Backends.Michelson.Parameterisation
  ( module Juvix.Backends.Michelson.Parameterisation,
    module Types,
  )
where

import qualified Control.Arrow as Arr
import Control.Monad.Fail (fail)
import Data.Foldable (foldr1) -- on NonEmpty
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Juvix.Backends.Michelson.Compilation as Compilation
import Juvix.Backends.Michelson.Compilation.Types as Types
import qualified Juvix.Backends.Michelson.Compilation.Types as CompTypes
import qualified Juvix.Backends.Michelson.Contract as Contract ()
import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions
import qualified Juvix.Backends.Michelson.DSL.InstructionsEff as Run
import qualified Juvix.Backends.Michelson.DSL.Interpret as Interpreter
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.ErasedAnn.Prim as Prim
import qualified Juvix.Core.ErasedAnn.Types as ErasedAnn
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.IR.Typechecker.Types as TC
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Core.Types as Core
import Juvix.Library hiding (many, try)
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified Michelson.Macro as M
import qualified Michelson.Parser as M
import qualified Michelson.Text as M
import qualified Michelson.Untyped as M
import qualified Michelson.Untyped.Type as Untyped
import Text.ParserCombinators.Parsec hiding ((<|>))
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (Show (..), String)

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

arityRaw :: RawPrimVal -> Natural
arityRaw (Inst inst) = fromIntegral (Instructions.toNumArgs inst)
arityRaw (Constant _) = 0
arityRaw prim = Run.instructionOf prim |> Instructions.toNewPrimErr |> arityRaw

data ApplyError
  = CompilationError CompilationError
  | ReturnTypeNotPrimitive (ErasedAnn.Type PrimTy)

instance Show ApplyError where
  show (CompilationError perr) = Prelude.show perr
  show (ReturnTypeNotPrimitive ty) =
    "not a primitive type:\n\t" <> Prelude.show ty

instance Core.CanApply PrimTy where
  arity (Application hd rest) =
    Core.arity hd - fromIntegral (length rest)
  arity x =
    Run.lengthType x

  apply (Application fn args1) args2 =
    Application fn (args1 <> args2)
      |> Right
  apply fun args =
    Application fun args
      |> Right

instance Core.CanApply (PrimVal' ext) where
  type ApplyErrorExtra (PrimVal' ext) = ApplyError

  arity Prim.Cont {numLeft} = numLeft
  arity Prim.Return {retTerm} = arityRaw retTerm

  apply fun' args2'
    | (fun, args1, ar) <- toTakes fun',
      Just args2 <- traverse toArg args2' =
      do
        let argLen = lengthN args2'
            args = foldr NonEmpty.cons args2 args1
        case argLen `compare` ar of
          LT ->
            Right $
              Prim.Cont {fun, args = toList args, numLeft = ar - argLen}
          EQ
            | Just takes <- traverse (traverse App.argToBase) args ->
              applyProper fun takes |> first Core.Extra
            | otherwise ->
              Right $ Prim.Cont {fun, args = toList args, numLeft = 0}
          GT -> Left $ Core.ExtraArguments fun' args2'
  apply fun args = Left $ Core.InvalidArguments fun args

-- | NB. requires that the right number of args are passed
applyProper :: Take -> NonEmpty Take -> Either ApplyError (Return' ext)
applyProper fun args =
  case compd >>= Interpreter.dummyInterpret of
    Right x -> do
      retType <- toPrimType $ ErasedAnn.type' newTerm
      pure $ Prim.Return {retType, retTerm = Constant x}
    Left err -> Left $ CompilationError err
  where
    fun' = takeToTerm fun
    args' = takeToTerm <$> toList args
    newTerm = Run.applyPrimOnArgs fun' args'
    -- TODO ∷ do something with the logs!?
    (compd, _log) = Compilation.compileExpr newTerm

takeToTerm :: Take -> RawTerm
takeToTerm (Prim.Take {usage, type', term}) =
  Ann {usage, type' = Prim.fromPrimType type', term = ErasedAnn.Prim term}

toPrimType :: ErasedAnn.Type PrimTy -> Either ApplyError (P.PrimType PrimTy)
toPrimType ty = maybe err Right $ go ty
  where
    err = Left $ ReturnTypeNotPrimitive ty
    go ty = goPi ty <|> (pure <$> goPrim ty)
    goPi (ErasedAnn.Pi _ s t) = NonEmpty.cons <$> goPrim s <*> go t
    goPi _ = Nothing
    goPrim (ErasedAnn.PrimTy p) = Just p
    goPrim _ = Nothing

parseTy :: Token.GenTokenParser String () Identity -> Parser PrimTy
parseTy lexer =
  try
    ( do
        ty <- wrapParser lexer M.type_
        pure (PrimTy ty)
    )

-- TODO: parse all values.
parseVal :: Token.GenTokenParser String () Identity -> Parser RawPrimVal
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

integerToPrimVal :: Integer -> Maybe RawPrimVal
integerToPrimVal x
  | x >= toInteger (minBound @Int),
    x <= toInteger (maxBound @Int) =
    Just $ Constant $ M.ValueInt $ fromInteger x
  | otherwise =
    Nothing

checkStringType :: Text -> PrimTy -> Bool
checkStringType val (PrimTy (M.Type ty _)) = case ty of
  M.TString -> Text.all M.isMChar val
  -- TODO other cases?
  _ -> False
checkStringType _ _ = False

checkIntType :: Integer -> PrimTy -> Bool
checkIntType val (PrimTy (M.Type ty _)) = case ty of
  M.TNat -> val >= 0 -- TODO max bound
  M.TInt -> True -- TODO bounds?
    -- TODO other cases?
  _ -> False
checkIntType _ _ = False

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
    ("Michelson.bytes", Untyped.TBytes),
    ("Michelson.mutez", Untyped.TMutez),
    ("Michelson.bool", Untyped.TBool),
    ("Michelson.key-hash", Untyped.TKeyHash),
    ("Michelson.timestamp", Untyped.TTimestamp),
    ("Michelson.address", Untyped.TAddress)
  ]
    |> fmap (NameSymbol.fromSymbol Arr.*** primify)
    |> ( <>
           [ ("Michelson.list", Types.List),
             ("Michelson.lambda", Types.Lambda),
             ("Michelson.option", Types.Option),
             ("Michelson.set", Types.Set),
             ("Michelson.map", Types.Map),
             ("Michelson.big-map", Types.BigMap)
           ]
       )
    |> Map.fromList

builtinValues :: P.Builtins RawPrimVal
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
    ("Michelson.nil", Nil),
    ("Michelson.cons", Cons),
    ("Michelson.none", None),
    ("Michelson.left", Left'),
    ("Michelson.right", Right'),
    ("Michelson.map", MapOp),
    ("Michelson.empty-set", EmptyS),
    ("Michelson.empty-map", EmptyM),
    ("Michelson.empty-big-map", EmptyBM),
    ("Michelson.cons-pair", Pair'),
    -- added symbols to not take values
    ("Michelson.if-builtin", Inst (M.IF [] []))
  ]
    |> fmap (first NameSymbol.fromSymbol)
    |> Map.fromList

michelson :: P.Parameterisation PrimTy RawPrimVal
michelson =
  P.Parameterisation
    { hasType,
      builtinTypes,
      builtinValues,
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

instance Eval.HasWeak PrimTy where weakBy' _ _ t = t

instance Eval.HasWeak RawPrimVal where weakBy' _ _ t = t

instance
  Monoid (IR.XVPrimTy ext PrimTy primVal) =>
  Eval.HasSubstValue ext PrimTy primVal PrimTy
  where
  substValueWith _ _ _ t = pure $ IR.VPrimTy' t mempty

instance
  ( Monoid (IR.XPrimTy ext PrimTy primVal),
    Monoid (IR.XAnn ext PrimTy primVal),
    Monoid (IR.XStar ext PrimTy primVal)
  ) =>
  Eval.HasPatSubstElim ext PrimTy primVal PrimTy
  where
  patSubstElim' _ _ t =
    pure $ IR.Ann' mempty (IR.PrimTy' t mempty) (IR.Star' 0 mempty) 1 mempty
