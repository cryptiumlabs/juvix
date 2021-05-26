{-# LANGUAGE UndecidableInstances #-}

module Juvix.Backends.Plonk.Parameterization
  ( hasType,
    builtinTypes,
    builtinValues,
    param,
    integerToPrimVal,
    ApplyError (..),
    arityRaw,
    toArg,
    toTakes,
    fromReturn,
    applyProper,
    evalTerm
  )
where

import Data.Field.Galois (GaloisField (..))
import qualified Data.List.NonEmpty as NonEmpty
import Juvix.Backends.Plonk.Types as Types
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.ErasedAnn.Types as CoreErased
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.ErasedAnn.Prim as Prim
import qualified Juvix.Core.Types as Core
import Juvix.Library hiding (many, show, try)
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import Prelude (Show (..))
import Debug.Pretty.Simple ( pTraceShow ) 
-- import qualified Juvix.Backends.Plonk.Compiler as Compiler
-- import qualified Juvix.Backends.Plonk.Circuit as Circuit
-- import qualified Juvix.Backends.Plonk.Builder as Builder
-- import qualified Juvix.Backends.Plonk.Circuit.Assignment as Circuit



isBool :: PrimTy f -> Bool
isBool PBool = True
isBool _ = False

hasType :: PrimVal f -> Param.PrimType (PrimTy f) -> Bool
hasType (PConst _v) ty
  | length ty == 1 = True
  | otherwise = False
-- BinOps
hasType PAdd ty = Param.check3Equal ty
hasType PSub ty = Param.check3Equal ty
hasType PMul ty = Param.check3Equal ty
hasType PDiv ty = Param.check3Equal ty
hasType PExp ty = Param.check3Equal ty
hasType PMod ty = Param.check3Equal ty
hasType PAnd ty = Param.check3Equal ty
hasType POr ty = Param.check3Equal ty
hasType PXor ty = Param.check3Equal ty
-- UnOps
hasType PIsZero ty = Param.check2Equal ty
hasType PNot ty = Param.check2Equal ty
hasType PShL ty = Param.check2Equal ty
hasType PShR ty = Param.check2Equal ty
hasType PRotL ty = Param.check2Equal ty
hasType PRotR ty = Param.check2Equal ty
hasType PAssertEq ty = Param.check2Equal ty
hasType PAssertIt ty = Param.check2Equal ty
-- CompOps
hasType PGt ty = Param.checkFirst2AndLast ty isBool
hasType PGte ty = Param.checkFirst2AndLast ty isBool
hasType PLt ty = Param.checkFirst2AndLast ty isBool
hasType PLte ty = Param.checkFirst2AndLast ty isBool
hasType PEq ty = Param.checkFirst2AndLast ty isBool

builtinTypes :: Param.Builtins (PrimTy f) -- TODO: Revisit this
builtinTypes =
  Map.fromList
    [ (NameSymbol.fromSymbol "Circuit.field", PField),
      (NameSymbol.fromSymbol "Circuit.int", PInt),
      (NameSymbol.fromSymbol "Circuit.bool", PBool)
    ]

builtinValues :: Param.Builtins (PrimVal f)
builtinValues =
  Map.fromList $
    first NameSymbol.fromSymbol
      <$> [ ("Circuit.add", PAdd),
            ("Circuit.sub", PSub),
            ("Circuit.mul", PMul),
            ("Circuit.div", PDiv),
            ("Circuit.exp", PExp),
            ("Circuit.and", PAnd),
            ("Circuit.or", POr),
            ("Circuit.xor", PXor),
            ("Circuit.eq", PEq)
          ] -- TODO: Do the rest

param :: (GaloisField f) => Param.Parameterisation (PrimTy f) (PrimVal f)
param =
  Param.Parameterisation
    { hasType,
      builtinTypes,
      builtinValues,
      stringVal = const Nothing,
      intVal = integerToPrimVal,
      floatVal = const Nothing
    }

integerToPrimVal :: forall f. GaloisField f => Integer -> Maybe (PrimVal f)
integerToPrimVal x = Just . PConst $ fromInteger x

instance Core.CanApply (PrimTy f) where
  arity (PApplication hd rest) =
    Core.arity hd - fromIntegral (length rest)
  arity x = 0 -- TODO: Refine if/when extending PrimTy

  apply (PApplication fn args1) args2 =
    PApplication fn (args1 <> args2)
      |> Right
  apply fun args =
    PApplication fun args
      |> Right

data ApplyError f
  = CompilationError CompilationError
  | ReturnTypeNotPrimitive (CoreErased.Type (PrimTy f))

instance Show f => Show (ApplyError f) where
  show (CompilationError perr) = show perr
  show (ReturnTypeNotPrimitive ty) =
    "not a primitive type:\n\t" <> show ty

arityRaw :: PrimVal f -> Natural
arityRaw = \case
  PConst _ -> 0
  PIsZero -> 1
  PNot -> 1
  PShL -> 1
  PShR -> 1
  PRotL -> 1
  PRotR -> 1
  PAssertEq -> 1
  PAssertIt -> 1
  -- BinOps
  PAdd -> 2
  PSub -> 2
  PMul -> 2
  PDiv -> 2
  PExp -> 2
  PMod -> 2
  PAnd -> 2
  POr -> 2
  PXor -> 2
  -- CompOps
  PGt -> 2
  PGte -> 2
  PLt -> 2
  PLte -> 2
  PEq -> 2

toArg :: App.Return' ext1 ty term -> Maybe (App.Arg' ext2 ty term)
toArg App.Cont {} = Nothing
toArg App.Return {retType, retTerm} =
  Just $
    App.TermArg $
      App.Take
        { usage = Usage.Omega,
          type' = retType,
          term = retTerm
        }

toTakes :: PrimVal' ext f -> (Take f, [Arg' ext f], Natural)
toTakes App.Cont {fun, args, numLeft} = (fun, args, numLeft)
toTakes App.Return {retType, retTerm} = (fun, [], arityRaw retTerm)
  where
    fun = App.Take {usage = Usage.Omega, type' = retType, term = retTerm}

fromReturn :: Return' ext f -> PrimVal' ext f
fromReturn = identity

instance (App.IsParamVar ext, Show f, Integral f, Fractional f) => Core.CanApply (PrimVal' ext f) where
  type ApplyErrorExtra (PrimVal' ext f) = ApplyError f

  type Arg (PrimVal' ext f) = Arg' ext f

  pureArg = toArg

  freeArg _ = fmap App.VarArg . App.freeVar (Proxy @ext)
  boundArg _ = fmap App.VarArg . App.boundVar (Proxy @ext)

  arity App.Cont {numLeft} = numLeft
  arity App.Return {retTerm} = arityRaw retTerm

  apply fun' args2
    | (fun, args1, ar) <- toTakes fun' =
      do
        let argLen = lengthN args2
            args = foldr NonEmpty.cons args2 args1
        case argLen `compare` ar of
          LT ->
            Right $
              App.Cont {fun, args = toList args, numLeft = ar - argLen}
          EQ
            | Just takes <- traverse App.argToTake args ->
              applyProper fun takes |> first Core.Extra
            | otherwise ->
              Right $ App.Cont {fun, args = toList args, numLeft = 0}
          GT -> Left $ Core.ExtraArguments fun' args2

applyProper :: (Show f, Integral f, Fractional f) => Take f -> NonEmpty (Take f) -> Either (ApplyError f) (Return' ext f)
applyProper fun@App.Take {usage, type', term} args = pTraceShow ("ApplyProper", fun, args) $  do
      retType <- toPrimType $ CoreErased.type' newTerm
      pure $ App.Return {
          retType, 
          retTerm = PConst $ evalTerm term (NonEmpty.toList $ App.term <$> args)
      }
  where
    fun' = takeToTerm fun
    args' = takeToTerm <$> toList args
    newTerm = applyPrimOnArgs fun' args'

evalBinOp ::(Fractional f, Integral f) => PrimVal f -> f -> f -> f 
evalBinOp op x y = case op of
  PAdd ->  x + y 
  PSub ->  x - y 
  PMul ->  x * y 
  PDiv ->  x / y
  PExp ->  x ^ y
  PMod ->  x `mod` y 
  PAnd -> if x == 0 || y == 0 then 0 else 1
  POr -> if x == 0 then y else x
  PXor -> if (x == 0 && y /= 0) || (x /= 0 && y == 0) then 1 else 0
  PGt -> if x > y then 1 else 0
  PGte -> if x >= y then 1 else 0
  PLt -> if x < y then 1 else 0
  PLte -> if x <= y then 1 else 0
  PEq -> if x == y then 1 else 0 

evalTerm :: (Fractional f, Integral f) => PrimVal f -> [PrimVal f] -> f
evalTerm val vals = case val of
  PConst f -> f
  _ | (PConst x:PConst y:_) <- vals -> evalBinOp val x y
  _ | (PConst x:_) <- vals -> panic "TODO: Implement evalUnOp" --evalUnOp val x y
  _ -> panic "Not implemented"

-- | Given a type, translate it to a type in the Plonk backend.
toPrimType :: CoreErased.Type (PrimTy f) -> Either (ApplyError f) (Param.PrimType (PrimTy f))
toPrimType ty = maybe err Right $ go ty
  where
    err = Left $ ReturnTypeNotPrimitive ty
    go ty = goPi ty <|> (pure <$> goPrim ty)
    goPi (CoreErased.Pi _ s t) = NonEmpty.cons <$> goPrim s <*> go t
    goPi _ = Nothing
    goPrim (CoreErased.PrimTy p) = Just p
    goPrim _ = Nothing

applyPrimOnArgs :: Types.AnnTerm f -> [Types.AnnTerm f] -> Types.AnnTerm f
applyPrimOnArgs prim arguments =
  let newTerm = CoreErased.AppM prim arguments
      retType = CoreErased.piToReturnType (CoreErased.type' prim)
   in CoreErased.Ann one retType newTerm

-- | Translate a 'Take' into a 'Term'.
takeToTerm :: Take f -> Types.AnnTerm f
takeToTerm App.Take {usage, type', term} =
  CoreErased.Ann {usage, type' = Prim.fromPrimType type', term = CoreErased.Prim term}

instance Eval.HasWeak (PrimTy f) where weakBy' _ _ t = t

instance Eval.HasWeak (PrimVal f) where weakBy' _ _ t = t

instance
  Monoid (IR.XVPrimTy ext (PrimTy f) primVal) =>
  Eval.HasSubstValue ext (PrimTy f) primVal (PrimTy f)
  where
  substValueWith _ _ _ t = pure $ IR.VPrimTy' t mempty

instance
  Monoid (IR.XPrimTy ext (PrimTy f) primVal) =>
  Eval.HasPatSubstTerm ext (PrimTy f) primVal (PrimTy f)
  where
  patSubstTerm' _ _ t = pure $ IR.PrimTy' t mempty

instance
  Monoid (IR.XPrim ext primTy (PrimVal f)) =>
  Eval.HasPatSubstTerm ext primTy (PrimVal f) (PrimVal f)
  where
  patSubstTerm' _ _ t = pure $ IR.Prim' t mempty
