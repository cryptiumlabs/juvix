{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}

module Juvix.Backends.Plonk.Parameterization
  ( module Juvix.Backends.Plonk.Parameterization,
    module Types,
  )
where

import Data.Field.Galois (GaloisField(..))
import qualified Data.List.NonEmpty as NonEmpty
import Juvix.Backends.Plonk.Types as Types
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.ErasedAnn.Types as ErasedAnn
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.Types as Core
import Juvix.Library hiding (many, show, try)
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import Prelude (Show (..))

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

isBool :: PrimTy f -> Bool
isBool PBool = True
isBool _ = False

checkFirst2AndLast :: Eq t => NonEmpty t -> (t -> Bool) -> Bool
checkFirst2AndLast (x :| [y, last]) check
  | check2Equal (x :| [y]) && check last = True
  | otherwise = False
checkFirst2AndLast (_ :| _) _ = False

hasType :: PrimVal f -> Param.PrimType (PrimTy f) -> Bool
hasType (PConst _v) ty
  | length ty == 1 = True
  | otherwise = False
-- BinOps
hasType PAdd ty = check3Equal ty
hasType PSub ty = check3Equal ty
hasType PMul ty = check3Equal ty
hasType PDiv ty = check3Equal ty
hasType PExp ty = check3Equal ty
hasType PMod ty = check3Equal ty
hasType PAnd ty = check3Equal ty
hasType POr ty = check3Equal ty
hasType PXor ty = check3Equal ty
-- UnOps
hasType PDup ty = check2Equal ty
hasType PIsZero ty = check2Equal ty
hasType PNot ty = check2Equal ty
hasType PShL ty = check2Equal ty
hasType PShR ty = check2Equal ty
hasType PRotL ty = check2Equal ty
hasType PRotR ty = check2Equal ty
hasType PAssertEq ty = check2Equal ty
hasType PAssertIt ty = check2Equal ty
-- CompOps
hasType PGt ty = checkFirst2AndLast ty isBool
hasType PGte ty = checkFirst2AndLast ty isBool
hasType PLt ty = checkFirst2AndLast ty isBool
hasType PLte ty = checkFirst2AndLast ty isBool
hasType PEq ty = checkFirst2AndLast ty isBool

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

plonk :: (GaloisField f) => Param.Parameterisation (PrimTy f) (PrimVal f)
plonk =
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
  | ReturnTypeNotPrimitive (ErasedAnn.Type (PrimTy f))

instance Show (ApplyError f) where
  show (CompilationError perr) = show perr
  show (ReturnTypeNotPrimitive ty) =
    "not a primitive type:\n\t" <> show ty

arityRaw :: PrimVal f -> Natural
arityRaw = \case
   PConst _ -> 0
   PDup -> 1
   PIsZero -> 1
   PNot -> 1
   PShL -> 1
   PShR-> 1
   PRotL-> 1
   PRotR-> 1
   PAssertEq-> 1
   PAssertIt-> 1
   -- BinOps
   PAdd-> 2
   PSub-> 2
   PMul-> 2
   PDiv-> 2
   PExp-> 2
   PMod-> 2
   PAnd-> 2
   POr-> 2
   PXor-> 2
   -- CompOps
   PGt-> 2
   PGte-> 2
   PLt-> 2
   PLte-> 2
   PEq-> 2

toArg :: App.Return' ext1 ty term -> Maybe (App.Arg' ext2 ty term)
toArg App.Cont {} = Nothing
toArg App.Return {retType, retTerm} =
  Just
    $ App.TermArg
    $ App.Take
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

instance App.IsParamVar ext => Core.CanApply (PrimVal' ext f) where
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

applyProper :: Take f -> NonEmpty (Take f) -> Either (ApplyError f) (Return' ext f)
applyProper fun args = panic "Apply proper not implemented"

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
