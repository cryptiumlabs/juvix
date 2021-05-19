{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Juvix.Core.Parameterisations.All
  ( Ty (..),
    Val (..),
    t,
  )
where

import Data.Bitraversable
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.IR.Evaluator as E
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Core.Parameterisations.Naturals as Naturals
import qualified Juvix.Core.Parameterisations.Unit as Unit
import Juvix.Library hiding ((<|>))

-- all primitive types
data Ty
  = NatTy Naturals.Ty
  | UnitTy Unit.Ty
  deriving (Show, Eq)

-- c: primitive constant and f: functions
data Val
  = NatVal Naturals.Val
  | UnitVal Unit.Val
  deriving (Show, Eq)

unNatTy :: Ty -> Maybe Naturals.Ty
unNatTy (NatTy t) = pure t
unNatTy _ = empty

unUnitTy :: Ty -> Maybe Unit.Ty
unUnitTy (UnitTy t) = pure t
unUnitTy _ = empty

hasType :: Val -> P.PrimType Ty -> Bool
hasType (NatVal x) (traverse unNatTy -> Just tys) = Naturals.hasType x tys
hasType (UnitVal x) (traverse unUnitTy -> Just tys) = Unit.hasType x tys
hasType _ _ = False

instance P.CanApply Ty where
  arity _ = 0
  apply f xs = Left $ P.ExtraArguments f xs

instance P.CanApply Val where
  arity (NatVal x) = P.arity x
  arity (UnitVal x) = P.arity x

  apply (NatVal f) (traverse unNatVal -> Just xs) =
    P.mapApplyErr NatVal $ P.apply f xs
  apply f xs = Left $ P.InvalidArguments f xs

instance P.CanApply (P.TypedPrim Ty Val) where
  arity (App.Cont {numLeft}) = numLeft
  arity (App.Return {retTerm}) = P.arity retTerm

  apply f' xs'
    | Just f <- unNatValR f',
      Just xs <- traverse unNatValR xs' =
      P.mapApplyErr natValR $ P.apply f xs
  apply f xs = Left $ P.InvalidArguments f xs

instance E.HasWeak Ty where weakBy' _ _ ty = ty

instance Monoid (IR.XVPrimTy ext Ty val) => E.HasSubstValue ext Ty val Ty where
  substValueWith _ _ _ ty = pure $ IR.VPrimTy' ty mempty

instance Monoid (IR.XPrimTy ext Ty val) => E.HasPatSubstTerm ext Ty val Ty where
  patSubstTerm' _ _ ty = pure $ IR.PrimTy' ty mempty

instance E.HasWeak Val where weakBy' _ _ val = val

instance Monoid (IR.XVPrim ext ty Val) => E.HasSubstValue ext ty Val Val where
  substValueWith _ _ _ val = pure $ IR.VPrim' val mempty

instance Monoid (IR.XPrim ext Ty Val) => E.HasPatSubstTerm ext Ty Val Val where
  patSubstTerm' _ _ val = pure $ IR.Prim' val mempty

natValR :: P.TypedPrim Naturals.Ty Naturals.Val -> P.TypedPrim Ty Val
natValR (App.Cont {fun, args, numLeft}) =
  App.Cont {App.fun = natValT fun, App.args = natValA <$> args, numLeft}
natValR (App.Return {retType, retTerm}) =
  App.Return {retType = NatTy <$> retType, retTerm = NatVal retTerm}

natValT ::
  App.Take (P.PrimType Naturals.Ty) Naturals.Val ->
  App.Take (P.PrimType Ty) Val
natValT (App.Take {usage, type', term}) =
  App.Take {usage, type' = NatTy <$> type', term = NatVal term}

natValA ::
  App.Arg (P.PrimType Naturals.Ty) Naturals.Val ->
  App.Arg (P.PrimType Ty) Val
natValA = bimap (fmap NatTy) NatVal

unNatValR :: P.TypedPrim Ty Val -> Maybe (P.TypedPrim Naturals.Ty Naturals.Val)
unNatValR (App.Cont {fun, args, numLeft}) =
  App.Cont <$> unNatValT fun <*> traverse unNatValA args <*> pure numLeft
unNatValR (App.Return {retType = t', retTerm = NatVal v})
  | Just t <- traverse unNatTy t' =
    Just $ App.Return t v
unNatValR (App.Return {}) = Nothing

unNatValT ::
  App.Take (P.PrimType Ty) Val ->
  Maybe (App.Take (P.PrimType Naturals.Ty) Naturals.Val)
unNatValT (App.Take {usage, type' = type'', term = term'})
  | Just type' <- traverse unNatTy type'',
    Just term <- unNatVal term' =
    Just $ App.Take {usage, type', term}
unNatValT (App.Take {}) = Nothing

unNatValA ::
  App.Arg (P.PrimType Ty) Val ->
  Maybe (App.Arg (P.PrimType Naturals.Ty) Naturals.Val)
unNatValA = bitraverse (traverse unNatTy) unNatVal

unNatVal :: Val -> Maybe Naturals.Val
unNatVal (NatVal n) = Just n
unNatVal _ = Nothing

builtinTypes :: P.Builtins Ty
builtinTypes =
  fmap NatTy Naturals.builtinTypes
    <> fmap UnitTy Unit.builtinTypes

builtinValues :: P.Builtins Val
builtinValues =
  fmap NatVal Naturals.builtinValues
    <> fmap UnitVal Unit.builtinValues

t :: P.Parameterisation Ty Val
t =
  P.Parameterisation
    { hasType,
      builtinTypes,
      builtinValues,
      stringVal = const Nothing,
      intVal = fmap NatVal . Naturals.natVal,
      floatVal = const Nothing
    }
