{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Typechecker.Types where

import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.IR.Types.Base
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (show)
import Prelude (Show (..))

data Annotation' ext primTy primVal = Annotation
  { annUsage :: Usage.T,
    annType :: IR.Value' ext primTy primVal
  }

type Annotation = Annotation' IR.NoExt

deriving instance
  (Eq (IR.Value' ext primTy primVal)) =>
  Eq (Annotation' ext primTy primVal)

deriving instance
  (Show (IR.Value' ext primTy primVal)) =>
  Show (Annotation' ext primTy primVal)

data ContextElement' ext primTy primVal = ContextElement
  { ctxName :: IR.Name,
    ctxAnn :: {-# UNPACK #-} !(Annotation' ext primTy primVal)
  }

type ContextElement = ContextElement' IR.NoExt

contextElement ::
  IR.Name ->
  Usage.T ->
  IR.Value' ext primTy primVal ->
  ContextElement' ext primTy primVal
contextElement n π t = ContextElement n (Annotation π t)

deriving instance
  (Eq (IR.Value' ext primTy primVal)) =>
  Eq (ContextElement' ext primTy primVal)

deriving instance
  (Show (IR.Value' ext primTy primVal)) =>
  Show (ContextElement' ext primTy primVal)

type Context' ext primTy primVal = [ContextElement' ext primTy primVal]

type Context primTy primVal = Context' IR.NoExt primTy primVal

lookupCtx ::
  IR.Name ->
  Context' ext primTy primVal ->
  Maybe (Annotation' ext primTy primVal)
lookupCtx x = fmap ctxAnn . find (\e -> ctxName e == x)

data TypecheckError' ext primTy primVal
  = TypeMismatch
      Natural
      (IR.Term' ext primTy primVal)
      (IR.Value' ext primTy primVal)
      (IR.Value' ext primTy primVal)
  | UniverseMismatch Natural Natural
  | CannotApply
      (IR.Value' ext primTy primVal)
      (IR.Value' ext primTy primVal)
  | ShouldBeStar (IR.Value' ext primTy primVal)
  | ShouldBeFunctionType
      (IR.Value' ext primTy primVal)
      (IR.Term' ext primTy primVal)
  | UnboundIndex Natural
  | SigmaMustBeZero
  | UsageMustBeZero
  | UsageNotCompatible Usage.T Usage.T
  | UnboundBinder Natural IR.Name
  | MustBeFunction
      (IR.Elim' ext primTy primVal)
      Natural
      (IR.Term' ext primTy primVal)
  | BoundVariableCannotBeInferred

type TypecheckError = TypecheckError' IR.NoExt

deriving instance
  ( Eq primTy,
    Eq primVal,
    IR.TermAll Eq ext primTy primVal,
    IR.ElimAll Eq ext primTy primVal,
    IR.ValueAll Eq ext primTy primVal,
    IR.NeutralAll Eq ext primTy primVal
  ) =>
  Eq (TypecheckError' ext primTy primVal)

instance
  ( Show primTy,
    Show primVal,
    IR.TermAll Show ext primTy primVal,
    IR.ElimAll Show ext primTy primVal,
    IR.ValueAll Show ext primTy primVal,
    IR.NeutralAll Show ext primTy primVal
  ) =>
  Show (TypecheckError' ext primTy primVal)
  where
  show (TypeMismatch binder term expectedT gotT) =
    "Type mismatched. \n" <> show term <> " \n (binder number " <> show binder
      <> ") is of type \n"
      <> show gotT
      <> ".\nBut the expected type is "
      <> show expectedT
      <> "."
  show (UniverseMismatch i j) =
    "The universe " <> show i
      <> " should be strictly less than "
      <> show j
      <> "."
  show (CannotApply f x) =
    "Application (vapp) error. Cannot apply \n" <> show f <> "\n to \n" <> show x
  show (ShouldBeStar ty) =
    "* n is of type * but " <> show ty <> " is not *."
  show (ShouldBeFunctionType ty f) =
    show ty <> " is not a function type but should be - while checking " <> show f
  show (UnboundIndex n) =
    "unbound index " <> show n
  show (SigmaMustBeZero) =
    "Sigma has to be 0."
  show (UsageMustBeZero) =
    "Usage has to be 0."
  show (UsageNotCompatible expectedU gotU) =
    "The usage of "
      <> (show gotU)
      <> " is not compatible with "
      <> (show expectedU)
  show (UnboundBinder ii x) =
    "Cannot find the type of \n"
      <> show x
      <> "\n (binder number "
      <> show ii
      <> ") in the environment."
  show (MustBeFunction m ii n) =
    ( show m <> "\n (binder number " <> show ii
        <> ") is not a function type and thus \n"
        <> show n
        <> "\n cannot be applied to it."
    )
  show (BoundVariableCannotBeInferred) =
    "Bound variable cannot be inferred"

type HasThrowTC' ext primTy primVal m =
  HasThrow "typecheckError" (TypecheckError' ext primTy primVal) m

type HasThrowTC primTy primVal m =
  HasThrowTC' IR.NoExt primTy primVal m

throwTC ::
  HasThrowTC' ext primTy primVal m =>
  TypecheckError' ext primTy primVal ->
  m z
throwTC = throw @"typecheckError"

data T

IR.extendTerm "Term" [] [t|T|] $
  \primTy primVal ->
    let typed = Just [[t|Annotation $primTy $primVal|]]
     in IR.defaultExtTerm
          { IR.typeStar = typed,
            IR.typePrimTy = typed,
            IR.typePi = typed,
            IR.typeLam = typed,
            IR.typeElim = typed
          }

IR.extendElim "Elim" [] [t|T|] $
  \primTy primVal ->
    let typed = Just [[t|Annotation $primTy $primVal|]]
     in IR.defaultExtElim
          { IR.typeBound = typed,
            IR.typeFree = typed,
            IR.typePrim = typed,
            IR.typeApp = typed,
            IR.typeAnn = typed
          }

getTermAnn :: Term primTy primVal -> Annotation primTy primVal
getTermAnn (Star _ ann) = ann
getTermAnn (PrimTy _ ann) = ann
getTermAnn (Pi _ _ _ ann) = ann
getTermAnn (Lam _ ann) = ann
getTermAnn (Elim _ ann) = ann

getElimAnn :: Elim primTy primVal -> Annotation primTy primVal
getElimAnn (Bound _ ann) = ann
getElimAnn (Free _ ann) = ann
getElimAnn (Prim _ ann) = ann
getElimAnn (App _ _ ann) = ann
getElimAnn (Ann _ _ _ _ ann) = ann
