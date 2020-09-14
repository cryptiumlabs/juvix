{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Typechecker.Types where

import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (show)
import Prelude (Show (..))

data Annotation' ext primTy primVal
  = Annotation
      { annUsage :: Usage.T,
        annType :: IR.Value' ext primTy primVal
      }
  deriving (Generic)

type Annotation = Annotation' IR.NoExt

deriving instance
  (Eq (IR.Value' ext primTy primVal)) =>
  Eq (Annotation' ext primTy primVal)

deriving instance
  (Show (IR.Value' ext primTy primVal)) =>
  Show (Annotation' ext primTy primVal)

data TypecheckError' ext primTy primVal
  = TypeMismatch
      { typeSubject :: IR.Elim' ext primTy primVal,
        typeExpected, typeGot :: IR.Value' ext primTy primVal
      }
  | UniverseMismatch
      { universeLower, universeHigher :: IR.Universe
      }
  | CannotApply
      { applyFun, applyArg :: IR.Value' ext primTy primVal
      }
  | ShouldBeStar
      { typeActual :: IR.Value' ext primTy primVal
      }
  | ShouldBeFunctionType
      { typeActual :: IR.Value' ext primTy primVal
      }
  | UnboundIndex
      { unboundIndex :: IR.BoundVar
      }
  | UsageMustBeZero
      { usageActual :: Usage.T
      }
  | LeftoverUsage
      { usageLeftover :: Usage.T
      }
  | InsufficientUsage
      { usageNeeded, usageActual :: Usage.T
      }
  | UnboundLocal
      { unboundIndex :: IR.BoundVar
      }
  | UnboundGlobal
      { unboundGlobal :: IR.GlobalName
      }
  | UnboundPatVar
      { unboundPatVar :: IR.PatternVar
      }
  | NotPrimTy
      { typeActual :: IR.Value' ext primTy primVal
      }
  | WrongPrimTy
      { primVal :: primVal,
        primTy :: P.PrimType primTy
      }

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
  show (TypeMismatch term expectedT gotT) =
    "Type mismatched.\n" <> show term <> "\n"
      <> "is of type\n"
      <> show gotT
      <> ".\nBut the expected type is\n"
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
  show (ShouldBeFunctionType ty) =
    show ty <> " is not a function type but should be"
  show (UnboundIndex n) =
    "unbound index " <> show n
  show (UsageMustBeZero π) =
    "The usage " <> show π <> " has to be 0."
  show (LeftoverUsage π) =
    "Leftover usage of " <> show π
  show (InsufficientUsage needed left) =
    "Needed " <> show needed <> " usages but only " <> show left <> " remain"
  show (UnboundLocal ii) =
    "Cannot find the type of binder number "
      <> show ii
      <> " in the context."
  show (UnboundGlobal x) =
    "Global name " <> show x <> " not in scope"
  show (UnboundPatVar x) =
    "Pattern variable " <> show x <> " not in scope"
  show (NotPrimTy x) =
    "Not a valid primitive type: " <> show x
  show (WrongPrimTy x ty) =
    "Primitive value " <> show x <> " cannot have type " <> show ty

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

data BindAnnotation' ext primTy primVal
  = BindAnnotation
      { baBindAnn, baResAnn :: {-# UNPACK #-} !(Annotation' ext primTy primVal)
      }
  deriving (Generic)

deriving instance
  (Eq (IR.Value' ext primTy primVal)) =>
  Eq (BindAnnotation' ext primTy primVal)

deriving instance
  (Show (IR.Value' ext primTy primVal)) =>
  Show (BindAnnotation' ext primTy primVal)

type BindAnnotation = BindAnnotation' IR.NoExt

IR.extendTerm "Term" [] [t|T|] $
  \primTy primVal ->
    let typed = Just [[t|Annotation $primTy $primVal|]]
        bindTyped = Just [[t|BindAnnotation $primTy $primVal|]]
     in IR.defaultExtTerm
          { IR.typeStar = typed,
            IR.typePrimTy = typed,
            IR.typePrim = typed,
            IR.typePi = typed,
            IR.typeLam = bindTyped,
            IR.typeLet = bindTyped,
            IR.typeElim = typed
          }

IR.extendElim "Elim" [] [t|T|] $
  \primTy primVal ->
    let typed = Just [[t|Annotation $primTy $primVal|]]
     in IR.defaultExtElim
          { IR.typeBound = typed,
            IR.typeFree = typed,
            IR.typeApp = typed,
            IR.typeAnn = typed
          }

getTermAnn :: Term primTy primVal -> Annotation primTy primVal
getTermAnn (Star _ ann) = ann
getTermAnn (PrimTy _ ann) = ann
getTermAnn (Prim _ ann) = ann
getTermAnn (Pi _ _ _ ann) = ann
getTermAnn (Lam _ anns) = baResAnn anns
getTermAnn (Let _ _ _ anns) = baResAnn anns
getTermAnn (Elim _ ann) = ann

getElimAnn :: Elim primTy primVal -> Annotation primTy primVal
getElimAnn (Bound _ ann) = ann
getElimAnn (Free _ ann) = ann
getElimAnn (App _ _ ann) = ann
getElimAnn (Ann _ _ _ _ ann) = ann
