{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Typechecker.Types where

import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding (show)
import qualified Juvix.Library.Usage as Usage
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

data TypecheckError' extV extT primTy primVal
  = TypeMismatch
      { typeSubject :: IR.Elim' extT primTy primVal,
        typeExpected, typeGot :: IR.Value' extV primTy primVal
      }
  | UniverseMismatch
      { universeLower, universeHigher :: IR.Universe
      }
  | CannotApply
      { applyFun, applyArg :: IR.Value' extV primTy primVal
      }
  | ShouldBeStar
      { typeActual :: IR.Value' extV primTy primVal
      }
  | ShouldBeFunctionType
      { typeActual :: IR.Value' extV primTy primVal
      }
  | ShouldBePairType
      { typeActual :: IR.Value' extV primTy primVal
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
      { typeActual :: IR.Value' extV primTy primVal
      }
  | WrongPrimTy
      { primVal :: primVal,
        primTy :: P.PrimType primTy
      }
  | UnsupportedTermExt
      { termExt :: IR.TermX extT primTy primVal
      }
  | UnsupportedElimExt
      { elimExt :: IR.ElimX extT primTy primVal
      }
  | PartiallyAppliedConstructor
      { pattern_ :: IR.Pattern' extT primTy primVal
      }

type TypecheckError = TypecheckError' IR.NoExt IR.NoExt

deriving instance
  ( Eq primTy,
    Eq primVal,
    IR.TermAll Eq extV primTy primVal,
    IR.ElimAll Eq extV primTy primVal,
    IR.ValueAll Eq extV primTy primVal,
    IR.NeutralAll Eq extV primTy primVal,
    IR.TermAll Eq extT primTy primVal,
    IR.ElimAll Eq extT primTy primVal,
    IR.ValueAll Eq extT primTy primVal,
    IR.NeutralAll Eq extT primTy primVal,
    IR.PatternAll Eq extT primTy primVal
  ) =>
  Eq (TypecheckError' extV extT primTy primVal)

instance
  ( Show primTy,
    Show primVal,
    IR.TermAll Show extV primTy primVal,
    IR.ElimAll Show extV primTy primVal,
    IR.ValueAll Show extV primTy primVal,
    IR.NeutralAll Show extV primTy primVal,
    IR.TermAll Show extT primTy primVal,
    IR.ElimAll Show extT primTy primVal,
    IR.ValueAll Show extT primTy primVal,
    IR.NeutralAll Show extT primTy primVal,
    IR.PatternAll Show extT primTy primVal
  ) =>
  Show (TypecheckError' extV extT primTy primVal)
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
  show (ShouldBePairType ty) =
    show ty <> " is not a pair type but should be"
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
  show (UnsupportedTermExt x) =
    "Unsupported syntax form " <> show x
  show (UnsupportedElimExt x) =
    "Unsupported syntax form " <> show x
  show (PartiallyAppliedConstructor pat) =
    "Partially-applied constructor in pattern " <> show pat

type HasThrowTC' extV extT primTy primVal m =
  HasThrow "typecheckError" (TypecheckError' extV extT primTy primVal) m

type HasThrowTC primTy primVal m =
  HasThrowTC' IR.NoExt IR.NoExt primTy primVal m

throwTC ::
  HasThrowTC' extV extT primTy primVal m =>
  TypecheckError' extV extT primTy primVal ->
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
            IR.typeSig = typed,
            IR.typePair = typed,
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
getTermAnn (Sig _ _ _ ann) = ann
getTermAnn (Pair _ _ ann) = ann
getTermAnn (Lam _ anns) = baResAnn anns
getTermAnn (Let _ _ _ anns) = baResAnn anns
getTermAnn (Elim _ ann) = ann

getElimAnn :: Elim primTy primVal -> Annotation primTy primVal
getElimAnn (Bound _ ann) = ann
getElimAnn (Free _ ann) = ann
getElimAnn (App _ _ ann) = ann
getElimAnn (Ann _ _ _ _ ann) = ann
