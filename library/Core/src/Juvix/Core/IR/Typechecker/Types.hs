{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Typechecker.Types
  ( module Juvix.Core.IR.Typechecker.Types,
    P.TypedPrim,

    -- * Constructors & fields for 'Return'
    pattern App.Cont,
    App.fun,
    App.args,
    App.numLeft,
    pattern App.Return,
    App.retType,
    App.retTerm,

    -- * Constructors & fields for 'Take'
    pattern App.Take,
    App.usage,
    App.type',
    App.term,
  )
where

import qualified Juvix.Core.Application as App
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import Juvix.Core.Translate
import qualified Juvix.Core.HR.Pretty as HR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library
import qualified Juvix.Library.PrettyPrint as PP
import qualified Juvix.Library.Usage as Usage

data Annotation' ext primTy primVal = Annotation
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

data T

data BindAnnotation' ext primTy primVal = BindAnnotation
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

IR.extendTerm "Term'" [] [t|T|] $
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
            IR.typeUnitTy = typed,
            IR.typeUnit = typed,
            IR.typeLam = bindTyped,
            IR.typeLet = bindTyped,
            IR.typeElim = typed
          }

IR.extendElim "Elim'" [] [t|T|] $
  \primTy primVal ->
    let typed = Just [[t|Annotation $primTy $primVal|]]
     in IR.defaultExtElim
          { IR.typeBound = typed,
            IR.typeFree = typed,
            IR.typeApp = typed,
            IR.typeAnn = typed
          }

type Term primTy primVal = Term' primTy (P.TypedPrim primTy primVal)

type Elim primTy primVal = Elim' primTy (P.TypedPrim primTy primVal)

type GlobalT' extV extT primTy primVal =
  IR.Global' extV extT primTy (P.TypedPrim primTy primVal)

type GlobalT primTy primVal = GlobalT' IR.NoExt IR.NoExt primTy primVal

type DatatypeT' ext primTy primVal =
  IR.Datatype' ext primTy (P.TypedPrim primTy primVal)

type DatatypeT primTy primVal = DatatypeT' IR.NoExt primTy primVal

type DataArgT' ext primTy primVal =
  IR.DataArg' ext primTy (P.TypedPrim primTy primVal)

type DataArgT primTy primVal = DataArgT' IR.NoExt primTy primVal

type DataConT' ext primTy primVal =
  IR.DataCon' ext primTy (P.TypedPrim primTy primVal)

type DataConT primTy primVal = DataConT' IR.NoExt primTy primVal

type FunctionT' extV extT primTy primVal =
  IR.Function' extV extT primTy (P.TypedPrim primTy primVal)

type FunctionT primTy primVal = FunctionT' IR.NoExt IR.NoExt primTy primVal

type FunClauseT' extT primTy primVal =
  IR.FunClause' extT primTy (P.TypedPrim primTy primVal)

type FunClauseT primTy primVal = FunClauseT' IR.NoExt primTy primVal

type PatternT' ext primTy primVal =
  IR.Pattern' ext primTy (P.TypedPrim primTy primVal)

type PatternT primTy primVal = PatternT' IR.NoExt primTy primVal

type AbstractT' extV primTy primVal =
  IR.Abstract' extV primTy (P.TypedPrim primTy primVal)

type AbstractT primTy primVal = AbstractT' IR.NoExt primTy primVal

type GlobalsT' extV extT primTy primVal =
  IR.Globals' extV extT primTy (P.TypedPrim primTy primVal)

type GlobalsT primTy primVal = GlobalsT' IR.NoExt IR.NoExt primTy primVal

type ValueT' ext primTy primVal =
  IR.Value' ext primTy (P.TypedPrim primTy primVal)

type ValueT primTy primVal = ValueT' IR.NoExt primTy primVal

type NeutralT' ext primTy primVal =
  IR.Neutral' ext primTy (P.TypedPrim primTy primVal)

type NeutralT primTy primVal = NeutralT' IR.NoExt primTy primVal

type AnnotationT' ext primTy primVal =
  Annotation' ext primTy (P.TypedPrim primTy primVal)

type AnnotationT primTy primVal = AnnotationT' IR.NoExt primTy primVal

type BindAnnotationT' ext primTy primVal =
  BindAnnotation' ext primTy (P.TypedPrim primTy primVal)

type BindAnnotationT primTy primVal = BindAnnotationT' IR.NoExt primTy primVal

getTermAnn :: Term' primTy primVal -> Annotation primTy primVal
getTermAnn (Star _ ann) = ann
getTermAnn (PrimTy _ ann) = ann
getTermAnn (Prim _ ann) = ann
getTermAnn (Pi _ _ _ ann) = ann
getTermAnn (Sig _ _ _ ann) = ann
getTermAnn (Pair _ _ ann) = ann
getTermAnn (UnitTy ann) = ann
getTermAnn (Unit ann) = ann
getTermAnn (Lam _ anns) = baResAnn anns
getTermAnn (Let _ _ _ anns) = baResAnn anns
getTermAnn (Elim _ ann) = ann

getElimAnn :: Elim' primTy primVal -> Annotation primTy primVal
getElimAnn (Bound _ ann) = ann
getElimAnn (Free _ ann) = ann
getElimAnn (App _ _ ann) = ann
getElimAnn (Ann _ _ _ _ ann) = ann
