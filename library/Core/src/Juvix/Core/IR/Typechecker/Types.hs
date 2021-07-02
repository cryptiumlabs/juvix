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
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library
import qualified Juvix.Library.Usage as Usage

data Annotation' ext primTy primVal = Annotation
  { annUsage :: Usage.T,
    annType :: Core.Value' ext primTy primVal
  }
  deriving (Generic)

type Annotation = Annotation' IR.T

deriving instance
  (Eq (Core.Value' ext primTy primVal)) =>
  Eq (Annotation' ext primTy primVal)

deriving instance
  (Show (Core.Value' ext primTy primVal)) =>
  Show (Annotation' ext primTy primVal)

data T

data BindAnnotation' ext primTy primVal = BindAnnotation
  { baBindAnn, baResAnn :: {-# UNPACK #-} !(Annotation' ext primTy primVal)
  }
  deriving (Generic)

deriving instance
  (Eq (Core.Value' ext primTy primVal)) =>
  Eq (BindAnnotation' ext primTy primVal)

deriving instance
  (Show (Core.Value' ext primTy primVal)) =>
  Show (BindAnnotation' ext primTy primVal)

type BindAnnotation = BindAnnotation' IR.T

Core.extendTerm "Term'" [] [t|T|] $
  \primTy primVal ->
    let typed = Just [[t|Annotation $primTy $primVal|]]
        bindTyped = Just [[t|BindAnnotation $primTy $primVal|]]
     in Core.defaultExtTerm
          { Core.typeStar = typed,
            Core.typePrimTy = typed,
            Core.typePrim = typed,
            Core.typePi = typed,
            Core.typeSig = typed,
            Core.typePair = typed,
            Core.typeUnitTy = typed,
            Core.typeUnit = typed,
            Core.typeLam = bindTyped,
            Core.typeLet = bindTyped,
            Core.typeElim = typed
          }

Core.extendElim "Elim'" [] [t|T|] $
  \primTy primVal ->
    let typed = Just [[t|Annotation $primTy $primVal|]]
     in Core.defaultExtElim
          { Core.typeBound = typed,
            Core.typeFree = typed,
            Core.typeApp = typed,
            Core.typeAnn = typed
          }

type Term primTy primVal = Term' primTy (P.TypedPrim primTy primVal)

type Elim primTy primVal = Elim' primTy (P.TypedPrim primTy primVal)

type GlobalT' extV extT primTy primVal =
  Core.Global' extV extT primTy (P.TypedPrim primTy primVal)

type GlobalT primTy primVal = GlobalT' IR.T IR.T primTy primVal

type DatatypeT' ext primTy primVal =
  Core.Datatype' ext primTy (P.TypedPrim primTy primVal)

type DatatypeT primTy primVal = DatatypeT' IR.T primTy primVal

type DataArgT' ext primTy primVal =
  Core.DataArg' ext primTy (P.TypedPrim primTy primVal)

type DataArgT primTy primVal = DataArgT' IR.T primTy primVal

type DataConT' ext primTy primVal =
  Core.DataCon' ext primTy (P.TypedPrim primTy primVal)

type DataConT primTy primVal = DataConT' IR.T primTy primVal

type FunctionT' extV extT primTy primVal =
  Core.Function' extV extT primTy (P.TypedPrim primTy primVal)

type FunctionT primTy primVal = FunctionT' IR.T IR.T primTy primVal

type FunClauseT' extT primTy primVal =
  Core.FunClause' extT primTy (P.TypedPrim primTy primVal)

type FunClauseT primTy primVal = FunClauseT' IR.T primTy primVal

type PatternT' ext primTy primVal =
  Core.Pattern' ext primTy (P.TypedPrim primTy primVal)

type PatternT primTy primVal = PatternT' IR.T primTy primVal

type AbstractT' extV primTy primVal =
  Core.Abstract' extV primTy (P.TypedPrim primTy primVal)

type AbstractT primTy primVal = AbstractT' IR.T primTy primVal

type GlobalsT' extV extT primTy primVal =
  Core.Globals' extV extT primTy (P.TypedPrim primTy primVal)

type GlobalsT primTy primVal = GlobalsT' IR.T IR.T primTy primVal

type ValueT' ext primTy primVal =
  Core.Value' ext primTy (P.TypedPrim primTy primVal)

type ValueT primTy primVal = ValueT' IR.T primTy primVal

type NeutralT' ext primTy primVal =
  Core.Neutral' ext primTy (P.TypedPrim primTy primVal)

type NeutralT primTy primVal = NeutralT' IR.T primTy primVal

type AnnotationT' ext primTy primVal =
  Annotation' ext primTy (P.TypedPrim primTy primVal)

type AnnotationT primTy primVal = AnnotationT' IR.T primTy primVal

type BindAnnotationT' ext primTy primVal =
  BindAnnotation' ext primTy (P.TypedPrim primTy primVal)

type BindAnnotationT primTy primVal = BindAnnotationT' IR.T primTy primVal

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
