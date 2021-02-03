module Typechecker.Types where

import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Parameterisations.All as All
import qualified Juvix.Core.Parameterisations.Naturals as Nat
import qualified Juvix.Core.Parameterisations.Unit as Unit
import qualified Juvix.Library.Usage as Usage

type NatGlobal = IR.Global Nat.Ty Nat.Val

type NatFunction = IR.Function Nat.Ty Nat.Val

type NatTerm = IR.Term Nat.Ty Nat.Val

type NatElim = IR.Elim Nat.Ty Nat.Val

type NatValue = IR.Value Nat.Ty Nat.Val

type NatValueT = IR.ValueT Nat.Ty Nat.Val

type NatAnnotation = IR.AnnotationT Nat.Ty Nat.Val

type UnitTerm = IR.Term Unit.Ty Unit.Val

type UnitElim = IR.Elim Unit.Ty Unit.Val

type UnitValue = IR.Value Unit.Ty Unit.Val

type UnitValueT = IR.ValueT Unit.Ty Unit.Val

type UnitAnnotation = IR.AnnotationT Unit.Ty Unit.Val

type AllTerm = IR.Term All.Ty All.Val

type AllElim = IR.Elim All.Ty All.Val

type AllValue = IR.Value All.Ty All.Val

type AllValueT = IR.ValueT All.Ty All.Val

type AllAnnotation = IR.AnnotationT All.Ty All.Val

infix 1 `ann`

ann :: Usage.T -> IR.Value primTy primVal -> IR.Annotation primTy primVal
ann = IR.Annotation
