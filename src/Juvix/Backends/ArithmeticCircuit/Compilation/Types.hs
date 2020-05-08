module Juvix.Backends.ArithmeticCircuit.Compilation.Types where

import qualified Juvix.Core.ErasedAnn as CoreErased
import Juvix.Library
import qualified Juvix.Backends.ArithmeticCircuit.Parameterisation as Parameterisation
import qualified Circuit.Expr as Expr
import qualified Circuit

data PrimVal f
  = Constant f
  | FEInteger f
  | BinOp BinOp (PrimVal f) (PrimVal f)
  | Op Op (PrimVal f)

data BinOp
  = Add
  | Mul
  | Sub
  | Exp
  | Eq

data Op = Neg

type Term = CoreErased.AnnTerm () (PrimVal Parameterisation.F)
type Type = CoreErased.Type () (PrimVal Parameterisation.F)

data CompilationError
  = NotYetImplemented
  | SomethingWentWrongSorry

data Memory x where
  Mem :: Parameterisation.FieldElement x => [x] -> Memory x

eval :: Mem x -> Term -> (Mem x, Expr.Expr Circuit.Wire Parameterisation.F a)
eval mem CoreErased.AnnTerm{ CoreErased.term = CoreErased.Term (CoreErased.Prim term) } = (mem, translateTerm term)
eval mem CoreErased.AnnTerm{ CoreErased.term = CoreErased.Term (CoreErased.Var var) } = (mem, Expr.EVar v)
eval (Mem ms) CoreErased.AnnTerm{ CoreErased.term = CoreErased.Term CoreErased.LamM{ CoreErased.arguments = arguments }}
  = (Mem (arguments ++ ms), undefined)

translateTerm Constant = Lang.c f
translateTerm BinOp Add prim prim' = Lang.add (translateTerm prim) (translateToArith prim')
translateTerm BinOp Mul prim prim' = Lang.mul (translateTerm prim) (translateToArith prim')
translateTerm BinOp Sub prim prim' = Lang.sub (translateTerm prim) (translateToArith prim')
-- implements exponentiation by hand
translateTerm BinOp Exp prim (FEInteger i)
    | i == 0 = translateTerm prim
    | otherwise = translateTerm (BinOp Mul prim (BinOp Exp prim (FEInteger (i - 1))))
translateTerm Binop Eq prim prim' = Lang.eq (translateTerm prim) (translateTerm prim')
translateTerm Op Neg prim = Lang.neg (translateTermh prim)

-- function-based operations
-- #TODO move to FieldElement instance
add prim prim' = CoreErased.Term (BinOp Add prim prim')
mul prim prim' = CoreErased.Term (BinOp Mul prim prim')
sub prim prim' = CoreErased.Term (BinOp Sub prim prim')
eq prim prim' = CoreErased.Term (Binop Eq prim prim')
neg prim = CoreErased.Term (Op Neg prim prim')
