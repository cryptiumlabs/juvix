module Juvix.Core.Erased.Types where

import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Type)
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Core.IR.Types.Base as IR
import Juvix.Core.IR.Types.Base hiding (extDatatype,
 extDataArg, extDataCon, extFunction, extFunClause, extPattern)
import Juvix.Core.Erased.Extend

data T

IR.extendTerm "Term" [] [t|T|] extTerm

pattern Lam x t = Lam0 t x

pattern Pi π x s t = Pi0 π s t x

{-# COMPLETE PrimTy, Pi, Lam, Elim #-}

IR.extendElim "Elim" [] [t|T|] extElim

IR.extendDatatype "Datatype" [] [t|T|] extDatatype

IR.extendDataArg "DataArg" [] [t|T|] extDataArg

IR.extendDataCon "DataCon" [] [t|T|] extDataCon

IR.extendFunction "Function" [] [t|T|] extFunction

IR.extendFunClause "FunClause" [] [t|T|] extFunClause

IR.extendPattern "Pattern" [] [t|T|] extPattern

{-
data Term primVal
  = Var Symbol
  | Prim primVal
  | -- TODO ∷ add proper lam with capture and arguments here!
    Lam Symbol (Term primVal)
  | App (Term primVal) (Term primVal)
  deriving (Show, Eq, Generic)

data Type primTy
  = SymT Symbol
  | Star Natural
  | PrimTy primTy
  | -- TODO: How to deal with dependency?
    Pi Usage.T (Type primTy) (Type primTy)
  deriving (Show, Eq, Generic)

type TypeAssignment primTy = Map.T Symbol (Type primTy)

data EvaluationError primVal
  = PrimitiveApplicationError primVal primVal
  deriving (Show, Eq, Generic)
-}
