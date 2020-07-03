module Juvix.Core.Erased.Types where

import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Type)
import qualified Juvix.Library.HashMap as Map
import Juvix.Core.Erased.Extend
import Juvix.Core.HRAnn.Types (Annotation (..), AppAnnotation (..), BindAnnotation (..))
import qualified Juvix.Core.IR.Types.Base as IR
import Juvix.Core.IR.Types.Base hiding
  ( extDataArg,
    extDataCon,
    extDatatype,
    extFunClause,
    extFunction,
    extPattern,
  )

data T

{-

IR.extendTerm "Term" [] [t|T|] extTerm

pattern Lam π x s t = Lam0 t (BindAnnotation x (Annotation π s))

pattern Pi π x s t = Pi0 π s t x

pattern Elim π s t = Elim0 s (Annotation π t)

{-# COMPLETE PrimTy, Pi, Lam, Elim #-}

IR.extendElim "Elim" [] [t|T|] extElim

pattern App π s ts ρ t tt =
  App0 s t (AppAnnotation (Annotation π ts) (Annotation ρ tt))

{-# COMPLETE Var, Prim, App, Ann #-}

-}

IR.extendValue "Value" [] [t|T|] extValue

IR.extendNeutral "Neutral" [] [t|T|] extNeutral

IR.extendDatatype "Datatype" [] [t|T|] extDatatype

IR.extendDataArg "DataArg" [] [t|T|] extDataArg

IR.extendDataCon "DataCon" [] [t|T|] extDataCon

IR.extendFunction "Function" [] [t|T|] extFunction

IR.extendFunClause "FunClause" [] [t|T|] extFunClause

IR.extendPattern "Pattern" [] [t|T|] extPattern

data Term primVal
  = Var Symbol
  | Prim primVal
  | -- TODO ∷ add proper lam with capture and arguments here!
    Lam Symbol (Term primVal)
  | Let Symbol (Term primVal) (Term primVal)
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
