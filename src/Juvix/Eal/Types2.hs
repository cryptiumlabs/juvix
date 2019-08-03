module Juvix.Eal.Types2 where

import qualified Data.Map.Strict as Map
import           Juvix.Library   hiding (Type)

-- Untyped term.
data Term
  = Var SomeSymbol
  | App Term Term
  | Lam SomeSymbol Term

  deriving (Show, Eq)

-- Simple type.
data Type
  = SymT SomeSymbol
  | ArrT Type Type

  deriving (Show, Eq)

-- Restricted pseudoterm (inner).
data RPTI
  = RVar SomeSymbol
  | RLam SomeSymbol RPTO
  | RApp RPTO RPTO

  deriving (Show, Eq)

-- Restricted pseudoterm (outer).
data RPTO
  = RBang Int RPTI

  deriving (Show, Eq)

-- Restricted pseudoterm (alias).
type RPT = RPTO

-- Simple type assignment (alias).
type TypeAssignment = Map.Map SomeSymbol Type

-- Parameterized restricted pseudoterm (alias).
type PRPT = RPT

-- Parameter (alias).
type Param = Int

-- Parameterized type.
data PType
  = PSymT Param SomeSymbol
  | PArrT Param PType PType

  deriving (Show, Eq)

-- Parameterized type assignment (alias).
type ParamTypeAssignment = Map.Map SomeSymbol PType

-- Linear (in)equality constraint on parameters.
data Constraint = Constraint {
  vars :: [ConstraintVar],
  op   :: Op
} deriving (Show, Eq)

-- Variable in constraint.
data ConstraintVar = ConstraintVar {
  coefficient :: Int,
  variable    :: Param
} deriving (Show, Eq)

-- Operation for constraint.
data Op
  = Gte Int
  | Eq  Int
  deriving (Show, Eq)

-- Path of parameters to present subterm.
type Path = [Param]

-- Variable paths.
type VarPaths = Map SomeSymbol Param

-- Environment for inference.
data Env = Env {
  path           :: Path,
  varPaths :: VarPaths,
  typeAssignment :: TypeAssignment,
  nextParam     :: Param,
  constraints    :: [Constraint]
} deriving (Show, Eq, Generic)

newtype EnvConstraint a = EnvCon (State Env a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "path" Path) via
    Field "path" () (MonadState (State Env))
  deriving (HasState "varPaths" VarPaths) via
    Field "varPaths" () (MonadState (State Env))
  deriving (HasState "typeAssignment" TypeAssignment) via
    Field "typeAssignment" () (MonadState (State Env))
  deriving (HasState "nextParam" Param) via
    Field "nextParam" () (MonadState (State Env))
  deriving (HasState "constraints" [Constraint]) via
    Field "constraints" () (MonadState (State Env))
