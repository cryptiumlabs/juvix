module Juvix.Core.EAC.Types where

import qualified Data.Text as T
import Juvix.Core.Erased.Types
import Juvix.Library hiding (Type)
import Juvix.Utility

-- Restricted pseudoterm (inner).
data RPTI primVal
  = RVar Symbol
  | RPrim primVal
  | RLam Symbol (RPTO primVal)
  | RApp (RPTO primVal) (RPTO primVal)
  deriving (Show, Eq, Generic)

-- Restricted pseudoterm (outer).
data RPTO primVal
  = RBang Int (RPTI primVal)
  deriving (Show, Eq)

-- Restricted pseudoterm (alias).
type RPT primVal = RPTO primVal

-- Parameterized restricted pseudoterm (alias).
type PRPT primVal = RPT primVal

-- Parameter (alias).
type Param = Int

-- Parameterized type.
data PType primTy
  = PSymT Param Symbol
  | PPrimT primTy
  | PArrT Param (PType primTy) (PType primTy)
  deriving (Show, Eq)

-- Parameterized type assignment (alias).
type ParamTypeAssignment primTy = HashMap Symbol (PType primTy)

-- Linear (in)equality constraint on parameters.
data Constraint
  = Constraint
      { vars ∷ [ConstraintVar],
        op ∷ Op
      }
  deriving (Show, Eq)

-- Variable in constraint.
data ConstraintVar
  = ConstraintVar
      { coefficient ∷ Int,
        variable ∷ Param
      }
  deriving (Show, Eq)

-- Operation for constraint.
data Op
  = Gte Int
  | Eq Int
  deriving (Show, Eq)

-- Path of parameters to present subterm.
type Path = [Param]

-- Variable paths.
type VarPaths = HashMap Symbol Param

-- Occurrence map.
type OccurrenceMap = HashMap Symbol Int

-- | Bracket Error Types
data BracketErrors
  = TooManyOpen
  | TooManyOpenV
  | TooManyClosing
  | TooManyClosingV
  | InvalidAssignment
  deriving (Show)

-- | Runner Type for Bracket and TypeError
newtype EitherTyp b a
  = EitherBracket {runEither ∷ (Either b a)}
  deriving
    (Functor, Applicative, Monad)
    via Except b
  deriving
    (HasThrow "typ" b)
    via MonadError (Except b)

-- | Error type when running the type Chekcer
data TypeErrors primTy primVal
  = MisMatchArguments (PType primTy) (PType primTy) (RPTI primVal)
  | TypeIsNotFunction (PType primTy)
  | MissingOverUse
  | ExpectedFunction
  | TooManyHats
  deriving (Show)

-- | Total errors among Type and Bracket Errors
data Errors primTy primVal
  = Typ (TypeErrors primTy primVal)
  | Brack BracketErrors
  deriving (Show)

-- Environment for errors.
newtype EnvError primTy primVal a = EnvError (ExceptT (TypeErrors primTy primVal) (State (Info primTy)) a)
  deriving (Functor, Applicative, Monad)
  deriving
    (HasState "ctxt" (HashMap Symbol (Type primTy)))
    via Field "ctxt" () (MonadState (ExceptT (TypeErrors primTy primVal) (State (Info primTy))))
  deriving
    (HasThrow "typ" (TypeErrors primTy primVal))
    via MonadError (ExceptT (TypeErrors primTy primVal) (State (Info primTy)))

data Info primTy = I {ctxt ∷ HashMap Symbol (Type primTy)} deriving (Show, Generic)

-- Environment for inference.
data Env primTy
  = Env
      { path ∷ Path,
        varPaths ∷ VarPaths,
        typeAssignment ∷ TypeAssignment primTy,
        nextParam ∷ Param,
        constraints ∷ [Constraint],
        occurrenceMap ∷ OccurrenceMap
      }
  deriving (Show, Eq, Generic)

newtype EnvConstraint primTy a = EnvCon (State (Env primTy) a)
  deriving (Functor, Applicative, Monad)
  deriving
    (HasState "path" Path)
    via Field "path" () (MonadState (State (Env primTy)))
  deriving
    (HasState "varPaths" VarPaths)
    via Field "varPaths" () (MonadState (State (Env primTy)))
  deriving
    (HasState "typeAssignment" (TypeAssignment primTy))
    via Field "typeAssignment" () (MonadState (State (Env primTy)))
  deriving
    (HasState "nextParam" Param)
    via Field "nextParam" () (MonadState (State (Env primTy)))
  deriving
    (HasState "occurrenceMap" OccurrenceMap)
    via Field "occurrenceMap" () (MonadState (State (Env primTy)))
  deriving
    ( HasStream "constraints" [Constraint],
      HasWriter "constraints" [Constraint]
    )
    via WriterLog (Field "constraints" () (MonadState (State (Env primTy))))

instance PrettyPrint ConstraintVar where
  prettyPrintValue (ConstraintVar coeff var) =
    T.concat ["(", prettyPrintValue coeff, " * m_", prettyPrintValue var, ")"]

instance PrettyPrint Op where
  prettyPrintValue (Gte n) = T.concat [">= ", prettyPrintValue n]
  prettyPrintValue (Eq n) = T.concat ["= ", prettyPrintValue n]

instance PrettyPrint Constraint where
  prettyPrintValue (Constraint vars op) =
    T.concat [T.intercalate " + " (map prettyPrintValue vars), " ", prettyPrintValue op]
