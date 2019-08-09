module Juvix.Eal.Types2 where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import           Juvix.Library   hiding (Type)
import           Juvix.Utility

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

-- Occurrence map.
type OccurrenceMap = Map SomeSymbol Int

-- | Bracket Error Types
data BracketErrors = TooManyOpen
                   | TooManyOpenV
                   | TooManyClosing
                   | TooManyClosingV
                   | InvalidAssignment
                   deriving Show

-- | Runner Type for Bracket and TypeError
newtype EitherTyp b a =
  EitherBracket { runEither :: (Either b a) }
  deriving (Functor, Applicative, Monad) via
    Except b
  deriving (HasThrow "typ" b) via
    MonadError (Except b)


-- | Error type when running the type Chekcer
data TypeErrors = MisMatchArguments PType PType RPTI
                | TypeIsNotFunction PType
                | MissingOverUse
                | ExpectedFunction
                deriving Show

-- | Total errors among Type and Bracket Errors
data Errors = Typ TypeErrors
           | Brack BracketErrors
           deriving Show


-- Environment for inference.
data Env = Env {
  path           :: Path,
  varPaths       :: VarPaths,
  typeAssignment :: TypeAssignment,
  nextParam      :: Param,
  constraints    :: [Constraint],
  occurrenceMap  :: OccurrenceMap
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
  deriving (HasState "occurrenceMap" OccurrenceMap) via
    Field "occurrenceMap" () (MonadState (State Env))
  deriving ( HasStream "constraints" [Constraint]
           , HasWriter "constraints" [Constraint]) via
    WriterLog (Field "constraints" () (MonadState (State Env)))

instance PrettyPrint ConstraintVar where
  prettyPrintValue (ConstraintVar coeff var) =
    T.concat ["(", prettyPrintValue coeff, " * m_", prettyPrintValue var, ")"]

instance PrettyPrint Op where
  prettyPrintValue (Gte n) = T.concat [">= ", prettyPrintValue n]
  prettyPrintValue (Eq n)  = T.concat ["= " , prettyPrintValue n]

instance PrettyPrint Constraint where
  prettyPrintValue (Constraint vars op) =
    T.concat [T.intercalate " + " (map prettyPrintValue vars), " ", prettyPrintValue op]
