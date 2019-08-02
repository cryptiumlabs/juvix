module Juvix.Eal.Types where

import           Juvix.Library hiding (link, reduce)
import qualified Juvix.Bohm.Type as BT
import qualified Data.Map.Strict as Map

data Eal = Term SomeSymbol
         | Lambda SomeSymbol Types Term
         | App Term Term
         deriving Show

data Term = Bang Integer Eal
          deriving Show

data NumberedEal
  = NTerm Int SomeSymbol
  | NLambda Int SomeSymbol Types NumberedEal
  | NApp Int NumberedEal NumberedEal
  | NBang Int NumberedEal
  deriving (Show, Eq)

data ParamSimpleType
  = PArrowT ParamSimpleType ParamSimpleType
  | PSpecificT SomeSymbol
  | PBangT Integer ParamSimpleType
  deriving (Show, Eq)

data Types = Lolly Types Types
           | BangT Integer Types
           | Forall
           | Specific SomeSymbol
           | UBang Integer Types
           deriving (Show, Eq)

data TypeErrors = MisMatchArguments
                | MissingOverUse
                | ExpectedFunction
                deriving Show

data BracketErrors = TooManyOpen
                   | TooManyClosing
                   deriving Show

data Info = I {ctxt :: Map SomeSymbol Types} deriving (Show, Generic)

newtype EnvError a = EnvError (ExceptT TypeErrors (State Info) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "ctxt" (Map SomeSymbol Types)) via
    Field "ctxt" () (MonadState (ExceptT TypeErrors (State Info)))
  deriving (HasThrow "typ" TypeErrors) via
    MonadError (ExceptT TypeErrors (State Info))

newtype EitherBracket a =
  EitherBracket { runEither :: (Either BracketErrors a) }
  deriving (Functor, Applicative, Monad) via
    Except BracketErrors
  deriving (HasThrow "typ" BracketErrors) via
    MonadError (Except BracketErrors)

data Constraint = Constraint {
  spots :: Path,
  op    :: Op
} deriving Show

type Spot = Int

data Op = Gte Int
        | Eq  Int
        deriving Show

type Path = [Spot]

-- we use the start at location to limit the list where we start at for the expression
type PathTerm = Map SomeSymbol Spot

type ParamTypeTerm = Map SomeSymbol (Spot, ParamSimpleType)

data ConstraintTermEnv = Con {
  path        :: Path,
  termsPath   :: PathTerm,
  paramTypeTerm :: ParamTypeTerm,
  count       :: Spot,
  constraints :: [Constraint]
} deriving (Show, Generic)

newtype EnvConstraint a = EnvCon (State ConstraintTermEnv a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "path" Path) via
     Field "path" () (MonadState (State ConstraintTermEnv))
  deriving (HasState "termsPath" PathTerm) via
     Field "termsPath" () (MonadState (State ConstraintTermEnv))
  deriving (HasState "count" Spot) via
     Field "count" () (MonadState (State ConstraintTermEnv))
  deriving (HasState "constraints" [Constraint]) via
     Field "constraints" () (MonadState (State ConstraintTermEnv))
  deriving (HasState "paramTypeTerm" ParamTypeTerm) via
     Field "paramTypeTerm" () (MonadState (State ConstraintTermEnv))
