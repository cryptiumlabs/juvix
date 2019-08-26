module Juvix.Encoding.Types where

import           Juvix.Library hiding (Product, Sum)

-- Adt Types -------------------------------------------------------------------

data Product = Product Product
             | Term
             | None
             deriving Show

data Sum = Branch SomeSymbol Product Sum
         | Single SomeSymbol Product
         deriving Show

data Name = Adt SomeSymbol Sum
          deriving Show

-- Object Syntax ---------------------------------------------------------------

-- Replace with bohm later!
data Lambda = Lambda SomeSymbol Lambda
            | Value SomeSymbol
            | Application Lambda Lambda
            deriving Show

-- In the real code bake this adt into Bohm or the Lambda itself!!!!

-- Case x of
-- /| Z   → body
-- /| S n → body
data Switch = Case Lambda [Case]

type Argument = SomeSymbol

type Body = Lambda

data Case = C SomeSymbol [Argument] Body

-- Environment type-------------------------------------------------------------
-- TODO :: Add smaller environments for testing
-- Doesn't matter much here, as all these values are pure
-- All this means is that we waste 1 allocation of a list and a map

-- One function does not require constructor, the other does not have
-- the missingCase writer

type Branches = [SomeSymbol]

data Bound = Bound { lam     :: Lambda
                   -- This is to remember what ADΤ we belong to
                   , adtName :: SomeSymbol
                   } deriving (Show, Generic)

data Env = Env { constructors :: Map SomeSymbol Bound
               -- | adtMap is a mapping between the adt name and the ordered cases thereof
               , adtMap       :: Map SomeSymbol Branches
               -- | missingCases represent the missing cases of a match
               , missingCases :: [SomeSymbol]
               } deriving (Show, Generic)

data Errors = AlreadyDefined
            | NotInAdt   SomeSymbol
            | NotInMatch SomeSymbol SomeSymbol
            | AdtNotDefined SomeSymbol
            | MatchWithoutCases
            | InvalidAdt
            deriving Show

newtype EnvS a = EnvS (StateT Env (Except Errors) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "constructors" (Map SomeSymbol Bound)) via
    Field "constructors" () (MonadState (StateT Env (Except Errors)))
  deriving (HasState "adtMap" (Map SomeSymbol Branches)) via
    Field "adtMap" () (MonadState (StateT Env (Except Errors)))
  deriving (HasThrow "err" Errors) via
    MonadError (StateT Env (Except Errors))
  deriving ( HasStream "missingCases" [SomeSymbol]
           , HasWriter "missingCases" [SomeSymbol] ) via
    WriterLog (Field "missingCases" () (MonadState (StateT Env (Except Errors))))

runEnvsS ∷ EnvS a → Either Errors (a, Env)
runEnvsS (EnvS a) = runExcept (runStateT a (Env mempty mempty mempty))
