module Juvix.EAL.EAL where

import           Data.Map.Strict as Map
import qualified Juvix.Bohm.Type as BT
import           Juvix.Library   hiding (link, reduce)

data Eal = Term SomeSymbol
         | Lambda SomeSymbol Types Term
         | App Term Term
         deriving Show

data Term = Bang Integer Eal
          deriving Show

data SimpleType = ArrowT SimpleType SimpleType
                | SpecificT SomeSymbol
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

runEnvError ∷ EnvError a → (Either TypeErrors a, Info)
runEnvError (EnvError a) = runState (runExceptT a) (I Map.empty)

newtype EitherBracket a =
  EitherBracket { runEither :: (Either BracketErrors a) }
  deriving (Functor, Applicative, Monad) via
    Except BracketErrors
  deriving (HasThrow "typ" BracketErrors) via
    MonadError (Except BracketErrors)

runBracketChecker ∷ Eal → Either BracketErrors ()
runBracketChecker t = runEither (bracketChecker t 0)

runTypeOf ∷ Eal → (Either TypeErrors Types, Info)
runTypeOf e = (reverseBangs <$> t, state)
  where (t, state) = runEnvError (typeOf e)

typeOf ∷ (HasState "ctxt" (Map SomeSymbol Types) m, HasThrow "typ" TypeErrors m)
       ⇒ Eal → m Types
typeOf (Term s) = do
  ctxt ← get @"ctxt"
  case ctxt Map.!? s of
    Nothing           → throw @"typ" MissingOverUse
    Just x@(BangT {}) → pure x
    Just x            → put @"ctxt" (Map.delete s ctxt) >> pure x

typeOf (Lambda sym symType term) = do
  modify @"ctxt" (Map.insert sym symType)
  typeTerm ← typeOfTerm term
  pure (Lolly symType typeTerm)

typeOf (App t1 t2) = do
  typeT2 ← typeOfTerm t2
  typeT1 ← typeOfTerm t1
  case getTypeInsideBang typeT1 of
    Lolly Forall target                 → pure target
    Lolly arg target
      | arg == typeT2
      ∨ Forall == getTypeInsideBang arg → pure target
      | otherwise                       → throw @"typ" MisMatchArguments
    _                                   → throw @"typ" ExpectedFunction

typeOfTerm ∷ (HasState "ctxt" (Map SomeSymbol Types) f, HasThrow "typ" TypeErrors f)
           ⇒ Term → f Types
typeOfTerm (Bang n e)
  | n > 0 = BangT n <$> typeOf e
  | n < 0 = UBang n <$> typeOf e
  | otherwise = typeOf e

getTypeInsideBang ∷ Types → Types
getTypeInsideBang (BangT _ t) = getTypeInsideBang t
getTypeInsideBang (UBang _ t) = getTypeInsideBang t
getTypeInsideBang t           = t

reverseBangs ∷ Types → Types
reverseBangs t = recursive t identity
  where
    -- Note this doesn't flatten all consecutive bangs and ubangs
    recursive (BangT n (BangT m t)) cps = recursive (BangT (n + m) t) cps
    recursive (UBang n (UBang m t)) cps = recursive (UBang (n + m) t) cps
    recursive (BangT n (UBang m t)) cps = recursive t (UBang m . BangT n . cps)
    recursive (UBang n (BangT m t)) cps = recursive t (BangT m . UBang n . cps)
    recursive (BangT m t)           cps = recursive t (BangT m . cps)
    recursive (UBang m t)           cps = recursive t (UBang m . cps)
    recursive t                     cps = cps t
-- Start of correct bracketing formula -----------------------------------------

bracketChecker ∷ HasThrow "typ" BracketErrors f ⇒ Eal → Integer → f ()
bracketChecker (Term _)       0 = pure ()
bracketChecker (Term _)       _ = throw @"typ" TooManyOpen
bracketChecker (Lambda _ _ t) n = bracketCheckerTerm t n
bracketChecker (App t1 t2)    n = bracketCheckerTerm t2 n >> bracketCheckerTerm t1 n

bracketCheckerTerm ∷ HasThrow "typ" BracketErrors f ⇒ Term → Integer → f ()
bracketCheckerTerm (Bang changeBy eal) n
  | changeBy + n < 0 = throw @"typ" TooManyClosing
  | otherwise        = bracketChecker eal (n + changeBy)

-- Constraint for terms --------------------------------------------------------

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

data ConstraintTermEnv = Con {
  path        :: Path,
  termsPath   :: PathTerm,
  count       :: Spot,
  constraints :: [Constraint]
} deriving (Show, Generic)

addPath ∷ (HasState "count" Spot m, HasState "path" Path m) ⇒ m Spot
addPath = do
  i ← get @"count"
  modify' @"path" (<> [i])
  put     @"count" (succ i)
  pure i

addCon ∷ HasState "constraints" [Constraint] m ⇒ Constraint → m ()
addCon con = modify' @"constraints" (con :)

boxConstraint ∷ ( HasState "constraints" [Constraint]  m
                , HasState "count"       Spot          m
                , HasState "path"        Path          m
                , HasState "termsPath"   PathTerm      m )
              ⇒ Term → m Term
boxConstraint (Bang _ t) = do
  termPaths ← get @"termsPath"
  count     ← addPath
  path      ← get @"path"
  addCon Constraint {spots = path, op = Gte 0}
  case t of
    Term s → do
      case termPaths Map.!? s of
        Just spot → addCon Constraint {spots = dropWhile (< spot) path, op = Eq 0}
        Nothing   → addCon Constraint {spots = path, op = Eq 0}
      pure (Bang (toInteger count) (Term s))
    Lambda s typ body → do
      put @"termsPath" (Map.insert s (succ count) termPaths)
      b ← boxConstraint body
      pure (Bang (toInteger count) (Lambda s typ b))
    App t1 t2 → do
      l ← boxConstraint t1
      put @"path"      path
      put @"termsPath" termPaths
      r ← boxConstraint t2
      pure (Bang (toInteger count) (App l r))

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

execBracketState ∷ EnvConstraint a → (a, ConstraintTermEnv)
execBracketState (EnvCon e) = runState e (Con mempty mempty 1 mempty)

-- Convert to Bohm--------------------------------------------------------------

ealToBohm ∷ Term → BT.Bohm
ealToBohm (Bang _ (Term s))       = BT.Symbol' s
ealToBohm (Bang _ (Lambda s _ t)) = BT.Lambda s (ealToBohm t)
ealToBohm (Bang _ (App t1 t2))    = BT.Application (ealToBohm t1) (ealToBohm t2)
