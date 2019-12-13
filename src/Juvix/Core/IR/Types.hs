-- | Quantitative type implementation inspired by
--   Atkey 2018 and McBride 2016.
module Juvix.Core.IR.Types where

import Juvix.Core.Usage
import Juvix.Library hiding (show)
import Prelude (Show (..), String)

-- | checkable terms
data Term primTy primVal
  = -- | (sort i) i th ordering of (closed) universe.
    Star Natural
  | -- | 'PrimTy' primitive type
    PrimTy primTy
  | -- | formation rule of the dependent function type 'PI'.
    -- the Usage(π) tracks how many times x is used.
    Pi Usage (Term primTy primVal) (Term primTy primVal)
  | -- | 'LAM' Introduction rule of PI.
    -- The abstracted variable's usage is tracked with the Usage(π).
    Lam (Term primTy primVal)
  | -- | 'CONV' conversion rule. TODO make sure 0Γ ⊢ S≡T
    -- 'Elim' is the constructor that embeds Elim to Term
    Elim (Elim primTy primVal)
  deriving (Eq)

instance (Show primTy, Show primVal) ⇒ Show (Term primTy primVal) where
  show (Star n) = "* " <> show n
  show (PrimTy p) = show p
  show (Pi _usage varTy resultTy) =
    "[Π] " <> show varTy <> "-> " <> show resultTy
  show (Lam var) = "\\x. " <> show var
  -- Elim should be invisible to users.
  show (Elim term) = show term

-- | inferable terms
data Elim primTy primVal
  = -- | Bound variables, in de Bruijn indices
    Bound Natural
  | -- | Free variables of type name (see below)
    Free Name
  | -- | primitive constant
    Prim primVal
  | -- | elimination rule of PI (APP).
    App (Elim primTy primVal) (Term primTy primVal)
  | -- | Annotation with usage.
    Ann Usage (Term primTy primVal) (Term primTy primVal)
  deriving (Eq)

instance (Show primTy, Show primVal) ⇒ Show (Elim primVal primTy) where
  show (Bound i) = "Bound " <> show i -- to be improved
  show (Free name) = show name -- using derived show Name instance, to be improved
  show (Prim p) = show p
  show (App f x) = show f <> " " <> show x
  show (Ann pi theTerm theType) =
    show theTerm <> " : [" <> show pi <> "] " <> show theType

data Name
  = -- | Global variables are represented by name thus type string
    Global String
  | -- | to convert a bound variable into a free one
    Local Natural
  | Quote Natural
  deriving (Show, Eq)

-- | Values/types
data Value primTy primVal m
  = VStar Natural
  | VPrimTy primTy
  | VPi Usage (Value primTy primVal m) (Value primTy primVal m → m (Value primTy primVal m))
  | VLam (Value primTy primVal m → m (Value primTy primVal m))
  | VNeutral (Neutral primTy primVal m)
  | VPrim primVal

-- | A neutral term is either a variable or an application of a neutral term to a value
data Neutral primTy primVal m
  = NFree Name
  | NApp (Neutral primTy primVal m) (Value primTy primVal m)

-- | 'Annotations' include usage and type.
type Annotation primTy primVal m = (Usage, Value primTy primVal m)

-- Contexts map variables to their types.
type Context primTy primVal m = [(Name, Annotation primTy primVal m)]

-- Evaluation
type Env primTy primVal m = [Value primTy primVal m]

instance
  (Eq primTy, Eq primVal) ⇒
  Eq (Value primTy primVal (EnvTypecheck primTy primVal))
  where
  x == y = fst (exec (quote0 x)) == fst (exec (quote0 y))

instance
  (Eq primTy, Eq primVal) ⇒
  Eq (Neutral primTy primVal (EnvTypecheck primTy primVal))
  where
  NFree x == NFree y = x == y
  NApp a b == NApp c d = a == c && b == d
  _ == _ = False

instance (Show primTy, Show primVal) ⇒ Show (Value primTy primVal (EnvTypecheck primTy primVal)) where
  show x = show (fst (exec (quote0 x)))

instance
  (Show primTy, Show primVal) ⇒
  Show (Neutral primTy primVal (EnvTypecheck primTy primVal))
  where
  show (NFree n) = "NFree " <> show n
  show (NApp n v) = "NApp " <> show n <> " " <> show v

data TypecheckError primTy primVal m
  = TypeMismatch
      Natural
      (Term primTy primVal)
      (Annotation primTy primVal m)
      (Annotation primTy primVal m)
  | UniverseMismatch (Term primTy primVal) (Value primTy primVal m)
  | CannotApply (Value primTy primVal m) (Value primTy primVal m)
  | ShouldBeStar (Value primTy primVal m)
  | ShouldBeFunctionType (Value primTy primVal m) (Term primTy primVal)
  | UnboundIndex Natural
  | SigmaMustBeZero
  | UsageMustBeZero
  | UsageNotCompatible (Annotation primTy primVal m) (Annotation primTy primVal m)
  | UnboundBinder Natural Name
  | MustBeFunction (Elim primTy primVal) Natural (Term primTy primVal)
  | BoundVariableCannotBeInferred

instance
  (Eq primTy, Eq primVal) ⇒
  Eq (TypecheckError primTy primVal (EnvTypecheck primTy primVal))
  where
  _ == _ = False -- TODO

instance
  (Show primTy, Show primVal) ⇒
  Show (TypecheckError primTy primVal (EnvTypecheck primTy primVal))
  where
  show (TypeMismatch binder term expectedT gotT) =
    "Type mismatched. \n" <> show term <> " \n (binder number " <> show binder
      <> ") is of type \n"
      <> show (snd gotT)
      <> " , with "
      <> show (fst gotT)
      <> " usage.\n But the expected type is "
      <> show (snd expectedT)
      <> " , with "
      <> show (fst expectedT)
      <> " usage."
  show (UniverseMismatch t ty) =
    show t <> " is of type * of a higher universe. But the expected type "
      <> show ty
      <> " is * of a equal or lower universe."
  show (CannotApply f x) =
    "Application (vapp) error. Cannot apply \n" <> show f <> "\n to \n" <> show x
  show (ShouldBeStar ty) =
    "* n is of type * but " <> show ty <> " is not *."
  show (ShouldBeFunctionType ty f) =
    show ty <> " is not a function type but should be - while checking " <> show f
  show (UnboundIndex n) =
    "unbound index " <> show n
  show (SigmaMustBeZero) =
    "Sigma has to be 0."
  show (UsageMustBeZero) =
    "Usage has to be 0."
  show (UsageNotCompatible expectedU gotU) =
    "The usage of " <> (show (fst gotU)) <> "is not compatible with " <> (show (fst expectedU))
  show (UnboundBinder ii x) =
    "Cannot find the type of \n" <> show x <> "\n (binder number " <> show ii <> ") in the environment."
  show (MustBeFunction m ii n) =
    ( show m <> "\n (binder number " <> show ii
        <> ") is not a function type and thus \n"
        <> show n
        <> "\n cannot be applied to it."
    )
  show (BoundVariableCannotBeInferred) =
    "Bound variable cannot be inferred"

newtype TypecheckerLog = TypecheckerLog {msg ∷ String}
  deriving (Show, Eq, Generic)

data EnvCtx primTy primVal
  = EnvCtx
      { typecheckerLog ∷ [TypecheckerLog]
      }
  deriving (Show, Eq, Generic)

type EnvAlias primTy primVal a =
  ( ExceptT (TypecheckError primTy primVal (EnvTypecheck primTy primVal))
      (State (EnvCtx primTy primVal))
      a
  )

newtype EnvTypecheck primTy primVal a = EnvTyp (EnvAlias primTy primVal a)
  deriving (Functor, Applicative, Monad)
  deriving
    (HasThrow "typecheckError" (TypecheckError primTy primVal (EnvTypecheck primTy primVal)))
    via MonadError
          ( ExceptT (TypecheckError primTy primVal (EnvTypecheck primTy primVal))
              (MonadState (State (EnvCtx primTy primVal)))
          )
  deriving
    ( HasStream "typecheckerLog" [TypecheckerLog],
      HasWriter "typecheckerLog" [TypecheckerLog]
    )
    via WriterLog
          ( Field "typecheckerLog" ()
              ( MonadState
                  ( ExceptT (TypecheckError primTy primVal (EnvTypecheck primTy primVal))
                      (State (EnvCtx primTy primVal))
                  )
              )
          )

exec ∷
  EnvTypecheck primTy primVal a →
  ( Either (TypecheckError primTy primVal (EnvTypecheck primTy primVal)) a,
    EnvCtx primTy primVal
  )
exec (EnvTyp env) = runState (runExceptT env) (EnvCtx [])

-- Quotation: takes a value back to a term
quote0 ∷
  ∀ primTy primVal m.
  (Monad m) ⇒
  Value primTy primVal m →
  m (Term primTy primVal)
quote0 = quote 0

quote ∷
  ∀ primTy primVal m.
  (Monad m) ⇒
  Natural →
  Value primTy primVal m →
  m (Term primTy primVal)
quote _ii (VStar n) = pure (Star n)
quote _ii (VPrimTy p) = pure (PrimTy p)
quote ii (VPi pi v f) =
  Pi pi <$> quote ii v <*> (quote (ii + 1) =<< f (vfree (Quote ii)))
quote ii (VLam f) = Lam <$> (quote (ii + 1) =<< f (vfree (Quote ii)))
quote ii (VNeutral n) = Elim <$> neutralQuote ii n
quote _ii (VPrim p) = pure (Elim (Prim p))

neutralQuote ∷
  ∀ primTy primVal m.
  (Monad m) ⇒
  Natural →
  Neutral primTy primVal m →
  m (Elim primTy primVal)
neutralQuote ii (NFree x) = pure (boundfree ii x)
neutralQuote ii (NApp n v) = App <$> neutralQuote ii n <*> quote ii v

-- | 'vfree' creates the value corresponding to a free variable
vfree ∷ Name → Value primTy primVal m
vfree n = VNeutral (NFree n)

-- checks if the variable occurring at the head of
-- the application is a bound variable or a free name
boundfree ∷ Natural → Name → Elim primTy primVal
boundfree ii (Quote k) = Bound (ii - k - 1)
boundfree _ii x = Free x

-- initial environment
initEnv ∷ Env primTy primVal m
initEnv = []
