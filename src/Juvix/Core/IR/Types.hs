-- | Quantitative type implementation inspired by
--   Atkey 2018 and McBride 2016.
module Juvix.Core.IR.Types
  ( module Juvix.Core.IR.Types,
    Name (..),
    Term' (..),
    Elim' (..),
    TermAll,
    ElimAll,
  )
where

import Juvix.Core.IR.Types.Base
import qualified Juvix.Core.Usage as Usage
-- import Juvix.Core.IR.Extension
import Juvix.Library hiding (show)
import Prelude (Show (..), String)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data NoExt

extendTerm "Term" [t|NoExt|] defaultExtTerm

extendElim "Elim" [t|NoExt|] defaultExtElim

-- | Values/types
data Value primTy primVal m
  = VStar Natural
  | VPrimTy primTy
  | VPi
      Usage.T
      (Value primTy primVal m)
      (Value primTy primVal m -> m (Value primTy primVal m))
  | VLam (Value primTy primVal m -> m (Value primTy primVal m))
  | VNeutral (Neutral primTy primVal m)
  | VPrim primVal

-- | A neutral term is either a variable or an application of a neutral term to a value
data Neutral primTy primVal m
  = NFree Name
  | NApp (Neutral primTy primVal m) (Value primTy primVal m)

-- | 'Annotations' include usage and type.
data Annotation primTy primVal m
  = Annotated
      { usage :: Usage.T,
        type' :: Value primTy primVal m
      }

data Context primTy primVal m
  = Context
      { ann :: Annotation primTy primVal m,
        name :: Name
      }

-- | 'Context's map variables to their types.
type Contexts primTy primVal m = [Context primTy primVal m]

-- Evaluation
type Env primTy primVal m = [Value primTy primVal m]

--------------------------------------------------------------------------------
-- Deriving Instances
--------------------------------------------------------------------------------

deriving instance
  (Eq primTy, Eq primVal) =>
  Eq (Neutral primTy primVal (EnvTypecheck primTy primVal))

instance
  (Eq primTy, Eq primVal) =>
  Eq (Value primTy primVal (EnvTypecheck primTy primVal))
  where
  x == y = fst (exec (quote0 x)) == fst (exec (quote0 y))

deriving instance
  (Eq primTy, Eq primVal) =>
  Eq (Annotation primTy primVal (EnvTypecheck primTy primVal))

deriving instance
  (Show primTy, Show primVal) =>
  Show (Annotation primTy primVal (EnvTypecheck primTy primVal))

deriving instance
  (Eq primTy, Eq primVal) =>
  Eq (Context primTy primVal (EnvTypecheck primTy primVal))

deriving instance
  (Show primTy, Show primVal) =>
  Show (Context primTy primVal (EnvTypecheck primTy primVal))

instance
  (Show primTy, Show primVal) =>
  Show (Value primTy primVal (EnvTypecheck primTy primVal))
  where
  show x = show (fst (exec (quote0 x)))

deriving instance
  (Show primTy, Show primVal) =>
  Show (Neutral primTy primVal (EnvTypecheck primTy primVal))

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

deriving instance
  (Eq primTy, Eq primVal) =>
  Eq (TypecheckError primTy primVal (EnvTypecheck primTy primVal))

instance
  (Show primTy, Show primVal) =>
  Show (TypecheckError primTy primVal (EnvTypecheck primTy primVal))
  where
  show (TypeMismatch binder term expectedT gotT) =
    "Type mismatched. \n" <> show term <> " \n (binder number " <> show binder
      <> ") is of type \n"
      <> show (type' gotT)
      <> " , with "
      <> show (usage gotT)
      <> " usage.\n But the expected type is "
      <> show (type' expectedT)
      <> " , with "
      <> show (usage expectedT)
      <> " usage."
  show (UniverseMismatch t ty) =
    show t
      <> " is of type * of a higher universe. But the expected type "
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
    "The usage of "
      <> (show (usage gotU))
      <> " is not compatible with "
      <> (show (usage expectedU))
  show (UnboundBinder ii x) =
    "Cannot find the type of \n"
      <> show x
      <> "\n (binder number "
      <> show ii
      <> ") in the environment."
  show (MustBeFunction m ii n) =
    ( show m <> "\n (binder number " <> show ii
        <> ") is not a function type and thus \n"
        <> show n
        <> "\n cannot be applied to it."
    )
  show (BoundVariableCannotBeInferred) =
    "Bound variable cannot be inferred"

newtype TypecheckerLog = TypecheckerLog {msg :: String}
  deriving (Show, Eq, Generic)

newtype EnvCtx primTy primVal
  = EnvCtx
      { typecheckerLog :: [TypecheckerLog]
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
    ( HasSink "typecheckerLog" [TypecheckerLog],
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

exec ::
  EnvTypecheck primTy primVal a ->
  ( Either (TypecheckError primTy primVal (EnvTypecheck primTy primVal)) a,
    EnvCtx primTy primVal
  )
exec (EnvTyp env) = runState (runExceptT env) (EnvCtx [])

-- Quotation: takes a value back to a term
quote0 ::
  forall primTy primVal m.
  (Monad m) =>
  Value primTy primVal m ->
  m (Term primTy primVal)
quote0 = quote 0

quote ::
  forall primTy primVal m.
  (Monad m) =>
  Natural ->
  Value primTy primVal m ->
  m (Term primTy primVal)
quote ii p =
  case p of
    VStar nat -> pure (Star nat)
    VPrimTy p -> pure (PrimTy p)
    VPi pi v f -> Pi pi <$> quote ii v <*> (quote (succ ii) =<< f (vfree (Quote ii)))
    VLam func -> Lam <$> (quote (succ ii) =<< func (vfree (Quote ii)))
    VPrim pri -> pure (Elim (Prim pri))
    VNeutral n -> Elim <$> neutralQuote ii n

neutralQuote ::
  forall primTy primVal m.
  (Monad m) =>
  Natural ->
  Neutral primTy primVal m ->
  m (Elim primTy primVal)
neutralQuote ii (NFree x) = pure (boundfree ii x)
neutralQuote ii (NApp n v) = App <$> neutralQuote ii n <*> quote ii v

-- | 'vfree' creates the value corresponding to a free variable
vfree :: Name -> Value primTy primVal m
vfree n = VNeutral (NFree n)

-- checks if the variable occurring at the head of
-- the application is a bound variable or a free name
boundfree :: Natural -> Name -> Elim primTy primVal
boundfree ii (Quote k) = Bound (ii - k - 1)
boundfree _ii x = Free x

-- initial environment
initEnv :: Env primTy primVal m
initEnv = []
