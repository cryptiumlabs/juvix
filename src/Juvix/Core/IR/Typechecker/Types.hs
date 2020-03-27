{-# LANGUAGE UndecidableInstances #-}
module Juvix.Core.IR.Typechecker.Types where

import Juvix.Library hiding (show)
import qualified Extensible as Ext
import qualified Juvix.Core.Usage as Usage
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.IR.Types.Base
import Prelude (Show (..), String)


data T

data Annotation primTy primVal m =
  Annotation
    { annUsage :: Usage.T
    , annType  :: IR.Value primTy primVal m
    }

deriving instance (Eq (IR.Value primTy primVal m)) =>
  Eq (Annotation primTy primVal m)

deriving instance (Show (IR.Value primTy primVal m)) =>
  Show (Annotation primTy primVal m)

data ContextElement primTy primVal m =
  ContextElement
    { ctxName :: IR.Name
    , ctxAnn  :: {-# UNPACK #-} !(Annotation primTy primVal m)
    }

contextElement :: IR.Name -> Usage.T -> IR.Value primTy primVal m
               -> ContextElement primTy primVal m
contextElement n π t = ContextElement n (Annotation π t)

deriving instance (Eq (IR.Value primTy primVal m)) =>
  Eq (ContextElement primTy primVal m)

deriving instance (Show (IR.Value primTy primVal m)) =>
  Show (ContextElement primTy primVal m)

type Context primTy primVal m = [ContextElement primTy primVal m]

lookupCtx :: IR.Name -> Context primTy primVal m
          -> Maybe (Annotation primTy primVal m)
lookupCtx x = fmap ctxAnn . find (\e -> ctxName e == x)


data TypecheckError primTy primVal m
  = TypeMismatch
      Natural
      (IR.Term primTy primVal)
      (Annotation primTy primVal m)
      (Annotation primTy primVal m)
  | UniverseMismatch (IR.Term primTy primVal) (IR.Value primTy primVal m)
  | CannotApply (IR.Value primTy primVal m) (IR.Value primTy primVal m)
  | ShouldBeStar (IR.Value primTy primVal m)
  | ShouldBeFunctionType (IR.Value primTy primVal m) (IR.Term primTy primVal)
  | UnboundIndex Natural
  | SigmaMustBeZero
  | UsageMustBeZero
  | UsageNotCompatible (Annotation primTy primVal m) (Annotation primTy primVal m)
  | UnboundBinder Natural IR.Name
  | MustBeFunction (IR.Elim primTy primVal) Natural (IR.Term primTy primVal)
  | BoundVariableCannotBeInferred

deriving instance
  (Eq primTy, Eq primVal) ⇒
  Eq (TypecheckError primTy primVal (EnvTypecheck primTy primVal))

instance
  (Show primTy, Show primVal) ⇒
  Show (TypecheckError primTy primVal (EnvTypecheck primTy primVal))
  where
  show (TypeMismatch binder term expectedT gotT) =
    "Type mismatched. \n" <> show term <> " \n (binder number " <> show binder
      <> ") is of type \n"
      <> show (annType gotT)
      <> " , with "
      <> show (annUsage gotT)
      <> " usage.\n But the expected type is "
      <> show (annType expectedT)
      <> " , with "
      <> show (annUsage expectedT)
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
      <> (show (annUsage gotU))
      <> " is not compatible with "
      <> (show (annUsage expectedU))
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

newtype TypecheckerLog = TypecheckerLog {msg ∷ String}
  deriving (Show, Eq, Generic)

data EnvCtx primTy primVal
  = EnvCtx
      { typecheckerLog ∷ [TypecheckerLog]
      }
  deriving (Show, Eq, Generic)

type EnvAlias primTy primVal =
  ExceptT (TypecheckError primTy primVal (EnvTypecheck primTy primVal))
    (State (EnvCtx primTy primVal))

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

instance
  (Eq primTy, Eq primVal) ⇒
  Eq (IR.Value primTy primVal (EnvTypecheck primTy primVal))
  where
  x == y = fst (exec (IR.quote0 x)) == fst (exec (IR.quote0 y))

deriving instance
  (Eq primTy, Eq primVal) ⇒
  Eq (IR.Neutral primTy primVal (EnvTypecheck primTy primVal))

instance
  (Show primTy, Show primVal) ⇒
  Show (IR.Value primTy primVal (EnvTypecheck primTy primVal))
  where
  show x = show (fst (exec (IR.quote0 x)))

deriving instance
  (Show primTy, Show primVal) ⇒
  Show (IR.Neutral primTy primVal (EnvTypecheck primTy primVal))


exec ∷
  EnvTypecheck primTy primVal a →
  ( Either (TypecheckError primTy primVal (EnvTypecheck primTy primVal)) a,
    EnvCtx primTy primVal
  )
exec (EnvTyp env) = runState (runExceptT env) (EnvCtx [])



do
  let typed = Ext.Ann $ \primTy primVal -> [t|IR.Term $primTy $primVal|]
  extT <- IR.extendTerm "Term" [t|T|] $
    IR.defaultExtTerm
      { IR.typeStar   = typed
      , IR.typePrimTy = typed
      , IR.typePi     = typed
      , IR.typeLam    = typed
      , IR.typeElim   = typed
      }
  extE <- IR.extendElim "Elim" [t|T|] $
    IR.defaultExtElim
      { IR.typeBound = typed
      , IR.typeFree  = typed
      , IR.typePrim  = typed
      , IR.typeApp   = typed
      , IR.typeAnn   = typed
      }
  pure $ extT ++ extE

