{-# LANGUAGE UndecidableInstances #-}
module Juvix.Core.IR.Typechecker.Types where

import Juvix.Library hiding (show)
import qualified Extensible as Ext
import qualified Juvix.Core.Usage as Usage
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.IR.Types.Base
import Prelude (Show (..), String)


data Annotation' ext primTy primVal =
  Annotation
    { annUsage :: Usage.T
    , annType  :: IR.Value' ext primTy primVal
    }

type Annotation = Annotation' IR.NoExt

deriving instance (Eq (IR.Value' ext primTy primVal)) =>
  Eq (Annotation' ext primTy primVal)

deriving instance (Show (IR.Value' ext primTy primVal)) =>
  Show (Annotation' ext primTy primVal)

data ContextElement' ext primTy primVal =
  ContextElement
    { ctxName :: IR.Name
    , ctxAnn  :: {-# UNPACK #-} !(Annotation' ext primTy primVal)
    }

type ContextElement = ContextElement' IR.NoExt

contextElement :: IR.Name -> Usage.T -> IR.Value' ext primTy primVal
               -> ContextElement' ext primTy primVal
contextElement n π t = ContextElement n (Annotation π t)

deriving instance (Eq (IR.Value' ext primTy primVal)) =>
  Eq (ContextElement' ext primTy primVal)

deriving instance (Show (IR.Value' ext primTy primVal)) =>
  Show (ContextElement' ext primTy primVal)

type Context' ext primTy primVal = [ContextElement' ext primTy primVal]

type Context primTy primVal = Context' IR.NoExt primTy primVal

lookupCtx :: IR.Name -> Context' ext primTy primVal
          -> Maybe (Annotation' ext primTy primVal)
lookupCtx x = fmap ctxAnn . find (\e -> ctxName e == x)


data TypecheckError' ext primTy primVal
  = TypeMismatch
      Natural
      (IR.Term' ext primTy primVal)
      (Annotation' ext primTy primVal)
      (Annotation' ext primTy primVal)
  | UniverseMismatch
      (IR.Term' ext primTy primVal)
      (IR.Value' ext primTy primVal)
  | CannotApply (IR.Value' ext primTy primVal) (IR.Value' ext primTy primVal)
  | ShouldBeStar (IR.Value' ext primTy primVal)
  | ShouldBeFunctionType
      (IR.Value' ext primTy primVal)
      (IR.Term' ext primTy primVal)
  | UnboundIndex Natural
  | SigmaMustBeZero
  | UsageMustBeZero
  | UsageNotCompatible
      (Annotation' ext primTy primVal)
      (Annotation' ext primTy primVal)
  | UnboundBinder Natural IR.Name
  | MustBeFunction
      (IR.Elim' ext primTy primVal)
      Natural
      (IR.Term' ext primTy primVal)
  | BoundVariableCannotBeInferred

type TypecheckError = TypecheckError' IR.NoExt

deriving instance
  (Eq primTy, Eq primVal,
   IR.ValueAll Eq ext primTy primVal,
   IR.NeutralAll Eq ext primTy primVal,
   IR.TermAll Eq ext primTy primVal,
   IR.ElimAll Eq ext primTy primVal) ⇒
  Eq (TypecheckError' ext primTy primVal)

instance (Show primTy, Show primVal,
          IR.ValueAll Show ext primTy primVal,
          IR.NeutralAll Show ext primTy primVal,
          IR.TermAll Show ext primTy primVal,
          IR.ElimAll Show ext primTy primVal)
        ⇒ Show (TypecheckError' ext primTy primVal) where
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
  ExceptT (TypecheckError primTy primVal)
    (State (EnvCtx primTy primVal))

newtype EnvTypecheck primTy primVal a = EnvTyp (EnvAlias primTy primVal a)
  deriving (Functor, Applicative, Monad)
  deriving
    (HasThrow "typecheckError" (TypecheckError primTy primVal))
    via MonadError
          ( ExceptT (TypecheckError primTy primVal)
              (MonadState (State (EnvCtx primTy primVal)))
          )
  deriving
    ( HasSink "typecheckerLog" [TypecheckerLog],
      HasWriter "typecheckerLog" [TypecheckerLog]
    )
    via WriterLog
          ( Field "typecheckerLog" ()
              ( MonadState
                  ( ExceptT (TypecheckError primTy primVal)
                      (State (EnvCtx primTy primVal))
                  )
              )
          )


exec ∷ EnvTypecheck primTy primVal a
     → (Either (TypecheckError primTy primVal) a, EnvCtx primTy primVal)
exec (EnvTyp env) = runState (runExceptT env) (EnvCtx [])


data T

do
  let typed = Ext.Ann $ \primTy primVal -> [t|Annotation $primTy $primVal|]
  extT <- IR.extendTerm "Term" [] [t|T|] $
    IR.defaultExtTerm
      { IR.typeStar   = typed
      , IR.typePrimTy = typed
      , IR.typePi     = typed
      , IR.typeLam    = typed
      , IR.typeElim   = typed
      }
  extE <- IR.extendElim "Elim" [] [t|T|] $
    IR.defaultExtElim
      { IR.typeBound = typed
      , IR.typeFree  = typed
      , IR.typePrim  = typed
      , IR.typeApp   = typed
      , IR.typeAnn   = typed
      }
  pure $ extT ++ extE

getTermAnn :: Term primTy primVal -> Annotation primTy primVal
getTermAnn (Star _ ann)   = ann
getTermAnn (PrimTy _ ann) = ann
getTermAnn (Pi _ _ _ ann) = ann
getTermAnn (Lam _ ann)    = ann
getTermAnn (Elim _ ann)   = ann

getElimAnn :: Elim primTy primVal -> Annotation primTy primVal
getElimAnn (Bound _ ann)   = ann
getElimAnn (Free _ ann)    = ann
getElimAnn (Prim _ ann)    = ann
getElimAnn (App _ _ ann)   = ann
getElimAnn (Ann _ _ _ ann) = ann


-- | A \"flexible\" type is one where some of the universe levels might be
-- unknown. The constructor @VStar Natural@ is replaced with
-- @VStar (Maybe Natural)@.
data Flex

IR.extendValue "FlexValue" [] [t|Flex|] $ IR.defaultExtValue
  { IR.typeVStar  = Ext.Disabled
  , IR.typeValueX = [("VStar", \_ _ -> [t|Maybe Natural|])]
  }
IR.extendNeutral "FlexNeutral" [] [t|Flex|] IR.defaultExtNeutral

makeFlexValue :: IR.Value primTy primVal -> FlexValue primTy primVal
makeFlexValue (IR.VStar x) = VStar (Just x)
makeFlexValue (IR.VPrimTy t) = VPrimTy t
makeFlexValue (IR.VPi π s t) = VPi π (makeFlexValue s) (makeFlexValue t)
makeFlexValue (IR.VLam t) = VLam (makeFlexValue t)
makeFlexValue (IR.VNeutral n) = VNeutral (makeFlexNeutral n)
makeFlexValue (IR.VPrim p) = VPrim p

makeFlexNeutral :: IR.Neutral primTy primVal -> FlexNeutral primTy primVal
makeFlexNeutral (IR.NBound x) = NBound x
makeFlexNeutral (IR.NFree x) = NFree x
makeFlexNeutral (IR.NApp n v) = NApp (makeFlexNeutral n) (makeFlexValue v)
