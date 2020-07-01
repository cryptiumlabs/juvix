module Juvix.Core.Erasure.Types
  ( module Juvix.Core.Erasure.Types,
    module Type,
  )
  where

import qualified Extensible as Ext
import qualified Juvix.Core.Erased.Types.Base
import qualified Juvix.Core.Erased.Types.Base as Erased
import Juvix.Core.Erased.Types as Type
  (Type, pattern SymT, pattern Star, pattern PrimTy, pattern Pi)
import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Typechecker as TC
import qualified Juvix.Core.IR.Typechecker.Types as Typed
import Juvix.Library hiding (empty, Type)

data Env primTy primVal = Env { nextName :: Int, nameStack :: [Symbol] }
  deriving Generic

type EnvEraAlias primTy primVal =
  ExceptT (Error primTy primVal) (State (Env primTy primVal))

newtype EnvT primTy primVal a
  = EnvEra (EnvEraAlias primTy primVal a)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "nextName" Int,
      HasSink "nextName" Int,
      HasSource "nextName" Int
    )
    via StateField "nextName" (EnvEraAlias primTy primVal)
  deriving
    ( HasState "nameStack" [Symbol],
      HasSink "nameStack" [Symbol],
      HasSource "nameStack" [Symbol]
    )
    via StateField "nameStack" (EnvEraAlias primTy primVal)
  deriving
    (HasThrow "erasureError" (Error primTy primVal))
    via MonadError (EnvEraAlias primTy primVal)

exec :: EnvT primTy primVal a
     -> Either (Error primTy primVal) a
exec (EnvEra m) = evalState (runExceptT m) (Env 0 [])

data Error primTy primVal
  = UnsupportedTermT (Typed.Term primTy primVal)
  | UnsupportedTermE (Typed.Elim primTy primVal)
  | UnsupportedTypeV (IR.Value primTy primVal)
  | UnsupportedTypeN (IR.Neutral primTy primVal)
  | CannotEraseZeroUsageTerm (Typed.Term primTy primVal)
  | TypeError (TC.TypecheckError primTy primVal)
  | InternalError Text
  deriving (Show, Eq, Generic)


data T primTy

do
  primTy' <- Ext.newName "primTy"
  let primTy = Ext.varT primTy'
  let typed = Just [[t|Type $primTy|]]
  Erased.extendTerm "Term" [primTy'] [t|T $primTy|] $
    \_ -> Erased.defaultExtTerm {
      Erased.typeVar = typed,
      Erased.typePrim = typed,
      Erased.typeLam = typed,
      Erased.typeLet = typed,
      Erased.typeApp = typed
    }

getType :: Term primTy primVal -> Type primTy
getType (Var _ ty) = ty
getType (Prim _ ty) = ty
getType (Lam _ _ ty) = ty
getType (Let _ _ _ ty) = ty
getType (App _ _ ty) = ty
