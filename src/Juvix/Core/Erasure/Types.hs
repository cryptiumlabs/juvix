module Juvix.Core.Erasure.Types
  ( module Juvix.Core.Erasure.Types,
    module Type,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Extensible as Ext
import Juvix.Core.Erased.Types as Type
  ( Type,
    pattern Pi,
    pattern PrimTy,
    pattern Star,
    pattern SymT,
  )
import qualified Juvix.Core.Erased.Types as Erased
import qualified Juvix.Core.Erased.Types.Base as Erased
import qualified Juvix.Core.IR.Typechecker as TC
import qualified Juvix.Core.IR.Typechecker.Types as Typed
import Juvix.Core.IR.Types (GlobalName, GlobalUsage, PatternVar)
import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.Usage (Usage)
import Juvix.Library hiding (Datatype, Type, empty)

data Env primTy primVal = Env {nextName :: Int, nameStack :: [Symbol]}
  deriving (Generic)

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

exec ::
  EnvT primTy primVal a ->
  Either (Error primTy primVal) a
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
  let typedTuple = Just [[t|(Type $primTy, Type $primTy)|]]
  Erased.extendTerm "Term" [primTy'] [t|T $primTy|] $
    \_ ->
      Erased.defaultExtTerm
        { Erased.typeVar = typed,
          Erased.typePrim = typed,
          Erased.typeLam = typed,
          Erased.typeLet = typedTuple,
          Erased.typeApp = typed
        }

-- TODO: Figure out how to do this with extensible.
-- IR.extendDatatype "Datatype" [] [t|T|] extDatatype

data Datatype primTy
  = Datatype
      { dataName :: GlobalName,
        dataArgs :: [DataArg primTy],
        dataLevel :: Natural,
        dataCons :: [DataCon primTy]
      }

-- TODO: Figure out how to do this with extensible.
-- IR.extendDataArg "DataArg" [] [t|T|] extDataArg

data DataArg primTy
  = DataArg
      { argName :: GlobalName,
        argUsage :: Usage,
        argType :: Type primTy,
        argIsParam :: Bool
      }

-- TODO: Figure out how to do this with extensible.
-- IR.extendDataCon "DataCon" [] [t|T|] extDataCon

data DataCon primTy
  = DataCon
      { conName :: GlobalName,
        conType :: Type primTy
      }

-- TODO: Figure out how to do this with extensible.
-- IR.extendFunction "Function" [] [t|T|] extFunction

data Function primTy primVal
  = Function
      { funName :: GlobalName,
        funUsage :: GlobalUsage,
        funType :: Type primTy,
        funClauses :: NonEmpty (FunClause primTy primVal)
      }

-- TODO: Figure out how to do this with extensible.
-- IR.extendFunClause "FunClause" [] [t|T|] extFunClause

data FunClause primTy primVal
  = FunClause [Pattern primTy primVal] (Term primTy primVal)

-- TODO: Figure out how to do this with extensible.
-- IR.extendPattern "Pattern" [] [t|T|] extPattern

data Pattern primTy primVal
  = PCon GlobalName [Pattern primTy primVal]
  | PVar PatternVar
  | PDot (Term primTy primVal)
  | PPrim primVal

data Global primTy primVal
  = GDatatype (Datatype primTy)
  | GDataCon (DataCon primTy)
  | GFunction (Function primTy primVal)
  | GAbstract GlobalUsage (Term primTy primVal)

type Globals primTy primVal = HM.HashMap GlobalName (Global primTy primVal)

getType :: Term primTy primVal -> Type primTy
getType (Var _ ty) = ty
getType (Prim _ ty) = ty
getType (Lam _ _ ty) = ty
getType (Let _ _ _ (_, ty)) = ty
getType (App _ _ ty) = ty

eraseAnn :: Term primTy primVal -> Erased.Term primVal
eraseAnn (Var sym _) = Erased.Var sym
eraseAnn (Prim p _) = Erased.Prim p
eraseAnn (Lam s b _) = Erased.Lam s (eraseAnn b)
eraseAnn (Let s a b _) = Erased.Let s (eraseAnn a) (eraseAnn b)
eraseAnn (App a b _) = Erased.App (eraseAnn a) (eraseAnn b)
