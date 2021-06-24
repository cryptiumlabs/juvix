{-# LANGUAGE UndecidableInstances #-}

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
    pattern Sig,
    pattern Star,
    pattern SymT,
    pattern UnitTy,
  )
import qualified Juvix.Core.Erased.Types as Erased
import qualified Juvix.Core.Erased.Types.Base as Erased
import qualified Juvix.Core.HR.Pretty as HR
import Juvix.Core.IR.TransformExt
import qualified Juvix.Core.IR.Typechecker.Types as Typed
import Juvix.Core.IR.Types (GlobalName, GlobalUsage, PatternVar)
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Core.Translate
import Juvix.Library hiding (Datatype, Type, empty)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.PrettyPrint as PP
import Juvix.Library.Usage (Usage)

 -- | `MapPrim` seems to convert `p1` into `p2` given a list of names,
 -- | `ty` and `val` exist for error purposes only
 -- TODO `MapPrim ty val` probably makes more sense,
 -- since it can be curried for `ty` and `val` first
 -- TODO MapPrim should probably be re-written as `newtype`
type MapPrim p1 p2 ty val =
  [NameSymbol.T] -> p1 -> Either (Error ty val) p2

 -- | `Env` represents an environment Gamma of names
 -- | parametised over 2 potentially different types and values
data Env primTy1 primTy2 primVal1 primVal2 = Env
  { -- | `nextName` is an iterator of names
    nextName :: Int,
    -- | `nameStack` maintains all names within the environment
    nameStack :: [NameSymbol.T],
    -- | `mapPrimTy` maps a primitive type into a potentially different type
    mapPrimTy :: MapPrim primTy1 primTy2 primTy1 primVal1,
    -- | `mapPrimVal` maps a primitive value into a potentially different value
    mapPrimVal :: MapPrim primVal1 primVal2 primTy1 primVal1
  }
  deriving (Generic)

-- | MonadTrans composing Exception, Error, State and Env
type EnvEraAlias primTy1 primTy2 primVal1 primVal2 =
  ExceptT
    (Error primTy1 primVal1)
    (State (Env primTy1 primTy2 primVal1 primVal2))

 -- | Promotes EnvEraAlias to a capability
newtype EnvT primTy1 primTy2 primVal1 primVal2 a
  = EnvEra (EnvEraAlias primTy1 primTy2 primVal1 primVal2 a)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "nextName" Int,
      HasSink "nextName" Int,
      HasSource "nextName" Int
    )
    via StateField "nextName" (EnvEraAlias primTy1 primTy2 primVal1 primVal2)
  deriving
    ( HasState "nameStack" [NameSymbol.T],
      HasSink "nameStack" [NameSymbol.T],
      HasSource "nameStack" [NameSymbol.T]
    )
    via StateField "nameStack" (EnvEraAlias primTy1 primTy2 primVal1 primVal2)
  deriving
    ( HasSource "mapPrimTy" (MapPrim primTy1 primTy2 primTy1 primVal1),
      HasReader "mapPrimTy" (MapPrim primTy1 primTy2 primTy1 primVal1)
    )
    via ReaderField "mapPrimTy" (EnvEraAlias primTy1 primTy2 primVal1 primVal2)
  deriving
    ( HasSource "mapPrimVal" (MapPrim primVal1 primVal2 primTy1 primVal1),
      HasReader "mapPrimVal" (MapPrim primVal1 primVal2 primTy1 primVal1)
    )
    via ReaderField "mapPrimVal" (EnvEraAlias primTy1 primTy2 primVal1 primVal2)
  deriving
    (HasThrow "erasureError" (Error primTy1 primVal1))
    via MonadError (EnvEraAlias primTy1 primTy2 primVal1 primVal2)

  -- | `exec` runs all the monads within `EnvEraAlias`
exec ::
  MapPrim primTy1 primTy2 primTy1 primVal1 ->
  MapPrim primVal1 primVal2 primTy1 primVal1 ->
  EnvT primTy1 primTy2 primVal1 primVal2 a ->
  Either (Error primTy1 primVal1) a
exec mt mv (EnvEra m) = evalState (runExceptT m) (Env 0 [] mt mv)

 -- | Erasure error definition
data Error primTy primVal
  = UnsupportedTermT (Typed.Term' primTy primVal)
  | UnsupportedTermE (Typed.Elim' primTy primVal)
  | UnsupportedTypeV (IR.Value primTy primVal)
  | UnsupportedTypeN (IR.Neutral primTy primVal)
  | CannotEraseZeroUsageTerm (Typed.Term' primTy primVal)
  | InternalError Text
  deriving (Generic)

 -- Boilerplate for `Error` to work with the rest of the codebase
deriving instance
  ( Show primTy,
    Show primVal,
    Show (Param.ApplyErrorExtra primTy)
  ) =>
  Show (Error primTy primVal)

deriving instance
  ( Eq primTy,
    Eq primVal,
    Eq (Param.ApplyErrorExtra primTy)
  ) =>
  Eq (Error primTy primVal)

type instance PP.Ann (Error _ _) = HR.PPAnn

instance
  ( PP.PrettySyntax primTy,
    HR.ToPPAnn (PP.Ann primTy),
    PP.PrettySyntax primVal,
    HR.ToPPAnn (PP.Ann primVal)
  ) =>
  PP.PrettyText (Error primTy primVal)
  where
  prettyT = \case
    UnsupportedTermT t ->
      PP.sepIndent'
        [ (False, "Unsupported term:"),
          (True, PP.pretty0 $ irToHR $ extForgetT t)
        ]
    UnsupportedTermE e ->
      PP.sepIndent'
        [ (False, "Unsupported term:"),
          (True, PP.pretty0 $ irToHR $ IR.Elim $ extForgetE e)
        ]
    UnsupportedTypeV v ->
      PP.sepIndent'
        [ (False, "Unsupported type:"),
          (True, PP.pretty0 $ irToHR $ IR.quote v)
        ]
    UnsupportedTypeN n ->
      PP.sepIndent'
        [ (False, "Unsupported type:"),
          (True, PP.pretty0 $ irToHR $ IR.quote $ IR.VNeutral n)
        ]
    CannotEraseZeroUsageTerm t ->
      PP.sepIndent'
        [ (False, "Entire term has zero usage:"),
          (True, PP.pretty0 $ irToHR $ extForgetT t)
        ]
    InternalError txt ->
      PP.text txt

 -- | `Core.Erasure.T`
data T primTy

-- | Is this a top-level `do` ?
-- | Shouldn't we handle this more gracefully? ^^"
-- | Also, why do we need to do this?
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
          Erased.typePair = typed,
          Erased.typeUnit = typed,
          Erased.typeLet = typedTuple,
          Erased.typeApp = typed
        }

 -- TODO Switch to `newtype`
type TermT primTy primVal = Term primTy (Typed.TypedPrim primTy primVal)

-- TODO: Figure out how to do this with extensible.
-- IR.extendDatatype "Datatype" [] [t|T|] extDatatype

  -- | Is it supposed to model type declarations?
data Datatype primTy primVal = Datatype
  { dataName :: GlobalName,
    dataArgs :: [DataArg primTy],
    dataLevel :: Natural,
    dataCons :: [DataCon primTy primVal]
  }

-- TODO: Figure out how to do this with extensible.
-- IR.extendDataArg "DataArg" [] [t|T|] extDataArg

 -- What's `DataArg` supposed to model? Type Arguments?
data DataArg primTy = DataArg
  { argName :: GlobalName,
    argUsage :: Usage,
    argType :: Type primTy
  }

-- TODO: Figure out how to do this with extensible.
-- IR.extendDataCon "DataCon" [] [t|T|] extDataCon

 -- What's `DataCon` supposed to model? Constants?
data DataCon primTy primVal = DataCon
  { conName :: GlobalName,
    conType :: Type primTy,
    conDef :: Maybe (Function primTy primVal)
  }

-- TODO: Figure out how to do this with extensible.
-- IR.extendFunction "Function" [] [t|T|] extFunction

-- | Lambda abstraction
data Function primTy primVal = Function
  { funName :: GlobalName,
    funUsage :: GlobalUsage,
    funType :: Type primTy,
    funClauses :: NonEmpty (FunClause primTy primVal)
  }

 -- | Lambda abstraction extended to primitive types
type FunctionT primTy primVal =
  Function primTy (Typed.TypedPrim primTy primVal)

-- TODO: Figure out how to do this with extensible.
-- IR.extendFunClause "FunClause" [] [t|T|] extFunClause

 -- | Function-level pattern matching
data FunClause primTy primVal
  = FunClause [Pattern primTy primVal] (Term primTy primVal)

 -- | Function-level pattern matching
type FunClauseT primTy primVal =
  FunClause primTy (Typed.TypedPrim primTy primVal)

-- TODO: Figure out how to do this with extensible.
-- IR.extendPattern "Pattern" [] [t|T|] extPattern

 -- | `Pattern` models the current patterns available for match?
data Pattern primTy primVal
  -- `PCon` is for constants?
  = PCon GlobalName [Pattern primTy primVal]
  | PPair (Pattern primTy primVal) (Pattern primTy primVal)
  | PUnit
  | PVar PatternVar
  -- `PDot` is for record field lookup?
  | PDot (Term primTy primVal)
  | PPrim primVal

 -- `PatternT` extends `Patterns` to primitive types
type PatternT primTy primVal =
  Pattern primTy (Typed.TypedPrim primTy primVal)

 -- What's `Abstract` supposed to model?
data Abstract primTy = Abstract
  { absName :: GlobalName,
    absUsage :: GlobalUsage,
    absType :: Type primTy
  }

 -- | `Global` models terms available in the global scope
data Global primTy primVal
  = GDatatype (Datatype primTy primVal)
  | GDataCon (DataCon primTy primVal)
  | GFunction (Function primTy primVal)
  | GAbstract (Abstract primTy)

 -- | `Globals` models the global scope
type Globals primTy primVal = HM.HashMap GlobalName (Global primTy primVal)

 -- | `GlobalT` models primitive types
type GlobalT primTy primVal = Global primTy (Typed.TypedPrim primTy primVal)

 -- | `GlobalsT` models a global scope of primitive types
type GlobalsT primTy primVal = Globals primTy (Typed.TypedPrim primTy primVal)

 -- | `getType` returns the type within a term
getType :: Term primTy primVal -> Type primTy
getType (Var _ ty) = ty
getType (Prim _ ty) = ty
getType (Lam _ _ ty) = ty
getType (Pair _ _ ty) = ty
getType (Unit ty) = ty
getType (Let _ _ _ (_, ty)) = ty
getType (App _ _ ty) = ty

 -- | `eraseAnn` converts `IR.Term` into `Erased.Term`
eraseAnn :: Term primTy primVal -> Erased.Term primVal
eraseAnn (Var sym _) = Erased.Var sym
eraseAnn (Prim p _) = Erased.Prim p
eraseAnn (Lam s b _) = Erased.Lam s (eraseAnn b)
eraseAnn (Pair a b _) = Erased.Pair (eraseAnn a) (eraseAnn b)
eraseAnn (Unit _) = Erased.Unit
eraseAnn (Let s a b _) = Erased.Let s (eraseAnn a) (eraseAnn b)
eraseAnn (App a b _) = Erased.App (eraseAnn a) (eraseAnn b)
