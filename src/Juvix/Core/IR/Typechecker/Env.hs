module Juvix.Core.IR.Typechecker.Env where

import Juvix.Library hiding (Datatype)
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Usage as Usage
import Juvix.Core.IR.Typechecker.Types
import Juvix.Core.IR.Typechecker.Log
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap


data EnvCtx primTy primVal
  = EnvCtx
    { typecheckerLog :: [Log primTy primVal]
    , globals :: Globals primTy primVal
    }
  deriving (Show, Eq, Generic)

type Globals primTy primVal =
  HashMap IR.GlobalName (Global primTy primVal)

data Global primTy primVal
  = GDatatype (Datatype primTy primVal)
  | GDataCon  (DataCon  primTy primVal)
  deriving (Show, Eq, Generic)

data Datatype primTy primVal
  = Datatype
    { dataName  :: IR.GlobalName
      -- | the type constructor's arguments
    , dataArgs  :: [DataArg primTy primVal]
      -- | the type constructor's target universe level
    , dataLevel :: Natural
    , dataCons  :: [DataCon primTy primVal]
    }
  deriving (Show, Eq, Generic)

data DataArg primTy primVal
  = DataArg
    { argName    :: IR.GlobalName
    , argUsage   :: Usage.T
    , argType    :: IR.Value primTy primVal
    , argIsParam :: Bool
    }
  deriving (Show, Eq, Generic)

data DataCon primTy primVal
  = DataCon
    { conName :: IR.GlobalName
    , conType :: IR.Value primTy primVal
    }
  deriving (Show, Eq, Generic)


type EnvAlias primTy primVal =
  ExceptT (TypecheckError primTy primVal)
    (State (EnvCtx primTy primVal))

newtype EnvTypecheck primTy primVal a = EnvTyp (EnvAlias primTy primVal a)
  deriving (Functor, Applicative, Monad)
  deriving
    (HasThrow "typecheckError" (TypecheckError primTy primVal))
    via MonadError (EnvAlias primTy primVal)
  deriving
    (HasSink "typecheckerLog" [Log primTy primVal],
     HasWriter "typecheckerLog" [Log primTy primVal])
  via WriterField "typecheckerLog" (EnvAlias primTy primVal)
  deriving
    (HasSink   "globals" (Globals primTy primVal),
     HasState  "globals" (Globals primTy primVal))
  via StateField "globals" (EnvAlias primTy primVal)
  deriving
    (HasSource "globals" (Globals primTy primVal),
     HasReader "globals" (Globals primTy primVal))
  via ReaderField "globals" (EnvAlias primTy primVal)

exec ::
  Globals primTy primVal ->
  EnvTypecheck primTy primVal a ->
  (Either (TypecheckError primTy primVal) a, EnvCtx primTy primVal)
exec globals (EnvTyp env) =
  runState (runExceptT env) $ EnvCtx [] globals
