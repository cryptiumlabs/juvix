-- |
-- - This serves as the monad in which all operations are run
-- - This is an extension of EACState
--   + Sadly we can't extend types easily in Haskell, hence the
--     boilerplate in this file
module Juvix.Backends.LLVM.Net.EAC.MonadEnvironment where

import Juvix.Backends.LLVM.Codegen hiding (CodegenState (..))
import Juvix.Library hiding (Type)
import qualified Juvix.Library.HashMap as Map
import LLVM.AST as AST

data EACState
  = EACState
      { -- | Name of the active block to append to
        currentBlock :: Name,
        -- | Blocks for function
        blocks :: Map.T Name BlockState,
        -- | Function scope symbol table
        symTab :: SymbolTable,
        -- | Mapping from symbol to Type
        typTab :: TypeTable,
        -- | a mapping from the variants to the sum type
        varTab :: VariantToType,
        -- | Count of basic blocks
        blockCount :: Int,
        -- | Count of unnamed instructions
        count :: Word,
        -- | Name Supply
        names :: Names,
        moduleAST :: AST.Module,
        -- | Debug level
        debug :: Int
        -- new data for EAC!
        --
      }
  deriving (Show, Generic)

newtype EAC a = EACGen {runEAC :: ExceptT Errors (State EACState) a}
  deriving (Functor, Applicative, Monad)
  deriving
    (HasState "currentBlock" Name)
    via Field "currentBlock" () (MonadState (ExceptT Errors (State EACState)))
  deriving
    (HasState "blocks" (Map.T Name BlockState))
    via Field "blocks" () (MonadState (ExceptT Errors (State EACState)))
  deriving
    (HasState "symTab" SymbolTable)
    via Field "symTab" () (MonadState (ExceptT Errors (State EACState)))
  deriving
    (HasState "varTab" VariantToType)
    via Field "varTab" () (MonadState (ExceptT Errors (State EACState)))
  deriving
    (HasState "typTab" TypeTable)
    via Field "typTab" () (MonadState (ExceptT Errors (State EACState)))
  deriving
    (HasState "blockCount" Int)
    via Field "blockCount" () (MonadState (ExceptT Errors (State EACState)))
  deriving
    (HasState "count" Word)
    via Field "count" () (MonadState (ExceptT Errors (State EACState)))
  deriving
    (HasState "names" Names)
    via Field "names" () (MonadState (ExceptT Errors (State EACState)))
  deriving
    (HasThrow "err" Errors)
    via MonadError (ExceptT Errors (State EACState))
  deriving
    (HasState "moduleAST" AST.Module)
    via Field "moduleAST" () (MonadState (ExceptT Errors (State EACState)))
  deriving
    (HasReader "debug" Int)
    via Field "debug" () (ReadStatePure (MonadState (ExceptT Errors (State EACState))))

instance HasState "moduleDefinitions" [Definition] EAC where
  get_ _ = moduleDefinitions <$> (get @"moduleAST")

  put_ _ x = do
    c <- get @"moduleAST"
    put @"moduleAST" (c {moduleDefinitions = x})

  state_ _ state = do
    c <- get @"moduleDefinitions"
    let (a, res) = state c
    put @"moduleDefinitions" res
    pure a

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

emptyEAC :: EACState
emptyEAC =
  EACState
    { currentBlock = mkName entryBlockName,
      blocks = Map.empty,
      symTab = Map.empty,
      typTab = Map.empty,
      varTab = Map.empty,
      count = 0,
      names = Map.empty,
      blockCount = 1,
      moduleAST = emptyModule "EAC",
      debug = 0
    }

emptyEACL1 :: EACState
emptyEACL1 = emptyEAC {debug = 1}

debugLevelOne :: HasReader "debug" Int m => m () -> m ()
debugLevelOne = whenM ((1 <=) <$> ask @"debug")

execEACState :: EAC a -> SymbolTable -> EACState
execEACState (EACGen m) a = execState (runExceptT m) (emptyEAC {symTab = a})

evalEACState :: EAC a -> SymbolTable -> Either Errors a
evalEACState (EACGen m) a = evalState (runExceptT m) (emptyEAC {symTab = a})

execEACStateLevel1 :: EAC a -> SymbolTable -> EACState
execEACStateLevel1 (EACGen m) a = execState (runExceptT m) (emptyEACL1 {symTab = a})

evalEACStateLevel1 :: EAC a -> SymbolTable -> Either Errors a
evalEACStateLevel1 (EACGen m) a = evalState (runExceptT m) (emptyEACL1 {symTab = a})
