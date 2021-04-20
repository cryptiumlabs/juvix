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

data EACState = EACState
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

type EACAlias = ExceptT Errors (State EACState)

newtype EAC a = EACGen {runEAC :: EACAlias a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "currentBlock" Name,
      HasSink "currentBlock" Name,
      HasSource "currentBlock" Name
    )
    via StateField "currentBlock" EACAlias
  deriving
    ( HasState "blocks" (Map.T Name BlockState),
      HasSink "blocks" (Map.T Name BlockState),
      HasSource "blocks" (Map.T Name BlockState)
    )
    via StateField "blocks" EACAlias
  deriving
    ( HasState "symTab" SymbolTable,
      HasSink "symTab" SymbolTable,
      HasSource "symTab" SymbolTable
    )
    via StateField "symTab" EACAlias
  deriving
    ( HasState "varTab" VariantToType,
      HasSink "varTab" VariantToType,
      HasSource "varTab" VariantToType
    )
    via StateField "varTab" EACAlias
  deriving
    ( HasState "typTab" TypeTable,
      HasSink "typTab" TypeTable,
      HasSource "typTab" TypeTable
    )
    via StateField "typTab" EACAlias
  deriving
    ( HasState "blockCount" Int,
      HasSink "blockCount" Int,
      HasSource "blockCount" Int
    )
    via StateField "blockCount" EACAlias
  deriving
    ( HasState "count" Word,
      HasSink "count" Word,
      HasSource "count" Word
    )
    via StateField "count" EACAlias
  deriving
    ( HasState "names" Names,
      HasSink "names" Names,
      HasSource "names" Names
    )
    via StateField "names" EACAlias
  deriving
    (HasThrow "err" Errors)
    via MonadError EACAlias
  deriving
    ( HasState "moduleAST" AST.Module,
      HasSink "moduleAST" AST.Module,
      HasSource "moduleAST" AST.Module
    )
    via StateField "moduleAST" EACAlias
  deriving
    ( HasReader "debug" Int,
      HasSource "debug" Int
    )
    via ReaderField "debug" EACAlias

instance HasState "moduleDefinitions" [Definition] EAC where
  state_ _ state = do
    c <- get @"moduleDefinitions"
    let (a, res) = state c
    put @"moduleDefinitions" res
    pure a

instance HasSink "moduleDefinitions" [Definition] EAC where
  yield_ _ x = do
    c <- get @"moduleAST"
    put @"moduleAST" (c {moduleDefinitions = x})

instance HasSource "moduleDefinitions" [Definition] EAC where
  await_ _ = moduleDefinitions <$> (get @"moduleAST")

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
