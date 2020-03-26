-- |
-- - Sanity checks for Michelson compilation.
module Juvix.Backends.Michelson.Compilation.Checks where

import qualified Juvix.Backends.Michelson.Compilation.Types as Types
import qualified Juvix.Backends.Michelson.Compilation.Util as Util
import qualified Juvix.Backends.Michelson.Compilation.VirtualStack as VStack
import Juvix.Library
import qualified Michelson.TypeCheck as M
import qualified Michelson.Untyped as M

-- Check that the stack types tracked internally & of the instruction match.
stackGuard ::
  ( HasState "stack" VStack.T m,
    HasThrow "compilationError" Types.CompilationError m,
    Show a
  ) =>
  a ->
  M.Type ->
  m (Either b Types.Op) ->
  m (Either b Types.Op)
stackGuard term paramTy func = do
  start <- get @"stack"
  maybeInstr <- func
  end <- get @"stack"
  case maybeInstr of
    Left _ -> pure maybeInstr
    Right instr -> do
      case Util.stackToStack start of
        M.SomeHST startStack -> do
          -- TODO: Real originated contracts.
          let originatedContracts = mempty
              typedChecked = M.typeCheckList [instr] startStack
          case M.runTypeCheck paramTy originatedContracts typedChecked of
            Left err -> throw @"compilationError" (Types.DidNotTypecheck err)
            Right (_ M.:/ (M.AnyOutInstr _)) ->
              throw @"compilationError" (Types.NotYetImplemented "any out instr")
            Right (_ M.:/ (_ M.::: endType)) -> do
              if Util.stackToStack end == M.SomeHST endType
                then pure maybeInstr
                else
                  throw @"compilationError"
                    ( Types.InternalFault
                        ( mconcat
                            [ "stack mismatch while compiling ",
                              show term,
                              " - end stack: ",
                              show end,
                              ", lifted stack: ",
                              show endType
                            ]
                        )
                    )
