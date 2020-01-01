-- |
-- - Sanity checks for Michelson compilation.
module Juvix.Backends.Michelson.Compilation.Checks where

import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Backends.Michelson.Compilation.Util
import Juvix.Backends.Michelson.Parameterisation
import Juvix.Library
import qualified Michelson.TypeCheck as M
import qualified Michelson.Untyped as M

-- Check that the stack types tracked internally & of the instruction match.
stackGuard ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m
  ) ⇒
  Term →
  M.Type →
  m Op →
  m Op
stackGuard term paramTy func = do
  start ← get @"stack"
  instr ← func
  end ← get @"stack"
  case stackToStack start of
    M.SomeHST startStack → do
      -- TODO: Real originated contracts.
      let originatedContracts = mempty
      case M.runTypeCheck paramTy originatedContracts (M.typeCheckList [instr] startStack) of
        Left err → throw @"compilationError" (DidNotTypecheck err)
        Right (_ M.:/ (M.AnyOutInstr _)) → throw @"compilationError" (NotYetImplemented "any out instr")
        Right (_ M.:/ (_ M.::: endType)) → do
          if stackToStack end == M.SomeHST endType
            then pure instr
            else
              throw @"compilationError"
                ( InternalFault
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
