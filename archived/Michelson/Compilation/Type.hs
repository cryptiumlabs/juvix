module Juvix.Backends.Michelson.Compilation.Type where

import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Text                                  as T
import           Protolude                                  hiding (Const, Type)

import           Juvix.Backends.Michelson.Compilation.Types
import           Juvix.Backends.Michelson.Compilation.Util
import qualified Juvix.Backends.Michelson.Untyped           as M
import           Juvix.Lang
import           Juvix.Utility

import qualified Idris.Core.TT                              as I
import qualified IRTS.Lang                                  as I

exprToType ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m) ⇒ Expr → m M.Type
exprToType expr =
  case expr of
    I.LLazyExp e -> exprToType e
    I.LForce e -> exprToType e
    I.LV (I.NS (I.UN "Operation") ["Prim", "Tezos"]) -> return M.OperationT
    I.LCon _ _ (I.NS (I.UN "Operation") ["Prim", "Tezos"]) _ -> return M.OperationT
    I.LV _ -> throw (NotYetImplemented ("exprToType (var): " <> prettyPrintValue expr))
    _ -> throw (NotYetImplemented ("exprToType: " <> prettyPrintValue expr))

typeToType ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m) ⇒ Type → m M.Type
typeToType ty =
  case ty of
    I.P _ name _ ->
      case name of
        I.NS (I.UN "Operation") ["Prim", "Tezos"] -> return M.OperationT
        I.NS (I.UN "Integer") ["Prim", "Tezos"] -> return M.IntT
        I.NS (I.UN "Tez") ["Prim", "Tezos"] -> return M.TezT
        I.NS (I.UN "Bool") ["Prim", "Tezos"] -> return M.BoolT
        I.UN "Unit" -> return M.UnitT
        _ -> throw (NotYetImplemented ("typeToType: unknown name " <> prettyPrintValue ty))
    I.V _ -> throw (NotYetImplemented "typeToType: cannot resolve deBrujin-indexed variables")
    I.App _ (I.App _ (I.P _ (I.NS (I.UN "Pair") ["Builtins"]) _) fst) snd -> do
      fst <- typeToType fst
      snd <- typeToType snd
      return $ M.PairT fst snd
    I.App _ (I.P _ (I.NS (I.UN "List") ["Prim", "Tezos"]) _) elemTy -> do
      elemTy <- typeToType elemTy
      return $ M.ListT elemTy
    I.App _ _ _ -> do
       throw (NotYetImplemented ("typeToType app: " <> prettyPrintValue ty))
    -- this is clumsy, is Idris replacing these?
    I.Constant const -> do
      case const of
        I.I _ -> return M.IntT
        I.BI _ -> return M.IntT
        I.AType (I.ATInt _) -> return M.IntT
        I.StrType -> return M.StringT
        _ -> throw (NotYetImplemented ("typeToType const: " <> prettyPrintValue const))
    I.Proj _ _ -> do
       throw (NotYetImplemented ("typeToType proj: " <> prettyPrintValue ty))
    I.Inferred ty -> typeToType ty
    I.Bind _ (I.Pi _ _ argTy _) retTy -> do
      argTy <- typeToType argTy
      retTy <- typeToType retTy
      return $ M.LamT argTy retTy
    _ -> throw (NotYetImplemented ("typeToType: " <> prettyPrintValue ty))
