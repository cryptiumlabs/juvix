module Juvix.Backends.Michelson.Compilation where

import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Text                                  as T
import           Protolude                                  hiding (Type, catch)

import           Juvix.Backends.Michelson.Compilation.Expr
import           Juvix.Backends.Michelson.Compilation.Types
import           Juvix.Backends.Michelson.Compilation.Util
import qualified Juvix.Backends.Michelson.Emit              as M
import qualified Juvix.Backends.Michelson.Lift              as M
import qualified Juvix.Backends.Michelson.Optimization      as M
import qualified Juvix.Backends.Michelson.Typed             as M
import qualified Juvix.Backends.Michelson.Untyped           as MU
import           Juvix.Lang
import           Juvix.Utility

compileToMichelsonSourceFile ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m) ⇒ Expr → Type → m Text
compileToMichelsonSourceFile expr ty = do
  (M.SomeExpr code, paramTy, _, storageTy) <- compileToMichelson expr ty
  return $ T.unlines [
    "parameter " <> M.emitType paramTy <> ";",
    "storage " <> M.emitType storageTy <> ";",
    "code " <> M.emitFinal code <> ";"
    ]

compileToMichelson ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m) ⇒ Expr → Type → m (M.SomeExpr, MU.Type, MU.Type, MU.Type)
compileToMichelson expr ty = do
  ((michelsonExpr', michelsonExprType), _) <- runStateT (exprToMichelson expr ty) []
  let michelsonExpr = leftSeq michelsonExpr'
  case michelsonExprType of
    MU.LamT start@(MU.PairT paramTy startStorageTy) end@(MU.PairT retTy endStorageTy) | startStorageTy == endStorageTy → do
      case (M.liftType paramTy, M.liftType startStorageTy, M.liftType retTy, M.liftType endStorageTy) of
        (DynamicType (Proxy ∷ Proxy paramTyLifted), DynamicType (Proxy ∷ Proxy startStorageTyLifted), DynamicType (Proxy ∷ Proxy retTyLifted), DynamicType (Proxy ∷ Proxy endStorageTyLifted)) → do
          (M.SomeExpr (expr ∷ M.Expr (M.Stack a) (M.Stack b)), _) ← M.liftUntyped michelsonExpr (M.typeToStack start) (DynamicType (Proxy :: Proxy startStorageTyLifted))
          case (eqT ∷ Maybe (a :~: (M.Pair paramTyLifted startStorageTyLifted, ())), eqT ∷ Maybe (b :~: (M.Pair retTyLifted endStorageTyLifted, ()))) of
            (Just Refl, Just Refl) → do
              optimized <- M.optimize expr
              return (M.SomeExpr optimized, paramTy, retTy, startStorageTy)
            _ →
              throw (InternalFault ("cannot unify start/end stack types - start: " <> prettyPrintType (undefined :: a) <>
                " but expected " <> prettyPrintType (undefined :: M.Pair startStorageTyLifted paramTyLifted, ()) <> ", end: "
                <> prettyPrintType (undefined :: b) <> " but expected " <> prettyPrintType (undefined :: M.Pair retTyLifted endStorageTyLifted, ()) <>
                ", expr: " <> prettyPrintValue michelsonExpr))
    ty -> throw (InvalidInput ("invalid type for main function: " <> prettyPrintValue ty))
