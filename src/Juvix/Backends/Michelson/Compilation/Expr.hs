module Juvix.Backends.Michelson.Compilation.Expr where

import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Text                                  as T
import           Protolude                                  hiding (Const, Type)

import           Juvix.Backends.Michelson.Compilation.Types
import           Juvix.Backends.Michelson.Compilation.Type
import           Juvix.Backends.Michelson.Compilation.Util
import qualified Juvix.Backends.Michelson.Untyped           as M
import           Juvix.Lang
import           Juvix.Utility

import qualified Idris.Core.TT                              as I
import qualified IRTS.Lang                                  as I

exprToMichelson ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m, MonadState M.Stack m) ⇒ Expr → Type → m (M.Expr, M.Type)
exprToMichelson expr ty = (,) <$> exprToExpr expr <*> typeToType ty

{-
 - Transform core expression to Michelson instruction sequence.
 - This requires tracking the stack state.
 - At the moment, this functions maintains a forward mapping from the Haskell expression type and the Michelson stack type.
 - ∷ { Haskell Type } ~ { Stack Pre-Evaluation } ⇒ { Stack Post-Evaluation }
 -}

exprToExpr ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m, MonadState M.Stack m) ⇒ Expr → m M.Expr
exprToExpr expr = do

  let tellReturn ∷ M.Expr → m M.Expr
      tellReturn ret = tell [ExprToExpr expr ret] >> return ret

      notYetImplemented ∷ m M.Expr
      notYetImplemented = throw (NotYetImplemented (prettyPrintValue expr))

      failWith ∷ Text → m M.Expr
      failWith = throw . NotYetImplemented

  case expr of

    -- :: a ~ s => (a, s)
    I.LV (I.NS (I.UN prim) ["Prim", "Tezos"]) -> primToExpr prim []

    -- :: a ~ s => (a, s)
    I.LV name            -> do
      stack <- get
      case position (prettyPrintValue name) stack of
        Nothing → throwError (NotYetImplemented ("variable not in scope: " <> prettyPrintValue name))
        Just i  → do
          let before  = if i == 0 then M.Nop else rearrange i
              after   = if i == 0 then M.Nop else M.Dip (unrearrange i)
          genReturn (M.Seq (M.Seq before M.Dup) after)

    -- ∷ (\a → b) a ~ s ⇒ (b, s)
    I.LApp _ func args   -> do
      args <- mapM exprToExpr args -- Check ordering.
      func <- exprToExpr func
      return (M.Seq (foldl M.Seq M.Nop args) func)

    -- ∷ (\a → b) a ~ s ⇒ (b, s)
    I.LLazyApp func args -> exprToExpr (I.LApp undefined (I.LV func) args)

    -- :: a ~ s => (a, s)
    I.LLazyExp expr      -> exprToExpr expr

    -- :: a ~ s => (a, s)
    I.LForce expr        -> exprToExpr expr

    -- :: a ~ s => (a, s)
    I.LLet name val expr -> notYetImplemented

    -- ∷ \a... → b ~ (a..., s) ⇒ (b, s)
    I.LLam args body     -> do
      -- Ordering: Treat as \a b -> c ~= \a -> \b -> c, e.g. reverse stack order.
      forM_ args (\a -> modify ((:) (M.VarE (prettyPrintValue a))))
      inner <- exprToExpr body
      after ← genReturn (foldDrop (length args))
      tellReturn (M.Seq inner after)

    -- ??
    I.LProj expr num     -> notYetImplemented

    I.LCon _ _ name args -> do
      args <- mapM exprToExpr args
      cons <- dataconToExpr name
      return (M.Seq (foldl M.Seq M.Nop args) cons)

      --throw (NotYetImplemented ("constructor: " <> prettyPrintValue name <> ", args: " <> prettyPrintValue args))

    -- ∷ a ~ s ⇒ (a, s)
    I.LCase ct expr alts -> do
      -- Do we care about the case type?
      -- Generate switch on native repr (never constructor tag except for e.g. data Ord = A | B | C)
      -- Unpack necessary variables in fixed pattern according to desired bindings
      -- Rewrite case with more than two alternatives to nested version.

      start <- get

      let invariant = do
            end <- get
            unless (drop 1 end == start) (throw (NotYetImplemented $ "Case compilation violated stack invariant: start " <> prettyPrintValue start <> ", end " <> prettyPrintValue end))

      -- Evaluate the scrutinee.
      scrutinee <- exprToExpr expr

      evaluand <-
        case alts of
          [I.LConCase _ con@(I.NS (I.UN "MkPair") ["Builtins"]) binds@[a, b] expr] -> do
            unpack <- unpack (M.PairT M.StringT M.StringT) (map (Just . prettyPrintValue) binds)
            expr <- exprToExpr expr
            unpackDrop <- unpackDrop (map (Just . prettyPrintValue) binds)
            return $ foldSeq [scrutinee, unpack, expr, unpackDrop]
          [I.LDefaultCase expr] -> do
            expr <- exprToExpr expr
            unpackDrop <- unpackDrop [Just ""]
            return $ foldSeq [scrutinee, expr, unpackDrop]
          _ -> throw (NotYetImplemented ("case switch: expr " <> prettyPrintValue expr <> " alts " <> T.intercalate ", " (fmap prettyPrintValue alts)))

      invariant

      return evaluand

    -- ∷ a ~ s ⇒ (a, s)
    I.LConst const       -> do
      M.Const |<< constToExpr const

    -- (various)
    I.LForeign _ _ _     -> notYetImplemented

    -- (various)
    I.LOp (I.LExternal (I.NS (I.UN prim) ["Prim", "Tezos"])) args -> primToExpr prim args

    I.LOp prim args      -> do
      args <- mapM exprToExpr args
      prim <- primExprToExpr prim
      return (M.Seq (foldl M.Seq M.Nop args) prim)

    I.LNothing           -> notYetImplemented

    I.LError msg         -> failWith (T.pack msg)

dataconToExpr ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m, MonadState M.Stack m) ⇒ Name → m M.Expr
dataconToExpr name =
  case name of
    I.NS (I.UN "Operation") ["Tezos"] -> do
      throw (NotYetImplemented "tezos.operation")
    I.NS (I.UN "MkPair") ["Builtins"] -> do
      modify ((:) M.FuncResult . drop 2)
      return (M.Seq M.Swap M.ConsPair)
    _ -> throw (NotYetImplemented ("data con: " <> prettyPrintValue name))

primExprToExpr ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m, MonadState M.Stack m) ⇒ Prim → m M.Expr
primExprToExpr prim = do
  let notYetImplemented ∷ m M.Expr
      notYetImplemented = throw (NotYetImplemented $ prettyPrintValue prim)

  case prim of
    I.LPlus (I.ATInt I.ITBig)  -> return M.AddIntInt
    I.LMinus (I.ATInt I.ITBig) -> return M.SubInt

    _                          -> notYetImplemented

constToExpr ∷ ∀ m . MonadError CompilationError m ⇒ Const → m M.Const
constToExpr const = do
  let notYetImplemented ∷ m M.Const
      notYetImplemented = throw (NotYetImplemented (prettyPrintValue const))

  case const of
    I.I v   -> pure (M.Int (fromIntegral v))
    I.BI v  -> pure (M.Int v)
    I.Str s -> pure (M.String (T.pack s))
    _       -> notYetImplemented

primToExpr ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m, MonadState M.Stack m) ⇒ Text → [Expr] -> m M.Expr
primToExpr prim args =
  case (prim, args) of
    ("prim__tezosAmount", []) -> do
      modify ((:) M.FuncResult)
      return M.Amount
    ("prim__tezosNil", [expr]) -> do
      modify ((:) M.FuncResult)
      ty <- exprToType expr
      return $ M.Nil M.OperationT -- TODOM.VarE (prettyPrintValue a))
    ("prim__tezosAddIntInt", [a, b]) -> do
      a <- exprToExpr a
      b <- exprToExpr b
      modify ((:) M.FuncResult . drop 2)
      return (foldSeq [a, b, M.AddIntInt])
    _ -> throw (NotYetImplemented ("primitive: " <> prim))
