module Juvix.Backends.Michelson.Compilation.Expr where

import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Text                                  as T
import           Protolude                                  hiding (Const, Type)

import           Juvix.Backends.Michelson.Compilation.Types
import           Juvix.Backends.Michelson.Compilation.Type
import Juvix.Backends.Michelson.Lift
import           Juvix.Backends.Michelson.Compilation.Util
import qualified Juvix.Backends.Michelson.Untyped           as M
import           Juvix.Lang
import           Juvix.Utility

import qualified Idris.Core.TT                              as I
import qualified IRTS.Lang                                  as I

exprToMichelson ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m, MonadState M.Stack m) ⇒ Expr → Type → m (M.Expr, M.Type)
exprToMichelson expr ty = do
  ty@(M.LamT start end) <- typeToType ty
  modify ((:) (M.FuncResult, start))
  expr <- exprToExpr expr
  return (expr, ty)

{-
 - Transform core expression to Michelson instruction sequence.
 - This requires tracking the stack state.
 - At the moment, this functions maintains a forward mapping from the Haskell expression type and the Michelson stack type.
 - ∷ { Haskell Type } ~ { Stack Pre-Evaluation } ⇒ { Stack Post-Evaluation }
 -}

liftGuard  ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m, MonadState M.Stack m) ⇒ Expr → (Expr → m M.Expr) → m M.Expr
liftGuard expr func = do
  stk <- get
  michelsonExpr <- func expr
  (_, endType) <- liftUntyped michelsonExpr (stackToStack stk) (liftType M.BoolT)
  end <- get
  unless (stackToStack end == endType) $
    throw (InternalFault ("Stack mismatch while compiling " <> prettyPrintValue expr <> " : end stack " <> prettyPrintValue end <> ", lifted stack: " <> prettyPrintValue endType))
  return michelsonExpr

exprToExpr ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m, MonadState M.Stack m) ⇒ Expr → m M.Expr
exprToExpr expr = liftGuard expr $ \expr -> do

  let tellReturn ∷ M.Expr → m M.Expr
      tellReturn ret = tell [ExprToExpr expr ret] >> return ret

      notYetImplemented ∷ m M.Expr
      notYetImplemented = throw (NotYetImplemented (prettyPrintValue expr))

      failWith ∷ Text → m M.Expr
      failWith = throw . NotYetImplemented

      stackGuard ∷ (M.Stack -> M.Stack -> Bool) -> m M.Expr -> m M.Expr
      stackGuard guard func = do
        pre   <- get
        res   <- func
        post  <- get
        unless (guard post pre) $ do
          throw (NotYetImplemented ("compilation violated stack invariant: " <> prettyPrintValue expr))
        return res

      takesOne ∷ M.Stack -> M.Stack -> Bool
      takesOne post pre = post == drop 1 pre

      addsOne ∷ M.Stack -> M.Stack -> Bool
      addsOne post pre = drop 1 post == pre

      changesTop ∷ M.Stack -> M.Stack -> Bool
      changesTop post pre = drop 1 post == drop 1 pre

      lambda ∷ Int -> M.Stack -> M.Stack -> Bool
      lambda nargs post pre = drop nargs pre == drop 1 post

  case expr of

    -- :: a ~ s => (a, s)
    I.LV (I.NS (I.UN prim) ["Prim", "Tezos"]) -> stackGuard addsOne $ do
      primToExpr prim []

    -- :: a ~ s => (a, s)
    I.LV name            -> stackGuard addsOne $ do
      stack <- get
      case position (prettyPrintValue name) stack of
        Nothing → throwError (NotYetImplemented ("variable not in scope: " <> prettyPrintValue name))
        Just i  → do
          let before  = if i == 0 then M.Nop else rearrange i
              after   = if i == 0 then M.Nop else M.Dip (unrearrange i)
          genReturn (M.Seq (M.Seq before M.Dup) after)

    -- ∷ (\a → b) a ~ s ⇒ (b, s)
    I.LApp _ func args   -> stackGuard addsOne $ do
      args <- mapM exprToExpr args -- Check ordering.
      func <- exprToExpr func
      return (M.Seq (foldl M.Seq M.Nop args) func)

    -- ∷ (\a → b) a ~ s ⇒ (b, s)
    I.LLazyApp func args -> stackGuard addsOne $ do
      exprToExpr (I.LApp undefined (I.LV func) args)

    -- :: a ~ s => (a, s)
    I.LLazyExp expr      -> stackGuard addsOne $ do
      exprToExpr expr

    -- :: a ~ s => (a, s)
    I.LForce expr        -> stackGuard addsOne $ do
      exprToExpr expr

    -- :: a ~ s => (a, s)
    I.LLet name val expr -> stackGuard addsOne $ do
      notYetImplemented

    -- ∷ \a... → b ~ (a..., s) ⇒ (b, s)
    I.LLam [arg] body     -> stackGuard (lambda 1) $ do
      modify (\((x, t):xs) -> (M.VarE (prettyPrintValue arg), t):xs)
      inner <- exprToExpr body
      after ← genReturn (foldDrop 1)
      return (M.Seq inner after)

    -- TODO: Multi-arg lambdas.
    -- Ordering: Treat as \a b -> c ~= \a -> \b -> c, e.g. reverse stack order.
    -- forM_ args (\a -> modify ((:) (M.VarE (prettyPrintValue a), M.PairT M.BoolT M.BoolT))) -- TODO

    -- ??
    I.LProj expr num     -> notYetImplemented

    -- ∷ a ~ s ⇒ (a, s)
    I.LCon _ _ name args -> stackGuard addsOne $ do
      dataconToExpr name args

    -- ∷ a ~ s ⇒ (a, s)
    ce@(I.LCase ct expr alts) -> stackGuard addsOne $ do
      -- Do we care about the case type?
      -- Generate switch on native repr (never constructor tag except for e.g. data Ord = A | B | C)
      -- Unpack necessary variables in fixed pattern according to desired bindings
      -- Rewrite case with more than two alternatives to nested version.

      (start :: M.Stack) <- get

      let invariant = do
            end <- get
            unless (drop 1 end == start) (throw (NotYetImplemented $ "Case compilation violated stack invariant: start " <> prettyPrintValue start <> ", end " <> prettyPrintValue end <> " with expr " <> prettyPrintValue ce))

      -- somehow we need the type of the scrutinee
      -- we'll need to look up data constructors anyways...
      -- OR we could track types of stack variables

      -- Evaluate the scrutinee.
      scrutinee <- exprToExpr expr

      stack <- get
      let Just (_, scrutineeType) = head stack

      evaluand <-
        case alts of
          [I.LConCase _ conA bindsA exprA, I.LConCase _ conB bindsB exprB] -> do
            switch <- genSwitch scrutineeType
            switchCase <- do
              (now :: M.Stack) <- get
              unpackA <- unpack scrutineeType (map (Just . prettyPrintValue) bindsA)
              exprA <- exprToExpr exprA
              unpackDropA <- unpackDrop (map (Just . prettyPrintValue) $ drop 1 bindsA)
              (endA :: M.Stack) <- get
              modify (const now)
              unpackB <- unpack scrutineeType (map (Just . prettyPrintValue) bindsB)
              exprB <- exprToExpr exprB
              unpackDropB <- unpackDrop (map (Just . prettyPrintValue) $ drop 1 bindsB)
              endB <- get
              unless (endA == endB) (throw (NotYetImplemented ("case compilation returned unequal stacks: " <> prettyPrintValue endA <> ", " <> prettyPrintValue endB)))
              let caseA = foldSeq [unpackA, exprA, unpackDropA]
                  caseB = foldSeq [unpackB, exprB, unpackDropB]
              return $ switch caseA caseB
            return $ foldSeq [scrutinee, switchCase]
          [I.LConCase _ con binds expr] -> do
            -- later: usage analysis
            unpack <- unpack scrutineeType (map (Just . prettyPrintValue) binds)
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
    I.LConst const       -> stackGuard addsOne $ do
      M.Const |<< constToExpr const

    -- (various)
    I.LForeign _ _ _     -> notYetImplemented

    -- ∷ a ~ s ⇒ (a, s)
    I.LOp (I.LExternal (I.NS (I.UN prim) ["Prim", "Tezos"])) args -> stackGuard addsOne $ do
      primToExpr prim args

    -- ∷ a ~ s ⇒ (a, s)
    I.LOp prim args      -> stackGuard addsOne $ do
      args <- mapM exprToExpr args
      prim <- primExprToExpr prim
      return (M.Seq (foldl M.Seq M.Nop args) prim)

    I.LNothing           -> notYetImplemented

    I.LError msg         -> failWith (T.pack msg)

genSwitch ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m, MonadState M.Stack m) ⇒ M.Type -> m (M.Expr -> M.Expr -> M.Expr)
genSwitch M.BoolT         = return (\x y -> M.If y x) -- TODO
genSwitch (M.EitherT _ _) = return M.IfLeft
genSwitch (M.OptionT _)   = return M.IfNone
genSwitch (M.ListT _)     = return M.IfCons
genSwitch ty              = throw (NotYetImplemented ("genSwitch: " <> prettyPrintValue ty))

dataconToExpr ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m, MonadState M.Stack m) ⇒ Name → [Expr] → m M.Expr
dataconToExpr name args =
  case (name, args) of
    (I.NS (I.UN "True") ["Prim", "Tezos"], []) -> do
      modify ((:) (M.FuncResult, M.BoolT))
      return (M.Const $ M.Bool True)
    (I.NS (I.UN "False") ["Prim", "Tezos"], []) -> do
      modify ((:) (M.FuncResult, M.BoolT))
      return (M.Const $ M.Bool False)
    (I.NS (I.UN "MkPair") ["Builtins"], args@[_, _]) -> do
      args <- mapM exprToExpr args
      modify (\( (_, xT) : (_, yT) : xs) -> (M.FuncResult, M.PairT yT xT) : xs)
      return $ foldSeq (args ++ [M.Swap, M.ConsPair])
    (I.NS (I.UN "Nil") ["Prim", "Tezos"], [expr]) -> do
      ty <- exprToType expr
      modify ((:) (M.FuncResult, M.ListT ty))
      return $ M.Nil ty
    (I.NS (I.UN "Nil") ["Prim", "Tezos"], []) -> do
      -- TODO
      modify ((:) (M.FuncResult, M.ListT M.OperationT))
      return $ M.Nil M.OperationT
    _ -> throw (NotYetImplemented ("data con: " <> prettyPrintValue name))

primExprToExpr ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m, MonadState M.Stack m) ⇒ Prim → m M.Expr
primExprToExpr prim = do
  let notYetImplemented ∷ m M.Expr
      notYetImplemented = throw (NotYetImplemented $ prettyPrintValue prim)

  case prim of
    I.LPlus (I.ATInt I.ITBig)  -> do
      modify ((:) (M.FuncResult, M.IntT) . drop 2)
      return M.AddIntInt
    I.LMinus (I.ATInt I.ITBig) -> do
      modify ((:) (M.FuncResult, M.IntT) . drop 2)
      return M.SubInt

    _                          -> notYetImplemented

constToExpr ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m, MonadState M.Stack m) ⇒ Const → m M.Const
constToExpr const = do
  let notYetImplemented ∷ m M.Const
      notYetImplemented = throw (NotYetImplemented (prettyPrintValue const))


  case const of
    I.I v   -> do
      modify ((:) (M.FuncResult, M.IntT))
      return (M.Int (fromIntegral v))
    I.BI v  -> do
      modify ((:) (M.FuncResult, M.IntT))
      return (M.Int v)
    I.Str s -> do
      modify ((:) (M.FuncResult, M.StringT))
      return (M.String (T.pack s))
    _       -> notYetImplemented

primToExpr ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m, MonadState M.Stack m) ⇒ Text → [Expr] -> m M.Expr
primToExpr prim args =
  case (prim, args) of
    ("prim__tezosAmount", []) -> do
      modify ((:) (M.FuncResult, M.TezT))
      return M.Amount
    ("prim__tezosAddIntInt", [a, b]) -> do
      a <- exprToExpr a
      b <- exprToExpr b
      modify ((:) (M.FuncResult, M.IntT) . drop 2)
      return (foldSeq [a, b, M.AddIntInt])
    ("prim__tezosMulIntInt", [a, b]) -> do
      a <- exprToExpr a
      b <- exprToExpr b
      modify ((:) (M.FuncResult, M.IntT) . drop 2)
      return (foldSeq [a, b, M.MulIntInt])
    ("prim__tezosLtTez", [a, b]) -> do
      a <- exprToExpr a
      b <- exprToExpr b
      modify ((:) (M.FuncResult, M.BoolT) . drop 2)
      return (foldSeq [a, b, M.Lt])
    ("prim__tezosFail", [_]) -> do
      modify ((:) (M.FuncResult, M.UnitT))
      return M.Fail
    _ -> throw (NotYetImplemented ("primitive: " <> prim))
