module Juvix.Backends.Michelson.Compilation.Expr where

exprToExpr ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m, MonadState M.Stack m) ⇒ Expr → m M.Expr
exprToExpr expr = liftGuard expr $ \expr -> do

  case expr of

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
