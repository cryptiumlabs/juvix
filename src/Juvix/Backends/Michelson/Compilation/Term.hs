module Juvix.Backends.Michelson.Compilation.Term where

import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Backends.Michelson.Compilation.Util
import Juvix.Backends.Michelson.Parameterisation
import qualified Juvix.Core.Erased.Types as J
import Juvix.Library
import qualified Michelson.TypeCheck as M
import qualified Michelson.Untyped as M

termToMichelson ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m,
    HasWriter "compilationLog" [CompilationLog] m
  ) ⇒
  Term →
  M.Type →
  m Op
termToMichelson term argTy = do
  modify @"stack" ((:) (FuncResultE, argTy))
  instr ← termToInstr term
  tell @"compilationLog" [TermToInstr term instr]
  pure instr

stackGuard ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m
  ) ⇒
  Term →
  (Term → m Op) →
  m Op
stackGuard term func = do
  start ← get @"stack"
  instr ← func term
  end ← get @"stack"
  case stackToStack start of
    M.SomeHST startStack → do
      -- TODO: Should use M.runTypeCheck instead.
      case M.runTypeCheckTest (M.typeCheckList [instr] startStack) of
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

{-
 - Transform core term to Michelson instruction sequence.
 - This requires tracking the stack (what variables are where).
 - At present, this function enforces a unidirectional mapping from the term type to the Michelson stack type.
 - :: { Haskell Type } ~ { Stack Pre-Evaluation } => { Stack Post-Evaluation }
 -}
termToInstr ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m,
    HasWriter "compilationLog" [CompilationLog] m
  ) ⇒
  Term →
  m Op
termToInstr term = stackGuard term $ \term → do
  let notYetImplemented ∷ m Op
      notYetImplemented = throw @"compilationError" (NotYetImplemented ("termToInstr: " <> show term))

      failWith ∷ Text → m Op
      failWith = throw @"compilationError" . InternalFault

      stackCheck ∷ (Stack → Stack → Bool) → m Op → m Op
      stackCheck guard func = do
        pre ← get @"stack"
        res ← func
        post ← get @"stack"
        if guard post pre
          then pure res
          else failWith ("compilation violated stack invariant: " <> show term)

      primToInstr ∷ PrimVal → m Op
      primToInstr prim =
        case prim of
          -- :: \x -> y ~ (x, s) => (y, s)
          PrimFst → stackCheck (lambda 1) $ do
            genReturn (M.PrimEx (M.CAR "" ""))
          -- :: \x -> y ~ (x, s) => (y, s)
          PrimSnd → stackCheck (lambda 1) $ do
            genReturn (M.PrimEx (M.CDR "" ""))
          -- :: \x y -> a ~ (x, (y, s)) => (a, s)
          PrimPair → stackCheck (lambda 2) $ do
            modify @"stack" (\((_, xT) : (_, yT) : xs) → (FuncResultE, M.Type (M.TPair "" "" xT yT) "") : xs)
            pure (M.PrimEx (M.PAIR "" "" "" ""))
          -- :: a ~ s => (a, s)
          PrimConst const → stackCheck addsOne $ do
            case const of
              M.ValueUnit → do
                modify @"stack" ((:) (FuncResultE, M.Type M.TUnit ""))
                pure (M.PrimEx (M.PUSH "" (M.Type M.TUnit "") M.ValueUnit))
              M.ValueNil → do
                modify @"stack" ((:) (FuncResultE, M.Type (M.TList (M.Type M.TOperation "")) ""))
                pure (M.PrimEx (M.NIL "" "" (M.Type M.TOperation "")))

  case term of
    -- TODO: Right now, this is pretty inefficient, even if optimisations later on sometimes help,
    --       since we copy the variable each time. We should be able to use precise usage information
    --       to relax the stack invariant and avoid duplicating variables that won't be used again.
    -- :: a ~ s => (a, s)
    J.Var n →
      stackCheck addsOne $ do
        stack ← get @"stack"
        case position n stack of
          Nothing → failWith ("variable not in scope: " <> show n)
          Just i → do
            let before = rearrange i
                after = M.PrimEx (M.DIP [unrearrange i])
            genReturn (M.SeqEx [before, M.PrimEx (M.DUP ""), after])
    -- :: (varies)
    J.Prim prim →
      primToInstr prim
    -- :: \a -> b ~ (a, s) => (b, s)
    J.Lam arg body →
      stackCheck (lambda 1) $ do
        modify @"stack" (\((_, t) : xs) → (VarE arg, t) : xs)
        inner ← termToInstr body
        after ← genReturn (foldDrop 1)
        pure (M.SeqEx [inner, after])
    -- TODO (maybe): Multi-arg lambdas.
    -- Ordering: Treat as \a b -> c ~= \a -> \b -> c, e.g. reverse stack order.
    -- forM_ args (\a -> modify ((:) (M.VarE (prettyPrintValue a), M.PairT M.BoolT M.BoolT)))

    -- TODO: This is a hack.
    J.App (J.App func arg1) arg2 →
      stackCheck addsOne $ do
        arg2 ← termToInstr arg2
        arg1 ← termToInstr arg1
        func ← termToInstr func
        pure (M.SeqEx [arg2, arg1, func])
    -- TODO: Linear logic types (when added to core), etc.
    -- :: (\a -> b) a ~ s => (b, s)
    -- Call-by-value (evaluate argument first).
    J.App func arg →
      stackCheck addsOne $ do
        arg ← termToInstr arg
        func ← termToInstr func
        pure (M.SeqEx [arg, func])
    -- TODO: Linear logic types (when added to core), etc.
    _ → notYetImplemented

takesOne ∷ Stack → Stack → Bool
takesOne post pre = post == drop 1 pre

addsOne ∷ Stack → Stack → Bool
addsOne post pre = drop 1 post == pre

changesTop ∷ Stack → Stack → Bool
changesTop post pre = drop 1 post == pre

lambda ∷ Natural → Stack → Stack → Bool
lambda nargs post pre = drop (fromIntegral nargs) pre == drop 1 post

genSwitch ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m,
    HasWriter "compilationLog" [CompilationLog] m
  ) ⇒
  M.T →
  m (Op → Op → Op)
genSwitch M.Tbool = pure (\x y → M.PrimEx (M.IF [y] [x])) -- TODO: Why flipped?
genSwitch (M.TOr _ _ _ _) = pure (\x y → M.PrimEx (M.IF_LEFT [x] [y]))
genSwitch (M.TOption _ _) = pure (\x y → M.PrimEx (M.IF_NONE [x] [y]))
genSwitch (M.TList _) = pure (\x y → M.PrimEx (M.IF_CONS [x] [y]))
genSwitch ty = throw @"compilationError" (NotYetImplemented ("genSwitch: " <> show ty))
