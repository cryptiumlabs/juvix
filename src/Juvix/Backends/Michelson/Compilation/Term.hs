module Juvix.Backends.Michelson.Compilation.Term where

import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Backends.Michelson.Compilation.Util
import Juvix.Backends.Michelson.Parameterisation
import qualified Juvix.Core.ErasedAnn.Types as J
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
termToMichelson term paramTy = do
  modify @"stack" ((FuncResultE, paramTy) :)
  instr ← termToInstr term paramTy
  tell @"compilationLog" [TermToInstr term instr]
  pure instr

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

{-
 - Transform core term to Michelson instruction sequence.
 - This requires tracking the stack (what variables are where).
 - At present, this function enforces a unidirectional mapping from the term type to the Michelson stack type.
 - TODO: Right now, usage information is ignored. It should be used in the future to avoid unnecessary stack elements.
 - :: { Haskell Type } ~ { Stack Pre-Evaluation } => { Stack Post-Evaluation }
 -}
termToInstr ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m,
    HasWriter "compilationLog" [CompilationLog] m
  ) ⇒
  Term →
  M.Type →
  m Op
termToInstr ann@(term, _, ty) paramTy = stackGuard ann paramTy $ do
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
          PrimFst → stackCheck addsOne $ do
            let J.Pi _ (J.PrimTy (PrimTy (M.Type pair@(M.TPair _ _ xT _) _))) _ = ty

                lamTy = M.Type (M.TLambda (M.Type pair "") xT) ""

                retTy = M.Type (M.TPair "" "" (M.Type M.TUnit "") lamTy) ""

            modify @"stack" ((:) (FuncResultE, retTy))
            pure
              ( M.PrimEx
                  ( M.PUSH
                      ""
                      retTy
                      ( M.ValuePair
                          M.ValueUnit
                          (M.ValueLambda (M.PrimEx (M.CAR "" "") :| []))
                      )
                  )
              )
          -- :: \x -> y ~ (x, s) => (y, s)
          PrimSnd → stackCheck addsOne $ do
            -- TODO: return lambda (pair x y) y CDR
            genReturn (M.PrimEx (M.CDR "" ""))
            notYetImplemented
          -- :: \x y -> a ~ (x, (y, s)) => (a, s)
          PrimPair → stackCheck addsOne $ do
            -- TODO: return lambda (x, y) (x, y)
            modify @"stack" (\((_, xT) : (_, yT) : xs) → (FuncResultE, M.Type (M.TPair "" "" xT yT) "") : xs)
            pure (M.PrimEx (M.PAIR "" "" "" ""))
            notYetImplemented
          -- :: a ~ s => (a, s)
          PrimConst const → stackCheck addsOne $ do
            let J.PrimTy (PrimTy t) = ty
            modify @"stack" ((:) (FuncResultE, t))
            pure (M.PrimEx (M.PUSH "" t const))

  case term of
    -- TODO: Right now, this is pretty inefficient, even if optimisations later on sometimes help,
    --       since we copy the variable each time. We should be able to use precise usage information
    --       to relax the stack invariant and avoid duplicating variables that won't be used again.
    -- TODO: There is probably some nicer sugar for this in Michelson now.
    -- Variable: find the variable in the stack & duplicate it at the top.
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
    -- Primitive: varies.
    -- :: (varies)
    J.Prim prim →
      primToInstr prim
    -- :: \a -> b ~ s => ((vars, Lam a b), s)
    J.Lam arg body →
      stackCheck addsOne $ do
        -- TODO FIXME
        modify @"stack" (\((_, t) : xs) → (VarE arg, t) : xs)
        inner ← termToInstr body paramTy
        after ← genReturn (foldDrop 1)
        pure (M.SeqEx [inner, after])
    {-
    -- TODO: Will this work in all cases?
    -- Consider app, e.g. (\f -> f 1 2) pair, seems problematic.
    -- :: (\a ... {n} b -> c) a ... {n} b ~ (a, ... {n} (b, s)) => (c, s)
    J.App func arg →
      stackCheck addsOne $ do
        let rec f args =
              case f of
                J.App f a → rec f (a : args)
                _ → (f, args)
            (fn, args) = rec func [arg]
        args ← mapM (flip termToInstr paramTy) (reverse args)
        func ← termToInstr fn paramTy
        pure (M.SeqEx (args <> [func]))
    -}

    -- :: (\a -> b) a ~ (a, s) => (b, s)
    -- Call-by-value (evaluate argument first).
    J.App func arg →
      stackCheck addsOne $ do
        func ← termToInstr func paramTy -- :: (vars, Lam (a, vars) b)
        arg ← termToInstr arg paramTy -- :: a
            -- Then pair up (a) with (vars) and execute the function.
        pure
          ( M.SeqEx
              [ func,
                arg,
                M.PrimEx (M.DIP [M.SeqEx [M.PrimEx (M.DUP ""), M.PrimEx (M.CAR "" ""), M.PrimEx (M.DIP [M.PrimEx (M.CDR "" "")])]]),
                M.PrimEx (M.PAIR "" "" "" ""),
                M.PrimEx (M.EXEC "")
              ]
          )

takesOne ∷ Stack → Stack → Bool
takesOne post pre = post == drop 1 pre

addsOne ∷ Stack → Stack → Bool
addsOne post pre = drop 1 post == pre

changesTop ∷ Stack → Stack → Bool
changesTop post pre = drop 1 post == pre

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
