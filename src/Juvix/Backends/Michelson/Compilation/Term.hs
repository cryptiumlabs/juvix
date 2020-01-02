-- |
-- - Compilation of core terms to Michelson instruction sequences.
module Juvix.Backends.Michelson.Compilation.Term where

import Juvix.Backends.Michelson.Compilation.Checks
import Juvix.Backends.Michelson.Compilation.Prim
import Juvix.Backends.Michelson.Compilation.Type
import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Backends.Michelson.Compilation.Util
import Juvix.Backends.Michelson.Parameterisation
import qualified Juvix.Core.Erased.Util as J
import qualified Juvix.Core.ErasedAnn as J
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
  case term of
    (J.Lam arg body, _, _) → do
      modify @"stack" ((VarE arg, paramTy) :)
      instr' ← termToInstr body paramTy
      let instr = M.SeqEx [instr', M.PrimEx (M.DIP [M.PrimEx M.DROP])]
      modify @"stack" (\(x : _ : xs) → x : xs)
      tell @"compilationLog" [TermToInstr body instr]
      pure instr
    _ → throw @"compilationError" (NotYetImplemented "must be a lambda function")

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
  let stackCheck ∷ (Stack → Stack → Bool) → m Op → m Op
      stackCheck guard func = do
        pre ← get @"stack"
        res ← func
        post ← get @"stack"
        if guard post pre
          then pure res
          else failWith ("compilation violated stack invariant: " <> show term <> " prior stack " <> show pre <> " posterior stack " <> show post)
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
    -- Primitive: adds one item to the stack.
    J.Prim prim → stackCheck addsOne (primToInstr prim ty)
    -- :: \a -> b ~ s => (Lam a b, s)
    J.Lam arg body →
      stackCheck addsOne $ do
        let J.Pi _ argTy retTy = ty
        argTy ← typeToType argTy
        stack ← get @"stack"
        let free = J.free (J.eraseTerm term)
            freeWithTypes = map (\v → let Just t = lookupType v stack in (v, t)) free
        modify @"stack" ((<>) [(FuncResultE, M.Type M.TUnit ""), (FuncResultE, M.Type M.TUnit "")]) -- TODO: second is a lambda, but it doesn't matter, this just needs to be positionally accurate for var lookup.
        vars ← mapM (\f → termToInstr (J.Var f, undefined, undefined) paramTy) free
        packOp ← packClosure free
        put @"stack" []
        let argUnpack = M.SeqEx [M.PrimEx (M.DUP ""), M.PrimEx (M.CDR "" "")]
        unpackOp ← unpackClosure freeWithTypes
        modify @"stack" ((:) (VarE arg, argTy))
        inner ← termToInstr body paramTy
        dropOp ← dropClosure ((arg, argTy) : freeWithTypes)
        post ← get @"stack"
        let ((_, retTy) : _) = post
        let (lTy, rTy) = lamRetTy freeWithTypes argTy retTy
        put @"stack" ((FuncResultE, rTy) : stack)
        pure
          ( M.SeqEx
              [ M.PrimEx (M.PUSH "" lTy (M.ValueLambda (argUnpack :| [M.PrimEx (M.DIP [M.PrimEx (M.CAR "" ""), unpackOp]), inner, dropOp]))),
                M.PrimEx (M.PUSH "" (M.Type M.TUnit "") M.ValueUnit),
                -- Evaluate everything in the closure.
                M.SeqEx vars,
                -- Pack the closure.
                packOp,
                -- Partially apply the function.
                M.PrimEx (M.APPLY "")
              ]
          )
    -- Special-case full application of primitive functions.
    J.App (J.App (J.Prim prim, _, _) a, _, _) b | arity prim == 2 →
      stackCheck addsOne $ do
        args ← mapM (flip termToInstr paramTy) [b, a]
        -- TODO
        func ← genReturn (M.PrimEx (M.PAIR "" "" "" ""))
        pure (M.SeqEx (args <> [func]))
    -- :: (\a -> b) a ~ (a, s) => (b, s)
    -- Call-by-value (evaluate argument first).
    J.App func arg →
      stackCheck addsOne $ do
        func ← termToInstr func paramTy -- :: Lam a b
        arg ← termToInstr arg paramTy -- :: a
        stack ← get @"stack"
        modify @"stack" (\(_ : (_, (M.Type (M.TLambda _ retTy) _)) : xs) → (FuncResultE, retTy) : xs)
        pure
          ( M.SeqEx
              [ func, -- Evaluate the function.
                arg, -- Evaluate the argument.
                M.PrimEx (M.EXEC "") -- Execute the function.
              ]
          )

takesOne ∷ Stack → Stack → Bool
takesOne post pre = post == drop 1 pre

addsOne ∷ Stack → Stack → Bool
addsOne post pre = drop 1 post == pre

changesTop ∷ Stack → Stack → Bool
changesTop post pre = drop 1 post == pre
