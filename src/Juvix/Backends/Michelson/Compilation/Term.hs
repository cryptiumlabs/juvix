-- |
-- - Compilation of core terms to Michelson instruction sequences.
module Juvix.Backends.Michelson.Compilation.Term where

-- import qualified Michelson.TypeCheck as M

import Data.Maybe (fromJust) -- bad remove!
import Juvix.Backends.Michelson.Compilation.Checks
import Juvix.Backends.Michelson.Compilation.Prim
import Juvix.Backends.Michelson.Compilation.Type
import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Backends.Michelson.Compilation.Util
import qualified Juvix.Backends.Michelson.Compilation.VirtualStack as VStack
import Juvix.Backends.Michelson.Parameterisation

import qualified Juvix.Core.ErasedAnn as J
import qualified Juvix.Core.Usage as Usage
import Juvix.Library
import qualified Michelson.Untyped as M

{-
 - Transform core term to Michelson instruction sequence.
 - This requires tracking the stack (what variables are where).
 - At present, this function enforces a unidirectional mapping from
   the term type to the Michelson stack type.
 - TODO: Right now, usage information is ignored. It should be used in
   the future to avoid unnecessary stack elements.
 - :: { Haskell Type } ~ { Stack Pre-Evaluation } => { Stack Post-Evaluation }
 -}
termToInstr ∷
  ∀ m.
  ( HasState "stack" VStack.T m,
    HasThrow "compilationError" CompilationError m,
    HasWriter "compilationLog" [CompilationLog] m
  ) ⇒
  Term →
  M.Type →
  m (Either LamPartial Op)
termToInstr ann@(term, _, ty) paramTy = stackGuard ann paramTy $ do
  case term of
    -- TODO: Right now, this is pretty inefficient, even if
    --       optimisations later on sometimes help,
    --       since we copy the variable each time. We should be able
    --       to use precise usage information
    --       to relax the stack invariant and avoid duplicating
    --       variables that won't be used again.
    -- TODO: There is probably some nicer sugar for this in Michelson now.
    -- Variable: find the variable in the stack & duplicate it at the top.
    -- :: a ~ s => (a, s)
    J.Var n → pure <$> varCase term n
    -- Primitive: adds one item to the stack.
    J.Prim prim → pure <$> stackCheck term addsOne (primToInstr prim ty)
    -- :: \a -> b ~ s => (Lam a b, s)
    J.LamM capture args body → do
      -- ~~
      -- We will either inline or compile to a lambda, and we don't yet know which because
      -- we don't know how many arguments this function is being applied to.
      -- We are ASSUMING that all names are unique, so captures will be in the stack,
      -- and will be kept around & available for lookup when we later inline the body.
      -- ~~
      -- Note: lambdas don't consume their args BUT builtins DO so we must copy (for now) before inlining a built-in
      pure (Left (LamPartial [] capture args body ty))
    -- :: (\a -> b) a ~ (a, s) => (b, s)
    -- Call-by-value (evaluate argument first).
    J.App _ _ → do
      -- TODO: figure out how to do stack check here
        pre ← get @"stack"
        let (lam, args) = argsFromApps ann
        case lam of
          (J.LamM capture arguments body, _usage, lamTy) → do
            let argsL = length args
                lamArgsL = length arguments
            insts <- evaluateAndPushArgs arguments lamTy args paramTy
            if
              | argsL == lamArgsL → do
                -- need to assert that we correctly drop them later, this is complicated!
                -- need to pass things to be dropped along with the partially applied function datatype
                eitherFuncOp ← termToInstr body paramTy
                case eitherFuncOp of
                  Right f -> do
                    -- Fully applied case without a lambda: evaluate all the arguments, inline the function, drop the args.
                    pure $ Right
                      ( M.SeqEx
                          ( insts
                              <> [ f,
                                   M.PrimEx
                                    (M.DIP (replicate argsL (M.PrimEx M.DROP)))
                                ]
                          )
                      )
                  Left (LamPartial ops captures remArgs body ty) -> do
                    -- Fully applied case with a returned lambda: return the instructions followed by the lambda, defer evaluation of the body.
                    pure (Left (LamPartial (insts <> ops) captures remArgs body ty))
              | argsL < lamArgsL → do
                -- todo: the lambda type is now wrong, fix this, must eat n arguments
                let (evaluatedArgs, remainingArgs) = splitAt argsL arguments
                -- Here we are turning formerly bound arguments into captures.
                pure (Left (LamPartial insts (capture <> evaluatedArgs) remainingArgs body ty))
              | otherwise → do
                -- argsL > lamArgsL
                -- TODO
                -- Add `inline` option to `termToInstr` --> superseded by additional return form
                -- When `f` is fully applied, inline it
                -- When `f` is overapplied, inline it, but if there is then a function in the body of `f`, don't inline that
                --    (this will hit a `Lam` case in the body of `f` or when compiling an argument)
                --    unless that function is also fully applied, in which case do inline it
                --    ~~> need some way to return a partially applied function that will either be compiled to a lambda or
                --    will be inlined if it is fully applied in an enclosing application node
                let (args, extraApplied) = splitAt lamArgsL args
                eitherFuncOp <- termToInstr body paramTy
                case eitherFuncOp of
                  Right _ -> failWith "invalid case"
                  Left (LamPartial ops captures args body ty) -> do
                    case compare (length args) (length extraApplied) of
                      EQ -> do
                        body <- termToInstr body paramTy
                        case body of
                          Right b -> do
                            pure (Right (M.SeqEx (insts <> ops <> [b])))
                          Left _ -> undefined
                      GT -> do
                        -- todo: the lambda type is now wrong, fix this, must eat n arguments
                        pure (Left (LamPartial (insts <> ops) (capture <> extraApplied) (drop (length extraApplied) args) body ty))
                      LT -> do
                        -- another overapplied case, we must recurse (first: separate out this function)
                        undefined

          t → do
            failWith ("Applications applied to non lambda term: " <> show t)

takesOne ∷ VStack.T → VStack.T → Bool
takesOne post pre = post == VStack.drop (1 ∷ Int) pre

addsOne ∷ VStack.T → VStack.T → Bool
addsOne post pre = VStack.drop 1 post == pre

changesTop ∷ VStack.T → VStack.T → Bool
changesTop post pre = VStack.drop 1 post == pre

varCase ∷
  ( HasState "stack" VStack.T m,
    HasThrow "compilationError" CompilationError m,
    Show a2
  ) ⇒
  a2 →
  Symbol →
  m Op
varCase term n = stackCheck term addsOne $ do
  stack ← get @"stack"
  case VStack.lookup n stack of
    Nothing → failWith ("variable not in scope: " <> show n)
    Just (VStack.Value i) → undefined
    Just (VStack.Position i) → do
      -- TODO ∷ replace with dip call

      let before = rearrange i
          after = M.PrimEx (M.DIP [unrearrange i])
      genReturn (M.SeqEx [before, M.PrimEx (M.DUP ""), after])

foldApps ∷
  J.AnnTerm primTy primVal →
  [J.AnnTerm primTy primVal] →
  ( (J.Term primTy primVal, Usage.Usage, J.Type primTy primVal),
    [J.AnnTerm primTy primVal]
  )
foldApps ((J.App f arg), _, _) args =
  foldApps f (arg : args)
foldApps inner args =
  (inner, args)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
stackCheck ∷
  ( HasState "stack" VStack.T m,
    HasThrow "compilationError" CompilationError m,
    Show a2
  ) ⇒
  a2 →
  (VStack.T → VStack.T → Bool) →
  m Op →
  m Op
stackCheck term guard func = do
  pre ← get @"stack"
  res ← func
  post ← get @"stack"
  unless (guard post pre) $
    failWith
      ( "compilation violated stack invariant: "
          <> show term
          <> " prior stack "
          <> show pre
          <> " posterior stack "
          <> show post
      )
  pure res

argsFromApps ∷
  J.AnnTerm primTy primVal →
  ( (J.Term primTy primVal, Usage.Usage, J.Type primTy primVal),
    [J.AnnTerm primTy primVal]
  )
argsFromApps xs = go xs []
  where
    go (J.App inner arg, _, _) acc = go inner (arg : acc)
    go inner acc = (inner, reverse acc)

-- TODO ∷ have a function which grabs all names of
--        lambdas recursively for over applied functions
evaluateAndPushArgs ∷
  ( HasThrow "compilationError" CompilationError m,
    HasState "stack" VStack.T m,
    HasWriter "compilationLog" [CompilationLog] m
  ) ⇒
  [Symbol] →
  J.Type PrimTy PrimVal →
  [Term] →
  M.Type →
  m [Op]
evaluateAndPushArgs names t args paramTy = do
  types ← typesFromPi t
  let namedArgs = zip (zip names types) args
  foldrM
    ( \((name, _type'), arg) instrs → do
        -- TODO ∷ assert that _type' and typeV are the same!
        -- TODO: deal with this correctly
        argEval' ← termToInstr arg paramTy
        let Right argEval = argEval'
        (v, typeV) ← pop
        case v of
          VStack.VarE _ _ → failWith "Never happens"
          VStack.Val v →
            modify @"stack" (cons (VStack.varE name (Just v), typeV))
        pure (argEval : instrs)
    )
    []
    namedArgs
-- TODO ∷ find a monadic version of mapAccumR

-- argTypeFromLam ∷
--   ( Foldable t,
--     HasState "stack" Stack m,
--     HasThrow "compilationError" CompilationError m,
--     HasWriter "compilationLog" [CompilationLog] m
--   ) ⇒
--   J.Type PrimTy PrimVal →
--   M.Type →
--   t (a, Term) →
--   m (J.Type PrimTy PrimVal, [M.Type])
-- argTypeFromLam t paramTy =
--   foldrM
--     ( \(sym, arg) (t, args) →
--         case t of
--           J.Pi _usage aType rest → do
--             argEval ← termToInstr arg paramTy
--             aType ← typeToType aType
--             (v, vType) ← pop
--             case v of
--               VarE _ _ → failWith' "Never happens"
--               -- TODO ∷ change logic if we get a constant
--               --        don't extend the env, but push the constant
--               --        to the stack
--               Val (ConstE v) →
--                 undefined
--               Val (FuncResultE) →
--                 undefined
--             modify @"stack" (cons (VarE sym (Just v), aType))
--             pure (rest, aType : args)
--           _ →
--             failWith'
--               ( "compilation argument invariant : "
--                   <> show t
--                   <> " is not of Pi/Lambda type"
--               )
--     )
--     (t, [])
