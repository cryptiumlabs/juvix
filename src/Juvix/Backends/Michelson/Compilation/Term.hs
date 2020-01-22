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
import qualified Juvix.Backends.Michelson.Compilation.VituralStack as VStack
import Juvix.Backends.Michelson.Parameterisation
import qualified Juvix.Core.Erased.Util as J
import qualified Juvix.Core.ErasedAnn as J
import qualified Juvix.Core.Usage as Usage
import Juvix.Library
import qualified Michelson.Untyped as M

termToMichelson ∷
  ∀ m.
  ( HasState "stack" VStack.T m,
    HasThrow "compilationError" CompilationError m,
    HasWriter "compilationLog" [CompilationLog] m
  ) ⇒
  Term →
  M.Type →
  m Op
termToMichelson term paramTy = do
  case term of
    (J.Lam arg body, _, _) → do
      modify @"stack" (cons (VStack.VarE arg Nothing, paramTy))
      instr' ← termToInstr body paramTy
      let instr = M.SeqEx [instr', M.PrimEx (M.DIP [M.PrimEx M.DROP])]
      modify @"stack" (\xs → cons (car xs) (cdr (cdr xs)))
      tell @"compilationLog" [TermToInstr body instr]
      pure instr
    _ → throw @"compilationError" (NotYetImplemented "must be a lambda function")

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
  m Op
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
    J.Var n → varCase term n
    -- Primitive: adds one item to the stack.
    J.Prim prim → stackCheck term addsOne (primToInstr prim ty)
    -- :: \a -> b ~ s => (Lam a b, s)
    J.Lam arg body → undefined
    -- stackCheck term addsOne $ do
    --   let J.Pi _ argTy _retTy = ty
    --   argTy ← typeToType argTy
    --   stack ← get @"stack"
    --   let free = J.free (J.eraseTerm term)
    --       freeWithTypes =
    --         map (\v → let Just t = VStack.lookupType v stack in (v, t)) free
    --   -- TODO: second is a lambda, but it doesn't matter,
    --   -- this just needs to be positionally accurate for var lookup.
    --   modify @"stack"
    --     ( VStack.append $
    --         VStack.fromList
    --           [ (VStack.Val VStack.FuncResultE, M.Type M.TUnit ""),
    --             (VStack.Val VStack.FuncResultE, M.Type M.TUnit "")
    --           ]
    --     )
    --   vars ← traverse (\f → stackGuard f paramTy (varCase term f)) free
    --   packOp ← packClosure free
    --   put @"stack" (VStack.fromList [])
    --   let argUnpack = M.SeqEx [M.PrimEx (M.DUP ""), M.PrimEx (M.CDR "" "")]
    --   unpackOp ← unpackClosure freeWithTypes
    --   modify @"stack" (cons (VStack.VarE arg Nothing, argTy))
    --   inner ← termToInstr body paramTy
    --   dropOp ← dropClosure ((arg, argTy) : freeWithTypes)
    --   post ← get @"stack"
    --   let (VStack.T ((_, retTy) : _) _) = post
    --   let (lTy, rTy) = lamRetTy freeWithTypes argTy retTy
    --   put @"stack" (cons (VStack.Val VStack.FuncResultE, rTy) stack)
    --   pure
    --     (M.SeqEx
    --         [M.PrimEx
    --             (M.PUSH
    --                 ""
    --                 lTy
    --                 (M.ValueLambda
    --                     (argUnpack
    --                         :| [M.PrimEx
    --                                (M.DIP [M.PrimEx (M.CAR "" ""), unpackOp]),
    --                              inner,
    --                              dropOp
    --                            ]))),
    --           M.PrimEx (M.PUSH "" (M.Type M.TUnit "") M.ValueUnit),
    --           -- Evaluate everything in the closure.
    --           M.SeqEx vars,
    --           -- Pack the closure.
    --           packOp,
    --           -- Partially apply the function.
    --           M.PrimEx (M.APPLY "")
    --         ]
    --     )
    -- TODO ∷ remove this special case
    -- Special-case full application of primitive functions.
    J.App (J.App (J.Prim prim, _, _) a, _, _) b | arity prim == 2 →
      stackCheck term addsOne $ do
        args ← mapM (flip termToInstr paramTy) [b, a]
        -- TODO
        func ← genReturn (M.PrimEx (M.PAIR "" "" "" ""))
        pure (M.SeqEx (args <> [func]))
    -- :: (\a -> b) a ~ (a, s) => (b, s)
    -- Call-by-value (evaluate argument first).
    J.App _ _ →
      stackCheck term addsOne $ do
        pre ← get @"stack"
        let (lam, args) = argsFromApps ann
        case lam of
          (J.LamM capture arguments body, _usage, lamTy) → do
            let argsL = length args
                lamArgsL = length arguments
            if
              | argsL == lamArgsL → do
                insts ← evaluateAndPushArgs arguments lamTy args paramTy
                f ← termToInstr body paramTy
                pure
                  ( M.SeqEx
                      ( insts
                          <> [ f,
                               M.PrimEx
                                 (M.DIP (replicate argsL (M.PrimEx M.DROP)))
                             ]
                      )
                  )
              | argsL < lamArgsL → do
                let (lams, extraArgs) = splitAt argsL arguments
                    inEnvironment = lams <> capture
                -- we could evaluate the captures or args first
                captureInsts ←
                  traverse
                    ( \x → do
                        currentStack ← get @"stack"
                        case VStack.lookup x currentStack of
                          Nothing →
                            failWith'
                              ( "free variable in lambda"
                                  <> " doesn't exist"
                              )
                          Just (VStack.Value v) → do
                            let (Just type') = VStack.lookupType x currentStack
                            modify @"stack"
                              ( cons
                                  ( VStack.VarE x (Just (VStack.ConstE v)),
                                    type'
                                  )
                              )
                            pure (M.SeqEx [])
                          Just (VStack.Position p) → do
                            let (Just type') = VStack.lookupType x currentStack
                            let inst = dupToFront (fromIntegral p)
                            modify @"stack" (cons (VStack.VarE x Nothing, type'))
                            pure inst
                    )
                    capture
                insts ← evaluateAndPushArgs arguments lamTy args paramTy
                current@(VStack.T currentStack _) ← get @"stack"
                let realValuesGen xs =
                      length
                        $ filter (VStack.inT . fst)
                        $ take (length xs) currentStack

                    realValues = realValuesGen inEnvironment

                    numVarsInClosure = (length (inEnvironment <> extraArgs))

                -- TODO ∷ WHAΤ about remaining args
                --       do we need to compile to multiple lambas
                --       how to share logic with actual lam case
                extraArgsWithTypes ← zip extraArgs . drop argsL <$> typesFromPi lamTy
                let createExtraArgs =
                      (\ (extra, extraType) → (VStack.VarE extra Nothing, extraType))
                      <$> extraArgsWithTypes
                modify @"stack" (VStack.insertAt (length inEnvironment) createExtraArgs)
                modify @"stack" (VStack.take numVarsInClosure)
                body ← termToInstr body paramTy
                --
                put @"stack" current
                packInstrs ← genReturn (pairN (realValues - 1))
                -- TODO ∷ must be there due to captureInsts effect and other inserts
                --        however, we should write this better.

                let inEnvironmentTypes =
                      (\x → (x, fromJust (VStack.lookupType x current)))
                        <$> VStack.symbolsInT inEnvironment current
                lTy ← lamType inEnvironmentTypes extraArgsWithTypes
                      <$> returnTypeFromPi lamTy
                -- TODO ∷ what if all captures or arguments are constants? This will end horribly!
                pure $
                  M.SeqEx
                    [ M.PrimEx
                        ( M.PUSH
                            ""
                            lTy
                            ( M.ValueLambda
                                ( M.SeqEx
                                    [ unpackTuple,
                                      M.PrimEx
                                        ( M.DIP
                                            [ unpackTupleN
                                                ( length
                                                    (VStack.symbolsInT extraArgs current)
                                                    - 1
                                                )
                                            ]
                                        ),
                                      unpackTupleN
                                        (length
                                         (VStack.symbolsInT inEnvironment current)
                                         - 1)
                                    ]
                                    :| [body]
                                )
                            )
                        ),
                      M.SeqEx captureInsts,
                      M.SeqEx insts,
                      packInstrs,
                      M.PrimEx (M.APPLY "")
                    ]
              | otherwise → do
                -- argsL > lamArgsL
                -- TODO ∷ rather hard to figure out, need to recursively go
                -- through the body to figure out names... etc etc.
                let (args, extraApplied) = splitAt lamArgsL args
                undefined
          t → do
            failWith ("Applications applied to non lambda term: " <> show t)
    -- TODO ∷ remove
    J.App func arg →
      stackCheck term addsOne $ do
        func ← termToInstr func paramTy -- :: Lam a b
        arg ← termToInstr arg paramTy -- :: a
        modify @"stack"
          ( \s@(VStack.T (_ : (_, (M.Type (M.TLambda _ retTy) _)) : _) _) →
              cons (VStack.Val VStack.FuncResultE, retTy) (cdr (cdr s))
          )
        pure
          ( M.SeqEx
              [ func, -- Evaluate the function.
                arg, -- Evaluate the argument.
                M.PrimEx (M.EXEC "") -- Execute the function.
              ]
          )

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
    failWith'
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

typesFromPi ∷
  HasThrow "compilationError" CompilationError f ⇒
  J.Type PrimTy PrimVal →
  f [M.Type]
typesFromPi (J.Pi _usage aType rest) = (:) <$> typeToType aType <*> typesFromPi rest
typesFromPi _ = pure []

returnTypeFromPi (J.Pi _usage _ rest) = returnTypeFromPi rest
returnTypeFromPi x = typeToType x

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
        argEval ← termToInstr arg paramTy
        (v, typeV) ← pop
        case v of
          VStack.VarE _ _ → failWith' "Never happens"
          VStack.Val v →
            modify @"stack" (cons (VStack.VarE name (Just v), typeV))
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
