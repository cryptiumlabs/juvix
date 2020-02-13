-- |
-- - Compilation of core terms to Michelson instruction sequences.
module Juvix.Backends.Michelson.Compilation.Term where

import qualified Data.Set as Set
import qualified Juvix.Backends.Michelson.Compilation.Checks as Checks
import qualified Juvix.Backends.Michelson.Compilation.Environment as Env
import qualified Juvix.Backends.Michelson.Compilation.Prim as Prim
import qualified Juvix.Backends.Michelson.Compilation.Type as Type
import qualified Juvix.Backends.Michelson.Compilation.Util as Util
import qualified Juvix.Backends.Michelson.Compilation.VirtualStack as VStack
import qualified Juvix.Backends.Michelson.Parameterisation as Paramaterisation
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import qualified Juvix.Core.Usage as Usage
import Juvix.Library
import qualified Michelson.Untyped as M
import qualified Prelude as Prelude (show)

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
    HasThrow "compilationError" Env.CompilationError m,
    HasWriter "compilationLog" [Env.CompilationLog] m
  ) ⇒
  Env.Term →
  M.Type →
  m (Either Env.LamPartial Env.Op)
termToInstr ann@(term, _, ty) paramTy = Checks.stackGuard ann paramTy $ do
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
    ErasedAnn.Var n → varCase n
    -- Primitive: adds one item to the stack.
    ErasedAnn.Prim prim → pure <$> stackCheck term addsOne (Prim.primToInstr prim ty)
    -- :: \a -> b ~ s => (Lam a b, s)
    ErasedAnn.LamM capture args body → do
      -- ~~
      -- We will either inline or compile to a lambda, and we don't yet know which because
      -- we don't know how many arguments this function is being applied to.
      -- We are ASSUMING that all names are unique, so captures will be in the stack,
      -- and will be kept around & available for lookup when we later inline the body.
      -- ~~
      -- Note: lambdas don't consume their args BUT builtins DO so we
      --       must copy (for now) before inlining a built-in
      pure (Left (Env.LamPartial [] capture args body ty))
    ErasedAnn.Lam _ _ → do
      Util.failWith ("This term should not exist")
    -- :: (\a -> b) a ~ (a, s) => (b, s)
    -- Call-by-value (evaluate argument first).
    ErasedAnn.App _ _ → do
      -- TODO: figure out how to do stack check here
      let (lam, args) = argsFromApps ann
      case lam of
        (ErasedAnn.LamM captures arguments body, _usage, lamTy) → do
          insts ← evaluateAndPushArgs arguments lamTy args paramTy
          recurseApplication (captures, arguments, body) lamTy args insts paramTy
        ann@(ErasedAnn.Prim prim, _, primTy) → do
          -- Treat the primitive as a function with n arguments
          -- , the body will eventually be inlined (or packed in a lambda).
          -- TODO ∷ generate better unique names
          let arguments =
                intern
                  <$> replicate (Paramaterisation.arity prim) "_"
                  <> fmap Prelude.show ([1 ..] ∷ [Integer])
          insts ← evaluateAndPushArgs arguments primTy args paramTy
          recurseApplication
            ([], arguments {- the arguments to the primitive -}, ann {- will be inlined -})
            primTy
            args
            insts
            paramTy
        t → do
          Util.failWith ("Applications applied to non lambda term: " <> show t)

-- captures, arguments, body are for the function
-- lamTy is the type of the function
-- args are the arguments to which it is being applied
recurseApplication ∷
  ∀ m a.
  ( HasState "stack" VStack.T m,
    HasThrow "compilationError" Env.CompilationError m,
    HasWriter "compilationLog" [Env.CompilationLog] m
  ) ⇒
  ([Symbol], [Symbol], Env.Term) →
  Env.Type →
  [a] →
  [Env.Op] →
  M.Type →
  m (Either Env.LamPartial M.ExpandedOp)
recurseApplication (captures, lamArguments, body) lamTy args insts paramTy = do
  let argsL = length args
      lamArgsL = length lamArguments
  case compare argsL lamArgsL of
    EQ → do
      -- Exactly applied case, we have the correct number of arguments.
      -- need to assert that we correctly drop them later, this is complicated!
      -- need to pass things to be dropped along with the partially applied
      -- function datatype
      eitherFuncOp ← termToInstr body paramTy
      case eitherFuncOp of
        Right f → do
          -- Fully applied case without a lambda: evaluate all the arguments,
          -- inline the function, drop the args.
          pure $
            Right
              ( M.SeqEx
                  ( insts
                      <> [ f,
                           M.PrimEx
                             (M.DIP (replicate argsL (M.PrimEx M.DROP)))
                         ]
                  )
              )
        Left (Env.LamPartial ops captures remArgs body ty) → do
          -- Fully applied case with a returned lambda: return the instructions
          -- followed by the lambda, defer evaluation of the body.
          -- Will this type be correct if we compile this to a lambda?
          pure (Left (Env.LamPartial (insts <> ops) captures remArgs body ty))
    LT → do
      -- Under-applied case, we don't yet have enough arguments to inline.
      -- Here we are turning formerly bound arguments into captures.
      let (evaluatedArgs, remainingArgs) = splitAt argsL lamArguments
      lamTy ← Type.dropNArgs lamTy argsL
      pure (Left (Env.LamPartial insts (captures <> evaluatedArgs) remainingArgs body lamTy))
    GT → do
      -- Over-applied case, we must figure out what the body is and recurse.
      let (args, extraApplied) = splitAt lamArgsL args
      eitherFuncOp ← termToInstr body paramTy
      case eitherFuncOp of
        Right _ → Util.failWith "invalid case"
        Left (Env.LamPartial ops parCaptures parArgs parBody parTy) → do
          -- todo: are these captures correct
          -- Will this type be correct if we compile this to a lambda?
          recurseApplication
            (parCaptures, parArgs, parBody)
            parTy
            extraApplied
            (insts <> ops)
            paramTy

takesOne ∷ VStack.T → VStack.T → Bool
takesOne post pre = post == VStack.drop 1 pre

addsOne ∷ VStack.T → VStack.T → Bool
addsOne post pre = VStack.drop 1 post == pre

changesTop ∷ VStack.T → VStack.T → Bool
changesTop post pre = VStack.drop 1 post == pre

varCase ∷
  ( HasState "stack" VStack.T m,
    HasThrow "compilationError" Env.CompilationError m
  ) ⇒
  Symbol →
  m (Either Env.LamPartial Env.Op)
varCase n = do
  stack ← get @"stack"
  case VStack.lookup n stack of
    Nothing → Util.failWith ("variable not in scope: " <> show n)
    -- TODO ∷ figure out how to do constant propagation here
    Just (VStack.Value (VStack.Val' v)) →
      let Just t = VStack.lookupType n stack
       in Right <$> Util.genReturn (M.PrimEx (M.PUSH "" t v))
    Just (VStack.Value (VStack.Lam' l)) →
      pure (Left l)
    Just (VStack.Position _usage i) → do
      -- TODO ∷ replace with dip call
      let before = Util.rearrange i
          after = M.PrimEx (M.DIP [Util.unrearrange i])
      Right <$> Util.genReturn (M.SeqEx [before, M.PrimEx (M.DUP ""), after])

foldApps ∷
  Env.Term →
  [Env.Term] →
  ( Env.Term,
    [Env.Term]
  )
foldApps ((ErasedAnn.App f arg), _, _) args =
  foldApps f (arg : args)
foldApps inner args =
  (inner, args)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
stackCheck ∷
  ( HasState "stack" VStack.T m,
    HasThrow "compilationError" Env.CompilationError m,
    Show a2
  ) ⇒
  a2 →
  (VStack.T → VStack.T → Bool) →
  m Env.Op →
  m Env.Op
stackCheck term guard func = do
  pre ← get @"stack"
  res ← func
  post ← get @"stack"
  unless (guard post pre) $
    Util.failWith
      ( "compilation violated stack invariant: "
          <> show term
          <> " prior stack "
          <> show pre
          <> " posterior stack "
          <> show post
      )
  pure res

-- this could also be AppM
-- seems like a useful pass
argsFromApps ∷
  ErasedAnn.AnnTerm primTy primVal →
  ( (ErasedAnn.Term primTy primVal, Usage.Usage, ErasedAnn.Type primTy primVal),
    [ErasedAnn.AnnTerm primTy primVal]
  )
argsFromApps xs = go xs []
  where
    go (ErasedAnn.App inner arg, _, _) acc = go inner (arg : acc)
    go inner acc = (inner, reverse acc)

evaluateAndPushArgs ∷
  ( HasThrow "compilationError" Env.CompilationError m,
    HasState "stack" VStack.T m,
    HasWriter "compilationLog" [Env.CompilationLog] m
  ) ⇒
  [Symbol] →
  Env.Type →
  [Env.Term] →
  M.Type →
  m [Env.Op]
evaluateAndPushArgs names t args paramTy = do
  types ← Type.typesFromPi t
  let namedArgs = zip (zip names types) args
  foldrM
    ( \((name, type'), arg) instrs → do
        -- TODO ∷ assert that _type' and typeV are the same!
        -- TODO: deal with this correctly
        argEval' ← termToInstr arg paramTy
        case argEval' of
          Right argEval → do
            (v, typeV) ← Util.pop
            case v of
              VStack.VarE x u v →
                modify @"stack" (VStack.cons (VStack.VarE (Set.insert name x) u v, typeV))
              VStack.Val v →
                modify @"stack" (VStack.cons (VStack.varE name (Just v), typeV))
            pure (argEval : instrs)
          -- In this case, Lambda adds nothing to the stack
          -- This could have been via an application or a lambda
          -- Application   ⟹ args are on the vstack, but not the lam
          -- ¬ Application ⟹ Nothing has been added
          Left lamPart → do
            modify
              @"stack"
              (VStack.cons (VStack.varE name (Just (VStack.LamPartialE lamPart)), type'))
            pure instrs
    )
    []
    namedArgs
