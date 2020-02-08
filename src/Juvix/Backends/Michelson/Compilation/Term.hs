-- |
-- - Compilation of core terms to Michelson instruction sequences.
module Juvix.Backends.Michelson.Compilation.Term where

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
import qualified Data.Set as Set

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
      -- Note: lambdas don't consume their args BUT builtins DO so we
      --       must copy (for now) before inlining a built-in
      pure (Left (LamPartial [] capture args body ty))
    J.Lam _ _ → do
      failWith ("This term should not exist")
    -- :: (\a -> b) a ~ (a, s) => (b, s)
    -- Call-by-value (evaluate argument first).
    J.App _ _ → do
      -- TODO: figure out how to do stack check here
      let (lam, args) = argsFromApps ann
      case lam of
        (J.LamM captures arguments body, _usage, lamTy) → do
          insts ← evaluateAndPushArgs arguments lamTy args paramTy
          recurseApplication (captures, arguments, body) lamTy args insts paramTy
        ann@(J.Prim prim, _, primTy) → do
          -- Treat the primitive as a function with n arguments
          -- , the body will eventually be inlined (or packed in a lambda).
          let arguments = replicate (arity prim) "_"
          insts ← evaluateAndPushArgs arguments primTy args paramTy
          recurseApplication
             ([], arguments {- the arguments to the primitive -}, ann {- will be inlined -})
             primTy
             args
             insts
             paramTy
        t → do
          failWith ("Applications applied to non lambda term: " <> show t)

-- captures, arguments, body are for the function
-- lamTy is the type of the function
-- args are the arguments to which it is being applied
recurseApplication ∷
  ∀ m a.
  ( HasState "stack" VStack.T m,
    HasThrow "compilationError" CompilationError m,
    HasWriter "compilationLog" [CompilationLog] m
  ) ⇒
  ([Symbol], [Symbol], Term) →
  J.Type PrimTy PrimVal →
  [a] →
  [Op] →
  M.Type →
  m (Either LamPartial M.ExpandedOp)
recurseApplication (captures, lamArguments, body) lamTy args insts paramTy = do
  let argsL = length args
      lamArgsL = length lamArguments
  case compare argsL lamArgsL of
    EQ → do
      -- Exactly applied case, we have the correct number of arguments.
      -- need to assert that we correctly drop them later, this is complicated!
      -- need to pass things to be dropped along with the partially applied function datatype
      eitherFuncOp ← termToInstr body paramTy
      case eitherFuncOp of
        Right f → do
          -- Fully applied case without a lambda: evaluate all the arguments, inline the function, drop the args.
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
        Left (LamPartial ops captures remArgs body ty) → do
          -- Fully applied case with a returned lambda: return the instructions
          -- followed by the lambda, defer evaluation of the body.
          -- Will this type be correct if we compile this to a lambda?
          pure (Left (LamPartial (insts <> ops) captures remArgs body ty))
    LT → do
      -- Underapplied case, we don't yet have enough arguments to inline.
      -- Here we are turning formerly bound arguments into captures.
      let (evaluatedArgs, remainingArgs) = splitAt argsL lamArguments
      lamTy ← dropNArgs lamTy argsL
      pure (Left (LamPartial insts (captures <> evaluatedArgs) remainingArgs body lamTy))
    GT → do
      -- Overapplied case, we must figure out what the body is and recurse.
      let (args, extraApplied) = splitAt lamArgsL args
      eitherFuncOp ← termToInstr body paramTy
      case eitherFuncOp of
        Right _ → failWith "invalid case"
        Left (LamPartial ops parCaptures parArgs parBody parTy) → do
          -- todo: are these captures correct
          -- Will this type be correct if we compile this to a lambda?
          recurseApplication (parCaptures, parArgs, parBody) parTy extraApplied (insts <> ops) paramTy

takesOne ∷ VStack.T → VStack.T → Bool
takesOne post pre = post == VStack.drop 1 pre

addsOne ∷ VStack.T → VStack.T → Bool
addsOne post pre = VStack.drop 1 post == pre

changesTop ∷ VStack.T → VStack.T → Bool
changesTop post pre = VStack.drop 1 post == pre

-- TODO ∷ to account for constant propagation we should change the output type
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
    -- TODO ∷ figure out how to do constant propagation here
    Just (VStack.Value v) →
      let Just t = VStack.lookupType n stack in
      genReturn (M.PrimEx (M.PUSH "" t v))

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

-- this could also be AppM
-- seems like a useful pass
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
        (v, typeV) ← pop
        case v of
          VStack.VarE x v →
            modify @"stack" (cons (VStack.VarE (Set.insert name x) v, typeV))
          VStack.Val v →
            modify @"stack" (cons (VStack.varE name (Just v), typeV))
        case argEval' of
          Right argEval →
            pure (argEval : instrs)
          Left _lamPartial →
            -- TODO ∷ turn this into an op?
            --        Really the output of this should probably be a custom
            --        sum type that accounts for the lambda,
            --        Maybe we should even set the vstack to hold these lambdas
            --        then have a decision function to decide if it's inlined or not
            undefined
    )
    []
    namedArgs
