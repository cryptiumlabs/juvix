-- |
-- - Compilation of core terms to Michelson instruction sequences.
module Juvix.Backends.Michelson.Compilation.Lambda where

import Data.Maybe (fromJust) -- bad remove!
import Juvix.Backends.Michelson.Compilation.Term -- TODO fixme
import Juvix.Backends.Michelson.Compilation.Type
import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Backends.Michelson.Compilation.Util
import qualified Juvix.Backends.Michelson.Compilation.VirtualStack as VStack
import Juvix.Backends.Michelson.Parameterisation
import qualified Juvix.Core.ErasedAnn as J
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
      modify @"stack" (cons (VStack.varE arg Nothing, paramTy))
      instr' ← termToInstrOuter body paramTy
      let instr = M.SeqEx [instr', M.PrimEx (M.DIP [M.PrimEx M.DROP])]
      modify @"stack" (\xs → cons (car xs) (cdr (cdr xs)))
      tell @"compilationLog" [TermToInstr body instr]
      pure instr
    _ → throw @"compilationError" (NotYetImplemented "must be a lambda function")

termToInstrOuter ∷
  ∀ m.
  ( HasState "stack" VStack.T m,
    HasThrow "compilationError" CompilationError m,
    HasWriter "compilationLog" [CompilationLog] m
  ) ⇒
  Term →
  M.Type →
  m Op
termToInstrOuter term ty = do
  maybeOp ← termToInstr term ty
  case maybeOp of
    Right op → pure op
    Left (LamPartial _ops _captures _args _body _) → do
      -- TODO: Actually compile the lambda to a closure.
      -- We should never need to do this elsewhere.
      -- ergo, if we do not return a lambda, we should never
      -- compile to a lambda, except for the michelson built-ins
      -- which take lambdas
      -- todo: formalise this a bit
      -- todo: deal with michelson builtins which take lambdas
      -- they will have to use this function or something
      undefined

funcToLambda ∷
  ∀ m.
  ( HasState "stack" VStack.T m,
    HasThrow "compilationError" CompilationError m,
    HasWriter "compilationLog" [CompilationLog] m
  ) ⇒
  LamPartial →
  M.Type →
  m M.ExpandedOp
funcToLambda (LamPartial ops captures args body lamTy) paramTy = do
  -- ~~
  -- Here we are dealing with a (possibly previously partially applied)
  -- function which will not be fully applied, ergo we must compile it
  -- to a Michelson lambda, which we do by copying the captures to the
  -- top of the stack, packing them all in a tuple, compiling the body
  -- of the lambda to a lambda function, then using the `APPLY` instruction
  -- to partially apply the function & returning a lambda encapsulated
  -- with this environment. Note that previous partial application of a
  -- function to some arguments will result in those arguments being captured.
  -- ~~
  -- TODO ∷ what if all captures or arguments are constants? This will end horribly!
  -- ~~
  -- Step 1: Copy the captures to the top of the stack.
  captureInsts ←
    traverse
      ( \x → do
          currentStack ← get @"stack"
          case VStack.lookup x currentStack of
            Nothing →
              failWith
                ( "free variable in lambda"
                    <> " doesn't exist"
                )
            Just (VStack.Value v) → do
              let Just type' = VStack.lookupType x currentStack
                  val = case v of
                    VStack.Val' v → VStack.ConstE v
                    VStack.Lam' l → VStack.LamPartialE l
              modify @"stack"
                ( cons
                    ( VStack.varE x (Just val),
                      type'
                    )
                )
              pure (M.SeqEx [])
            Just (VStack.Position p) → do
              let (Just type') = VStack.lookupType x currentStack
              let inst = dupToFront (fromIntegral p)
              modify @"stack" (cons (VStack.varE x Nothing, type'))
              pure inst
      )
      captures
  -- Step 2: Figure out what the stack will be in the body of the function.
  current@(VStack.T currentStack _) ← get @"stack"
  let realValuesGen xs =
        length
          $ filter (VStack.inT . fst)
          $ take (length xs) currentStack

      realValues = realValuesGen captures

      numVarsInClosure = (length (captures <> args))

  extraArgsWithTypes ← zip args . drop (length args) <$> typesFromPi lamTy
  let createExtraArgs =
        (\(extra, extraType) → (VStack.varE extra Nothing, extraType))
          <$> extraArgsWithTypes
  modify @"stack" (VStack.insertAt (length captures) createExtraArgs)
  modify @"stack" (VStack.take numVarsInClosure)
  -- Step 3: Compile the body of the lambda.
  body ← termToInstrOuter body paramTy
  put @"stack" current
  -- Step 4: Pack up the captures.
  packInstrs ← genReturn (pairN (realValues - 1))
  -- Step 5: Determine the type of the lambda.
  let capturesTypes =
        (\x → (x, fromJust (VStack.lookupType x current)))
          <$> VStack.symbolsInT captures current
  lTy ←
    lamType capturesTypes extraArgsWithTypes
      <$> returnTypeFromPi lamTy
  -- Step 6: Return the sequence of Michelson instructions, ending in `APPLY`.
  let dipGen x =
        case length (VStack.symbolsInT x current) of
          0 → M.SeqEx []
          i → M.PrimEx (M.DIP [unpackTupleN (pred i)])

      dipArgs = dipGen args

      dipCurr = dipGen captures

  pure $
    M.SeqEx
      [ M.SeqEx ops,
        M.PrimEx
          ( M.PUSH
              ""
              lTy
              ( M.ValueLambda
                  ( M.SeqEx
                      [ unpackTuple,
                        dipArgs,
                        dipCurr
                      ]
                      :| [body]
                  )
              )
          ),
        M.SeqEx captureInsts,
        packInstrs,
        M.PrimEx (M.APPLY "")
      ]
