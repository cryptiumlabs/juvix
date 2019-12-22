module Juvix.Backends.Michelson.Compilation.Term where

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
  let failWith ∷ Text → m Op
      failWith = throw @"compilationError" . InternalFault

      stackCheck ∷ (Stack → Stack → Bool) → m Op → m Op
      stackCheck guard func = do
        pre ← get @"stack"
        res ← func
        post ← get @"stack"
        if guard post pre
          then pure res
          else failWith ("compilation violated stack invariant: " <> show term <> " prior stack " <> show pre <> " posterior stack " <> show post)

      primToInstr ∷ PrimVal → m Op
      primToInstr prim =
        case prim of
          -- :: \x -> y ~ s => (f, s)
          PrimFst → stackCheck addsOne $ do
            let J.Pi _ (J.PrimTy (PrimTy pairTy@(M.Type (M.TPair _ _ xT _) _))) _ = ty
                retTy = M.Type (M.TLambda pairTy xT) ""
            modify @"stack" ((:) (FuncResultE, retTy))
            pure (oneArgPrim (M.PrimEx (M.CAR "" "") :| []) retTy)
          -- :: \x -> y ~ s => (f, s)
          PrimSnd → stackCheck addsOne $ do
            let J.Pi _ (J.PrimTy (PrimTy pairTy@(M.Type (M.TPair _ _ _ yT) _))) _ = ty
                retTy = M.Type (M.TLambda pairTy yT) ""
            modify @"stack" ((:) (FuncResultE, retTy))
            pure (oneArgPrim (M.PrimEx (M.CDR "" "") :| []) retTy)
          -- :: \x y -> a ~ (x, (y, s)) => (a, s)
          PrimPair → stackCheck addsOne $ do
            let J.Pi _ firstArgTy (J.Pi _ secondArgTy _) = ty
            firstArgTy ← typeToType firstArgTy
            secondArgTy ← typeToType secondArgTy
            -- TODO: Clean this up.
            let mkPair x y = M.Type (M.TPair "" "" x y) ""

                mkUnit = M.Type (M.TUnit) ""

                mkLam x y = M.Type (M.TLambda x y) ""

                secondLamTy = mkLam (mkPair secondArgTy (mkPair firstArgTy mkUnit)) (mkPair firstArgTy secondArgTy)

                firstLamTy = mkLam (mkPair firstArgTy mkUnit) (mkPair (mkPair firstArgTy mkUnit) secondLamTy)

                retTy = mkPair mkUnit firstLamTy

            modify @"stack" ((:) (FuncResultE, retTy))
            pure
              ( M.PrimEx
                  ( M.PUSH
                      ""
                      retTy
                      ( M.ValuePair
                          M.ValueUnit
                          ( M.ValueLambda
                              ( M.SeqEx
                                  [ M.PrimEx
                                      ( M.DIP
                                          [ M.PrimEx
                                              ( M.PUSH
                                                  ""
                                                  secondLamTy
                                                  ( M.ValueLambda
                                                      ( M.SeqEx [M.PrimEx (M.DUP ""), M.PrimEx (M.CAR "" ""), M.PrimEx (M.DIP [M.PrimEx (M.CDR "" ""), M.PrimEx (M.CAR "" "")]), M.PrimEx M.SWAP, M.PrimEx (M.PAIR "" "" "" "")]
                                                          :| []
                                                      )
                                                  )
                                              )
                                          ]
                                      ),
                                    M.PrimEx (M.PAIR "" "" "" "")
                                  ]
                                  :| []
                              )
                          )
                      )
                  )
              )
          -- :: a ~ s => (a, s)
          PrimConst const → stackCheck addsOne $ do
            case const of
              M.ValueNil → do
                let J.PrimTy (PrimTy t@(M.Type (M.TList elemTy) _)) = ty
                modify @"stack" ((:) (FuncResultE, t))
                pure (M.PrimEx (M.NIL "" "" elemTy))
              _ → do
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
        let J.Pi _ argTy retTy = ty
        -- TODO: How to deal with packed lambdas here? For now, we don't.
        -- If the argument is a lambda, we don't know what its closure type is.
        argTy ← typeToType argTy
        stack ← get @"stack"
        let free = J.free (J.eraseTerm term)
            freeWithTypes = map (\v → let Just t = lookupType v stack in (v, t)) free
        vars ← mapM (\f → termToInstr (J.Var f, undefined, undefined) paramTy) free
        packOp ← packClosure free
        put @"stack" []
        unpackOp ← unpackClosure ((arg, argTy) : freeWithTypes)
        inner ← termToInstr body paramTy
        dropOp ← dropClosure ((arg, argTy) : freeWithTypes)
        post ← get @"stack"
        let ((_, retTy) : _) = post
        let (lTy, rTy) = lamRetTy freeWithTypes argTy retTy
        put @"stack" ((FuncResultE, rTy) : stack)
        pure
          ( M.SeqEx
              [ M.PrimEx (M.PUSH "" lTy (M.ValueLambda (unpackOp :| [inner, dropOp]))),
                M.PrimEx (M.PUSH "" (M.Type M.TUnit "") M.ValueUnit),
                M.SeqEx vars,
                packOp,
                M.PrimEx (M.PAIR "" "" "" ""), -- pair the env & lambda
                M.PrimEx (M.APPLY "")
              ]
          )
    -- :: (\a -> b) a ~ (a, s) => (b, s)
    -- Call-by-value (evaluate argument first).
    J.App func arg →
      stackCheck addsOne $ do
        func ← termToInstr func paramTy -- :: (vars, Lam (a, vars) b)
        arg ← termToInstr arg paramTy -- :: a
        stack ← get @"stack"
        modify @"stack" (\(_ : (_, M.Type (M.TPair _ _ _ (M.Type (M.TLambda _ retTy) _)) _) : xs) → (FuncResultE, retTy) : xs)
        -- Then pair up (a) with (vars) and execute the function.
        pure
          ( M.SeqEx
              [ func,
                arg,
                M.PrimEx (M.DIP [M.SeqEx [M.PrimEx (M.DUP ""), M.PrimEx (M.CAR "" ""), M.PrimEx (M.DIP [M.PrimEx (M.CDR "" "")])]]),
                -- Unpacked, now arg : env : lambda
                M.PrimEx (M.PAIR "" "" "" ""),
                -- Paired, now (arg, env) : lambda
                M.PrimEx (M.EXEC "")
                -- Then execute the function.
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
genSwitch (M.TOption _) = pure (\x y → M.PrimEx (M.IF_NONE [x] [y]))
genSwitch (M.TList _) = pure (\x y → M.PrimEx (M.IF_CONS [x] [y]))
genSwitch ty = throw @"compilationError" (NotYetImplemented ("genSwitch: " <> show ty))
