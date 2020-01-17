-- |
-- - Compilation of primitive terms to Michelson instruction sequences.
module Juvix.Backends.Michelson.Compilation.Prim where

import Juvix.Backends.Michelson.Compilation.Type
import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Backends.Michelson.Parameterisation
import qualified Juvix.Core.ErasedAnn as J
import Juvix.Library
import qualified Michelson.Untyped as M

primToInstr ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m,
    HasWriter "compilationLog" [CompilationLog] m
  ) ⇒
  PrimVal →
  J.Type PrimTy PrimVal →
  m Op
primToInstr prim ty =
  case prim of
    -- :: \x -> y ~ s => (f, s)
    PrimFst → do
      let J.Pi _ (J.PrimTy (PrimTy pairTy@(M.Type (M.TPair _ _ xT _) _))) _ = ty
          retTy = M.Type (M.TLambda pairTy xT) ""
      modify @"stack" ((:) (FuncResultE, retTy))
      pure (oneArgPrim (M.PrimEx (M.CAR "" "") :| []) retTy)
    -- :: \x -> y ~ s => (f, s)
    PrimSnd → do
      let J.Pi _ (J.PrimTy (PrimTy pairTy@(M.Type (M.TPair _ _ _ yT) _))) _ = ty
          retTy = M.Type (M.TLambda pairTy yT) ""
      modify @"stack" ((:) (FuncResultE, retTy))
      pure (oneArgPrim (M.PrimEx (M.CDR "" "") :| []) retTy)
    -- :: \x y -> a ~ (x, (y, s)) => (a, s)
    PrimPair → do
      let J.Pi _ firstArgTy (J.Pi _ secondArgTy _) = ty
      firstArgTy ← typeToType firstArgTy
      secondArgTy ← typeToType secondArgTy
      -- TODO: Clean this up.
      let mkPair x y = M.Type (M.TPair "" "" x y) ""

          mkLam x y = M.Type (M.TLambda x y) ""

          secondLamTy = mkLam (mkPair firstArgTy secondArgTy) (mkPair firstArgTy secondArgTy) -- ??

          firstLamTy = mkLam firstArgTy (mkLam secondArgTy (mkPair firstArgTy secondArgTy))

      modify @"stack" ((:) (FuncResultE, firstLamTy))
      pure
        ( M.PrimEx
            ( M.PUSH
                ""
                firstLamTy
                ( M.ValueLambda
                    ( M.SeqEx
                        [ M.PrimEx
                            ( M.DIP
                                [ M.PrimEx
                                    ( M.PUSH
                                        ""
                                        secondLamTy
                                        ( M.ValueLambda
                                            ( M.SeqEx [M.PrimEx (M.DUP ""), M.PrimEx M.DROP]
                                                :| []
                                            )
                                        )
                                    )
                                ]
                            ),
                          M.PrimEx (M.APPLY "")
                        ]
                        :| []
                    )
                )
            )
        )
    -- :: a ~ s => (a, s)
    PrimConst const → do
      case const of
        M.ValueNil → do
          let J.PrimTy (PrimTy t@(M.Type (M.TList elemTy) _)) = ty
          modify @"stack" ((:) (FuncResultE, t))
          pure (M.PrimEx (M.NIL "" "" elemTy))
        _ → do
          let J.PrimTy (PrimTy t) = ty
          modify @"stack" ((:) (FuncResultE, t))
          pure (M.PrimEx (M.PUSH "" t const))

oneArgPrim ∷ NonEmpty Op → M.Type → Op
oneArgPrim ops retTy = M.PrimEx (M.PUSH "" retTy (M.ValueLambda ops))
