module Juvix.Backends.Michelson.Compilation.Util where

import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Library hiding (Type)
import Michelson.TypeCheck
import qualified Michelson.Typed as MT
import Michelson.Untyped

stackToStack ∷ Stack → SomeHST
stackToStack [] = SomeHST SNil
stackToStack ((_, ty) : xs) =
  case stackToStack xs of
    SomeHST tail →
      MT.withSomeSingT (MT.fromUType ty) $ \sty →
        SomeHST (sty -:& tail)

pack ∷
  ∀ m.
  (HasThrow "compilationError" CompilationError m) ⇒
  Type →
  m ExpandedInstr
pack (Type TUnit _) = pure (PUSH "" (Type TUnit "") ValueUnit)
pack ty = throw @"compilationError" (NotYetImplemented ("pack: " <> show ty))

unpack ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m
  ) ⇒
  Type →
  [Maybe Symbol] →
  m ExpandedOp
unpack (Type ty _) binds =
  case ty of
    Tbool → do
      modify @"stack" (drop 1)
      return (SeqEx [])
    TPair _ _ fT sT →
      case binds of
        [Just fst, Just snd] → do
          modify @"stack" (appendDrop [(VarE fst, fT), (VarE snd, sT)])
          pure (SeqEx [PrimEx (DUP ""), PrimEx (CDR "" ""), PrimEx SWAP, PrimEx (CAR "" "")])
        [Just fst, Nothing] → do
          modify @"stack" (appendDrop [(VarE fst, fT)])
          pure (PrimEx (CAR "" ""))
        [Nothing, Just snd] → do
          modify @"stack" (appendDrop [(VarE snd, sT)])
          pure (PrimEx (CDR "" ""))
        [Nothing, Nothing] →
          genReturn (PrimEx DROP)
        _ → throw @"compilationError" (InternalFault "binds do not match type")
    _ → throw @"compilationError" (NotYetImplemented ("unpack: " <> show ty))
unpack _ _ = throw @"compilationError" (InternalFault "invalid unpack type")

unpackDrop ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m
  ) ⇒
  [Maybe Symbol] →
  m ExpandedOp
unpackDrop binds = genReturn (foldDrop (fromIntegral (length (filter isJust binds))))

appendDrop ∷ Stack → Stack → Stack
appendDrop prefix = (<>) prefix . drop 1

position ∷ Symbol → Stack → Maybe Natural
position _ [] = Nothing
position n (x : xs) = if fst x == VarE n then Just 0 else succ |<< position n xs

dropFirst ∷ Symbol → Stack → Stack
dropFirst n (x : xs) = if fst x == VarE n then xs else x : dropFirst n xs
dropFirst _ [] = []

rearrange ∷ Natural → ExpandedOp
rearrange 0 = SeqEx []
rearrange 1 = PrimEx SWAP
rearrange n = SeqEx [PrimEx (DIP [rearrange (n - 1)]), PrimEx SWAP]

unrearrange ∷ Natural → ExpandedOp
unrearrange 0 = SeqEx []
unrearrange 1 = PrimEx SWAP
unrearrange n = SeqEx [PrimEx SWAP, PrimEx (DIP [unrearrange (n - 1)])]

foldDrop ∷ Natural → ExpandedOp
foldDrop 0 = SeqEx []
foldDrop n = PrimEx (DIP [SeqEx (replicate (fromIntegral n) (PrimEx DROP))])

leftSeq ∷ ExpandedOp → ExpandedOp
leftSeq = foldSeq . unrollSeq

foldSeq ∷ [ExpandedOp] → ExpandedOp
foldSeq [] = SeqEx []
foldSeq (x : xs) = SeqEx [x, foldSeq xs]

unrollSeq ∷ ExpandedOp → [ExpandedOp]
unrollSeq op =
  case op of
    PrimEx instr → [PrimEx instr]
    SeqEx xs → concatMap unrollSeq xs
    WithSrcEx _ op → unrollSeq op

genReturn ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m
  ) ⇒
  ExpandedOp →
  m ExpandedOp
genReturn instr = do
  modify @"stack" =<< genFunc instr
  pure instr

genFunc ∷
  ∀ m.
  (HasThrow "compilationError" CompilationError m) ⇒
  ExpandedOp →
  m (Stack → Stack)
genFunc instr =
  case instr of
    SeqEx is → do
      fs ← mapM genFunc is
      pure (\s → foldl (flip ($)) s fs)
    PrimEx p →
      case p of
        DROP → pure (drop 1)
        DUP _ → pure (\(x : xs) → x : x : xs)
        SWAP → pure (\(x : y : xs) → y : x : xs)
        CAR _ _ → pure (\((_, Type (TPair _ _ x _) _) : xs) → (FuncResultE, x) : xs)
        CDR _ _ → pure (\((_, Type (TPair _ _ _ y) _) : xs) → (FuncResultE, y) : xs)
        PAIR _ _ _ _ → pure (\((_, xT) : (_, yT) : xs) → (FuncResultE, Type (TPair "" "" xT yT) "") : xs)
        DIP ops → do
          f ← genFunc (SeqEx ops)
          return (\(x : xs) → x : f xs)
        AMOUNT _ → pure ((:) (FuncResultE, Type (Tc CMutez) ""))
        NIL _ _ _ → pure ((:) (FuncResultE, Type (TList (Type TOperation "")) ""))
        _ → throw @"compilationError" (NotYetImplemented ("genFunc: " <> show p))
    _ → throw @"compilationError" (NotYetImplemented ("genFunc: " <> show instr))

oneArgPrim ∷ [ExpandedOp] → Type → ExpandedOp
oneArgPrim ops retTy = PrimEx (PUSH "" retTy (ValuePair ValueUnit (ValueLambda (PrimEx (CAR "" "") :| ops))))
