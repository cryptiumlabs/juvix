module Juvix.Backends.Michelson.Compilation.Util where

import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Library hiding (Type)
import Michelson.Untyped

pack ∷
  ∀ m.
  (HasThrow "compilationError" CompilationError m) ⇒
  Type →
  m ExpandedInstr
pack (Type TUnit _) = pure (PUSH "" (Type TUnit "") ValueUnit)
pack _ = throw @"compilationError" NotYetImplemented

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
          modify @"stack" ((<>) [(VarE fst, fT), (VarE snd, sT)] . drop 1)
          pure (SeqEx [PrimEx (DUP ""), PrimEx (CDR "" ""), PrimEx SWAP, PrimEx (CAR "" "")])
        [Just fst, Nothing] → do
          modify @"stack" ((:) (VarE fst, fT) . drop 1)
          pure (PrimEx (CAR "" ""))
        [Nothing, Just snd] → do
          modify @"stack" ((:) (VarE snd, sT) . drop 1)
          pure (PrimEx (CDR "" ""))
        [Nothing, Nothing] →
          genReturn (PrimEx DROP)
    _ → throw @"compilationError" NotYetImplemented

unpackDrop ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m
  ) ⇒
  [Maybe Symbol] →
  m ExpandedOp
unpackDrop binds = genReturn (foldDrop (fromIntegral (length (filter isJust binds))))

position ∷ Symbol → Stack → Maybe Natural
position n [] = Nothing
position n (x : xs) = if fst x == VarE n then Just 0 else (+) 1 |<< position n xs

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
unrearrange n = SeqEx [PrimEx (DIP [unrearrange (n - 1)]), PrimEx SWAP]

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
    WithSrcEx _ _ → undefined

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
      pure (\s → foldl (flip ($)) s (reverse fs))
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
    _ → throw @"compilationError" NotYetImplemented
