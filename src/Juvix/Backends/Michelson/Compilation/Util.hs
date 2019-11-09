module Juvix.Backends.Michelson.Compilation.Util where

import Juvix.Library
import Michelson.Untyped

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
