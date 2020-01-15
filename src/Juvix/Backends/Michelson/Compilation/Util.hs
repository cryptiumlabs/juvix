-- |
-- Utility functions used by the Michelson backend.
module Juvix.Backends.Michelson.Compilation.Util where

import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Library hiding (Type)
import Michelson.TypeCheck
import qualified Michelson.Typed as MT
import Michelson.Untyped

-- TODO ∷ find better name
failWith' ∷ (HasThrow "compilationError" CompilationError m) ⇒ Text → m a
failWith' = throw @"compilationError" . InternalFault

failWith ∷ ∀ m. (HasThrow "compilationError" CompilationError m) ⇒ Text → m ExpandedOp
failWith = throw @"compilationError" . InternalFault

stackToStack ∷ Stack → SomeHST
stackToStack [] = SomeHST SNil
stackToStack ((_, ty) : xs) =
  case stackToStack xs of
    SomeHST tail →
      MT.withSomeSingT (MT.fromUType ty) $ \sty →
        SomeHST (sty -:& tail)

appendDrop ∷ Stack → Stack → Stack
appendDrop prefix = (<>) prefix . drop 1

lookupType ∷ Symbol → Stack → Maybe Type
lookupType _ [] = Nothing
lookupType n (x : xs) = if fst x == VarE n then pure (snd x) else lookupType n xs

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

packClosure ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m
  ) ⇒
  [Symbol] →
  m ExpandedOp
packClosure vars = do
  let count = length vars
  genReturn
    ( SeqEx
        ( replicate count (PrimEx (PAIR "" "" "" ""))
        )
    )

unpackClosure ∷
  ∀ m.
  (HasState "stack" Stack m) ⇒
  [(Symbol, Type)] →
  m ExpandedOp
unpackClosure [] = pure (PrimEx DROP)
unpackClosure env = do
  let count = length env
  modify @"stack" ((<>) (map (\(s, t) → (VarE s, t)) env))
  -- dup (count - 1) times,
  pure
    ( SeqEx
        ( replicate (count - 1) (PrimEx (DUP ""))
            <> [carN count]
        )
    )

dropClosure ∷
  ∀ m.
  (HasState "stack" Stack m) ⇒
  [(Symbol, Type)] →
  m ExpandedOp
dropClosure env = do
  let count = length env
  modify @"stack" (\(x : xs) → x : drop count xs)
  pure (PrimEx (DIP (replicate count (PrimEx DROP))))

pop ∷
  (HasState "stack" Stack m, HasThrow "compilationError" CompilationError m) ⇒
  m (StackElem, Type)
pop = do
  s ← get @"stack"
  case s of
    x : xs → x <$ put @"stack" xs
    [] → throw @"compilationError" NotEnoughStackSpace

carN ∷ Int → ExpandedOp
carN 0 = SeqEx []
carN 1 = PrimEx (CAR "" "")
carN n = SeqEx [PrimEx (CAR "" ""), PrimEx (DIP [carN (n - 1)])]
