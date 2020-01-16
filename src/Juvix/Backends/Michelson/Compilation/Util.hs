-- |
-- Utility functions used by the Michelson backend.
module Juvix.Backends.Michelson.Compilation.Util where

import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Library hiding (Type)
import Michelson.TypeCheck
import qualified Michelson.Typed as MT
import Michelson.Untyped

-- TODO ∷ find better name
failWith' ∷ HasThrow "compilationError" CompilationError m ⇒ Text → m a
failWith' = throw @"compilationError" . InternalFault

failWith ∷ HasThrow "compilationError" CompilationError m ⇒ Text → m ExpandedOp
failWith = throw @"compilationError" . InternalFault

-- TODO ∷ don't add a value if it is a constant
stackToStack ∷ Stack → SomeHST
stackToStack (Stack stack' _) =
  foldr
    ( \(_, ty) (SomeHST tail) →
        MT.withSomeSingT (MT.fromUType ty) $ \sty →
          SomeHST (sty -:& tail)
    )
    (SomeHST SNil)
    (filter (inStack . fst) stack')

append ∷ Stack → Stack → Stack
append (Stack pres size) (Stack posts size') = Stack (pres <> posts) (size + size')

appendDrop ∷ Stack → Stack → Stack
appendDrop prefix = append prefix . cdr

lookupType ∷ Symbol → Stack → Maybe Type
lookupType n (Stack stack' _) = go stack'
  where
    go ((VarE n' _, typ) : _)
      | n' == n = Just typ
    go ((_, _) : xs) = go xs
    go [] = Nothing

dropS ∷ (Ord t, Num t, Enum t) ⇒ t → Stack → Stack
dropS n xs
  | n <= 0 = xs
  | otherwise = dropS (pred n) (cdr xs)

data Lookup
  = Value Value
  | Position Natural

-- | 'lookup' looks up a symbol from the stack
-- May return None if the symbol does not exist at all on the stack
-- Otherwise, the function returns Either
-- a Value if the symbol is not stored on the stack
-- or the position, if the value is stored on the stack
lookup ∷ Symbol → Stack → Maybe Lookup
lookup n (Stack stack' _) = go stack' 0
  where
    go ((v@(VarE n' _), _) : _) acc
      | n' == n && inStack v =
        Just (Position acc)
      | n' == n =
        Just (Value (valueOfErr v))
    go ((v, _) : vs) acc
      | inStack v = go vs (succ acc)
      | otherwise = go vs acc

dropFirst ∷ Symbol → Stack → [(StackElem, Type)] → Stack
dropFirst n (Stack stack' size) = go stack'
  where
    go ((v@(VarE n' _), _) : xs) acc
      | n' == n && inStack v =
        Stack (reverse acc <> xs) (pred size)
      | n' == n =
        Stack (reverse acc <> xs) size
    go (v : vs) acc =
      go vs (v : acc)
    go [] _ = Stack stack' size

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
        DROP → pure cdr
        DUP _ → pure (\s → cons (car s) s)
        SWAP →
          pure
            ( \ss →
                let cs = cdr ss
                 in cons (car cs) (cons (car ss) cs)
            )
        -- TODO ∷ remove the FuncResultE of these, and have constant propagation
        CAR _ _ →
          pure
            ( \s@(Stack ((_, Type (TPair _ _ x _) _) : _) _) →
                cons (Val FuncResultE, x) (cdr s)
            )
        CDR _ _ →
          pure
            ( \s@(Stack ((_, Type (TPair _ _ _ y) _) : _) _) →
                cons (Val FuncResultE, y) (cdr s)
            )
        PAIR _ _ _ _ →
          pure
            ( \ss@(Stack ((_, xT) : (_, yT) : _) _) →
                cons
                  (Val FuncResultE, Type (TPair "" "" xT yT) "")
                  (cdr (cdr ss))
            )
        DIP ops → do
          f ← genFunc (SeqEx ops)
          pure (\ss → cons (car ss) (f (cdr ss)))
        AMOUNT _ → pure (cons (Val FuncResultE, Type (Tc CMutez) ""))
        NIL _ _ _ → pure (cons (Val FuncResultE, Type (TList (Type TOperation "")) ""))
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
  modify @"stack"
    ( append
        $ Stack (fmap (\(s, t) → (VarE s Nothing, t)) env)
        $ length env
    )
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
  modify @"stack" (\xs → cons (car xs) (dropS count (cdr xs)))
  pure (PrimEx (DIP (replicate count (PrimEx DROP))))

pop ∷
  (HasState "stack" Stack m, HasThrow "compilationError" CompilationError m) ⇒
  m (StackElem, Type)
pop = do
  s ← get @"stack"
  case s of
    Stack (x : _) _ → x <$ put @"stack" (cdr s)
    Stack [] _ → throw @"compilationError" NotEnoughStackSpace

carN ∷ Int → ExpandedOp
carN 0 = SeqEx []
carN 1 = PrimEx (CAR "" "")
carN n = SeqEx [PrimEx (CAR "" ""), PrimEx (DIP [carN (n - 1)])]
