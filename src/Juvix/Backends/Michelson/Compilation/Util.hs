-- |
-- Utility functions used by the Michelson backend.
module Juvix.Backends.Michelson.Compilation.Util where

import Juvix.Backends.Michelson.Compilation.Types hiding (Type)
import qualified Juvix.Backends.Michelson.Compilation.VirtualStack as VStack
import Juvix.Backends.Michelson.Compilation.VirtualStack (car, cdr, cons)
import Juvix.Library hiding (Type)
import Michelson.TypeCheck
import qualified Michelson.Typed as MT
import Michelson.Untyped

failWith ∷ HasThrow "compilationError" CompilationError m ⇒ Text → m a
failWith = throw @"compilationError" . InternalFault

-- TODO ∷ don't add a value if it is a constant
stackToStack ∷ VStack.T → SomeHST
stackToStack (VStack.T stack' _) =
  foldr
    ( \(_, ty) (SomeHST tail) →
        MT.withSomeSingT (MT.fromUType ty) $ \sty →
          SomeHST (sty -:& tail)
    )
    (SomeHST SNil)
    (filter (VStack.inT . fst) stack')

dupToFront ∷ Word → ExpandedOp
dupToFront 0 = PrimEx (DUP "")
dupToFront n = SeqEx [PrimEx (DIG n), PrimEx (DUP ""), PrimEx (DUG n)]

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
  ( HasState "stack" VStack.T m,
    HasThrow "compilationError" CompilationError m
  ) ⇒
  ExpandedOp →
  m ExpandedOp
genReturn instr = do
  modify @"stack" =<< instToStackEff instr
  pure instr

instToStackEff ∷
  ∀ m.
  (HasThrow "compilationError" CompilationError m) ⇒
  ExpandedOp →
  m (VStack.T → VStack.T)
instToStackEff instr =
  case instr of
    SeqEx is → do
      fs ← mapM instToStackEff is
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
            ( \s@(VStack.T ((_, Type (TPair _ _ x _) _) : _) _) →
                cons (VStack.Val VStack.FuncResultE, x) (cdr s)
            )
        CDR _ _ →
          pure
            ( \s@(VStack.T ((_, Type (TPair _ _ _ y) _) : _) _) →
                cons (VStack.Val VStack.FuncResultE, y) (cdr s)
            )
        PAIR _ _ _ _ →
          pure
            ( \ss@(VStack.T ((_, xT) : (_, yT) : _) _) →
                cons
                  (VStack.Val VStack.FuncResultE, Type (TPair "" "" xT yT) "")
                  (cdr (cdr ss))
            )
        DIP ops → do
          f ← instToStackEff (SeqEx ops)
          pure (\ss → cons (car ss) (f (cdr ss)))
        AMOUNT _ →
          pure (cons (VStack.Val VStack.FuncResultE, Type (Tc CMutez) ""))
        NIL _ _ _ →
          pure
            ( cons
                ( VStack.Val VStack.FuncResultE,
                  Type (TList (Type TOperation "")) ""
                )
            )
        _ →
          throw @"compilationError" (NotYetImplemented ("instToStackEff: " <> show p))
    _ → throw @"compilationError" (NotYetImplemented ("instToStackEff: " <> show instr))

pairN ∷ Int → ExpandedOp
pairN count = SeqEx (replicate count (PrimEx (PAIR "" "" "" "")))

unpackTuple ∷ ExpandedOp
unpackTuple =
  SeqEx
    [ PrimEx (DUP ""),
      PrimEx (CAR "" ""),
      PrimEx (DIP [PrimEx (CDR "" "")])
    ]

unpackTupleN ∷ (Eq t, Num t, Enum t) ⇒ t → ExpandedOp
unpackTupleN 0 = SeqEx []
unpackTupleN n =
  SeqEx
    [ unpackTuple,
      PrimEx (DIP [unpackTupleN (pred n)])
    ]

packClosure ∷
  ( HasState "stack" VStack.T m,
    HasThrow "compilationError" CompilationError m
  ) ⇒
  [Symbol] →
  m ExpandedOp
packClosure vars = do
  let count = length vars
  genReturn
    (SeqEx (replicate count (PrimEx (PAIR "" "" "" ""))))

unpackClosure ∷
  ∀ m.
  (HasState "stack" VStack.T m) ⇒
  [(Symbol, Type)] →
  m ExpandedOp
unpackClosure [] = pure (PrimEx DROP)
unpackClosure env = do
  let count = length env
  modify @"stack"
    ( VStack.append
        $ VStack.T (fmap (\(s, t) → (VStack.varE s Nothing, t)) env)
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
  (HasState "stack" VStack.T m) ⇒
  [(Symbol, Type)] →
  m ExpandedOp
dropClosure env = do
  let count = length env
  modify @"stack" (\xs → cons (car xs) (VStack.drop count (cdr xs)))
  pure (PrimEx (DIP (replicate count (PrimEx DROP))))

pop ∷
  (HasState "stack" VStack.T m, HasThrow "compilationError" CompilationError m) ⇒
  m (VStack.Elem, Type)
pop = do
  s ← get @"stack"
  case s of
    VStack.T (x : _) _ → x <$ put @"stack" (cdr s)
    VStack.T [] _ → throw @"compilationError" NotEnoughStackSpace

carN ∷ Int → ExpandedOp
carN 0 = SeqEx []
carN 1 = PrimEx (CAR "" "")
carN n = SeqEx [PrimEx (CAR "" ""), PrimEx (DIP [carN (n - 1)])]
