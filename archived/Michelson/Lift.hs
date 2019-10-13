module Juvix.Backends.Michelson.Lift where

import           Control.Monad.Writer
import           Data.Typeable
import           Protolude                                  hiding (Const (..),
                                                             Either (..),
                                                             Option (..),
                                                             notImplemented)
import qualified Protolude                                  as P

import           Juvix.Backends.Michelson.Compilation.Types
import           Juvix.Backends.Michelson.Typed
import qualified Juvix.Backends.Michelson.Untyped           as U
import           Juvix.Utility

{-  Lift an untyped Michelson instruction sequence, with an initial stack type, into a typed (GADT) Michelson instruction sequence and return stack type in a typesafe manner.
    The type of the instruction sequence is *not* known until runtime (hence the heavy existential / scoped type variable use).
    If typechecking fails, return useful information about why and where.
    See [https://stackoverflow.com/questions/38024458/type-juggling-with-existentials-at-runtime] for some background on this mishmash of GHC extensions, although we need to do something more intricate.
    This would be far nicer with dependent types.   -}

-- Do we need to also pass an expected return type? e.g. Either
-- Intermediary lift stage for type annotation of future instructions?
-- We do *know* the stack state from exprToMichelson, we could keep it through this stage.
-- https://github.com/tezos/tezos/blob/master/src/proto/alpha/script_ir_translator.ml
-- return a λ on type, i.e. ExprUT → SomeStack → ?
-- ok, this is the right idea, will need to refine
-- but type of what, e.g. LEFT 2; IF_LEFT {...} {...} - don't *know* type
-- the annotation route is correct I think

liftUntyped ∷ ∀ m . (MonadWriter [CompilationLog] m, MonadError CompilationError m) ⇒ U.Expr → DynamicType → DynamicType → m (SomeExpr, DynamicType)
liftUntyped expr stk@(DynamicType (prx@(Proxy ∷ Proxy stkTy))) str@(DynamicType (Proxy ∷ Proxy storageTy)) = do

  --tell [TryLift expr stk str]

  let wrap ∷ TypecheckError → TypecheckError
      wrap = Wrapped ("expr: " <> prettyPrintValue expr <> ", stack: " <> prettyPrintProxy prx)

      notImplemented ∷ (MonadError CompilationError m) ⇒ m a
      notImplemented = throw (DidNotTypecheck $ wrap NotImplemented)

      cannotCastAs ∷ DynamicError → (MonadError CompilationError m) ⇒ m a
      cannotCastAs = throw . DidNotTypecheck . wrap . CannotCastAs

      take1 ∷ (MonadError CompilationError m) ⇒ m (DynamicType, DynamicType)
      take1 = case unProduct prx of
                P.Left e  → cannotCastAs e
                P.Right r → return r

      take2 ∷ (MonadError CompilationError m) ⇒ m (DynamicType, DynamicType, DynamicType)
      take2 = do
        (x, DynamicType r) ← take1
        case unProduct r of
          P.Left e       → cannotCastAs e
          P.Right (y, z) → return (x, y, z)

      take3 ∷ (MonadError CompilationError m) ⇒ m (DynamicType, DynamicType, DynamicType, DynamicType)
      take3 = do
        (x, y, DynamicType r) ← take2
        case unProduct r of
          P.Left e       → cannotCastAs e
          P.Right (a, b) → return (x, y, a, b)

      take4 ∷ (MonadError CompilationError m) ⇒ m (DynamicType, DynamicType, DynamicType, DynamicType, DynamicType)
      take4 = do
        (x, y, z, DynamicType r) ← take3
        case unProduct r of
          P.Left e       → cannotCastAs e
          P.Right (a, b) → return (x, y, z, a, b)

      take1As ∷ (MonadError CompilationError m) ⇒ (DynamicType → m b) → m (b, DynamicType)
      take1As func = do
        (head, rest) ← take1
        maybe ← func head
        return (maybe, rest)

      asUnion ∷ (MonadError CompilationError m) ⇒ DynamicType → m (DynamicType, DynamicType)
      asUnion (DynamicType prx) =
        case unSum prx of
          P.Left e  → cannotCastAs e
          P.Right r → return r

      asProduct ∷ (MonadError CompilationError m) ⇒ DynamicType → m (DynamicType, DynamicType)
      asProduct (DynamicType prx) =
        case unProduct prx of
          P.Left e  → cannotCastAs e
          P.Right r → return r

      asArrow ∷ (MonadError CompilationError m) ⇒ DynamicType → m (DynamicType, DynamicType)
      asArrow (DynamicType prx) =
        case unArrow prx of
          P.Left e  → cannotCastAs e
          P.Right r → return r

      take1Pair ∷ (MonadError CompilationError m) ⇒ m ((DynamicType, DynamicType), DynamicType)
      take1Pair = take1As asProduct

      take1Union ∷ (MonadError CompilationError m) ⇒ m ((DynamicType, DynamicType), DynamicType)
      take1Union = take1As asUnion

  case expr of

    U.Drop → do
      (DynamicType (_ ∷ Proxy a), DynamicType (rest ∷ Proxy b)) ← take1
      return (SomeExpr (Drop ∷ Expr (Stack (a, b)) (Stack b)), DynamicType rest)

    U.Dup → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b)) ← take1
      return (SomeExpr (Dup ∷ Expr (Stack (a, b)) (Stack (a, (a, b)))), DynamicType (Proxy ∷ Proxy (a, (a, b))))

    U.Swap → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b), DynamicType (_ ∷ Proxy c)) ← take2
      return (SomeExpr (Swap ∷ Expr (Stack (a, (b, c))) (Stack (b, (a, c)))), DynamicType (Proxy ∷ Proxy (b, (a, c))))

    U.Const c ->
      case c of
        U.Unit      → return (SomeExpr (Const () ∷ Expr (Stack stkTy) (Stack ((), stkTy))), DynamicType (Proxy ∷ Proxy ((), stkTy)))
        U.String s  → return (SomeExpr (Const s ∷ Expr (Stack stkTy) (Stack (Text, stkTy))), DynamicType (Proxy ∷ Proxy (Text, stkTy)))
        U.Bool b    → return (SomeExpr (Const b ∷ Expr (Stack stkTy) (Stack (Bool, stkTy))), DynamicType (Proxy ∷ Proxy (Bool, stkTy)))
        U.Tez t     → return (SomeExpr (Const (Tez t) ∷ Expr (Stack stkTy) (Stack (Tez, stkTy))), DynamicType (Proxy ∷ Proxy (Tez, stkTy)))
        U.Int i → return (SomeExpr (Const i ∷ Expr (Stack stkTy) (Stack (Integer, stkTy))), DynamicType (Proxy ∷ Proxy (Integer, stkTy)))

    U.ConsPair → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b), DynamicType (_ ∷ Proxy c)) ← take2
      return (SomeExpr (ConsPair ∷ Expr (Stack (a, (b, c))) (Stack (Pair a b, c))), DynamicType (Proxy ∷ Proxy (Pair a b, c)))

    U.Car → do
      ((DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b)), DynamicType (_ ∷ Proxy c)) ← take1Pair
      return (SomeExpr (Car ∷ Expr (Stack (Pair a b, c)) (Stack (a, c))), DynamicType (Proxy ∷ Proxy (a, c)))

    U.Cdr → do
      ((DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b)), DynamicType (_ ∷ Proxy c)) ← take1Pair
      return (SomeExpr (Cdr ∷ Expr (Stack (Pair a b, c)) (Stack (b, c))), DynamicType (Proxy ∷ Proxy (b, c)))

    U.IfLeft xUT yUT → do
      -- x ty is wrong?
      ((DynamicType (Proxy ∷ Proxy xT), DynamicType (Proxy ∷ Proxy yT)), DynamicType (_ ∷ Proxy z)) ← take1Union
      (SomeExpr (x ∷ Expr (Stack xS) (Stack xF)), xEnd) ← liftUntyped xUT (DynamicType (Proxy ∷ Proxy (xT, z))) str
      (SomeExpr (y ∷ Expr (Stack yS) (Stack yF)), _)    ← liftUntyped yUT (DynamicType (Proxy ∷ Proxy (yT, z))) str
      case (eqT ∷ Maybe (xF :~: yF), eqT ∷ Maybe (xS :~: (xT, z)), eqT ∷ Maybe (yS :~: (yT, z))) of
        (Just Refl, Just Refl, Just Refl) → return (SomeExpr (IfLeft x y ∷ Expr (Stack (Union xT yT, z)) (Stack xF)), xEnd)
        (Nothing, _, _) → cannotCastAs (CannotUnify (Proxy ∷ Proxy xF) (Proxy ∷ Proxy yF))
        (_, Nothing, _) → cannotCastAs (CannotUnify (Proxy ∷ Proxy xS) (Proxy ∷ Proxy (xT, z)))
        (_, _, Nothing) → cannotCastAs (CannotUnify (Proxy ∷ Proxy yS) (Proxy ∷ Proxy (yT, z)))

    U.AddIntInt → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b), DynamicType (_ ∷ Proxy c)) ← take2
      case (eqT ∷ Maybe (a :~: Integer), eqT ∷ Maybe (b :~: Integer)) of
        (Just Refl, Just Refl) → return (SomeExpr (AddIntInt ∷ Expr (Stack (Integer, (Integer, c))) (Stack (Integer, c))), DynamicType (Proxy ∷ Proxy (Integer, c)))
        (Nothing, _) → cannotCastAs (CannotUnify (Proxy ∷ Proxy a) (Proxy ∷ Proxy Integer))
        (_, Nothing) → cannotCastAs (CannotUnify (Proxy ∷ Proxy b) (Proxy ∷ Proxy Integer))

    U.MulIntInt → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b), DynamicType (_ ∷ Proxy c)) ← take2
      case (eqT ∷ Maybe (a :~: Integer), eqT ∷ Maybe (b :~: Integer)) of
        (Just Refl, Just Refl) → return (SomeExpr (MulIntInt ∷ Expr (Stack (Integer, (Integer, c))) (Stack (Integer, c))), DynamicType (Proxy ∷ Proxy (Integer, c)))
        (Nothing, _) → cannotCastAs (CannotUnify (Proxy ∷ Proxy a) (Proxy ∷ Proxy Integer))
        (_, Nothing) → cannotCastAs (CannotUnify (Proxy ∷ Proxy b) (Proxy ∷ Proxy Integer))

    U.AddTez → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b), DynamicType (_ ∷ Proxy c)) ← take2
      case (eqT ∷ Maybe (a :~: Tez), eqT ∷ Maybe (b :~: Tez)) of
        (Just Refl, Just Refl) → return (SomeExpr (AddTez ∷ Expr (Stack (Tez, (Tez, c))) (Stack (Tez, c))), DynamicType (Proxy ∷ Proxy (Tez, c)))
        (Nothing, _) → cannotCastAs (CannotUnify (Proxy ∷ Proxy a) (Proxy ∷ Proxy Tez))
        (_, Nothing) → cannotCastAs (CannotUnify (Proxy ∷ Proxy b) (Proxy ∷ Proxy Tez))

    U.Seq xUT yUT → do
      (SomeExpr (x ∷ Expr (Stack xS) (Stack xF)), xEnd) ← liftUntyped xUT stk str
      (SomeExpr (y ∷ Expr (Stack yS) (Stack yF)), yEnd) ← liftUntyped yUT xEnd str
      case eqT ∷ Maybe (xF :~: yS) of
        Just Refl → return (SomeExpr (Seq x y ∷ Expr (Stack xS) (Stack yF)), yEnd)
        Nothing   → cannotCastAs (CannotUnify (Proxy ∷ Proxy xF) (Proxy ∷ Proxy yS))

    U.If xUT yUT → do
      (DynamicType (_ ∷ Proxy a), DynamicType (rest ∷ Proxy b)) ← take1
      case eqT ∷ Maybe (a :~: Bool) of
        Nothing → cannotCastAs (CannotUnify (Proxy ∷ Proxy a) (Proxy ∷ Proxy Bool))
        Just Refl → do
          (SomeExpr (x ∷ Expr (Stack xS) (Stack xF)), DynamicType (Proxy ∷ Proxy xEndT)) ← liftUntyped xUT (DynamicType rest) str
          (SomeExpr (y ∷ Expr (Stack yS) (Stack yF)), DynamicType (Proxy ∷ Proxy yEndT)) ← liftUntyped yUT (DynamicType rest) str
          case (eqT ∷ Maybe (xS :~: yS), eqT ∷ Maybe (xF :~: yF), eqT ∷ Maybe (xEndT :~: yEndT)) of
            (Just Refl, Just Refl, Just Refl) → return (SomeExpr (If x y), DynamicType (Proxy ∷ Proxy xEndT))
            (Nothing, _, _) → cannotCastAs (CannotUnify (Proxy ∷ Proxy xS) (Proxy ∷ Proxy yS))
            (_, Nothing, _) → cannotCastAs (CannotUnify (Proxy ∷ Proxy xF) (Proxy ∷ Proxy yF))
            (_, _, Nothing) → cannotCastAs (CannotUnify (Proxy ∷ Proxy xEndT) (Proxy ∷ Proxy yEndT))

    U.Dip xUT → do
      (DynamicType (_ ∷ Proxy a), r@(DynamicType (_ ∷ Proxy b))) ← take1
      (SomeExpr (x ∷ Expr (Stack xS) (Stack xF)), _) ← liftUntyped xUT r str
      case eqT ∷ Maybe (xS :~: b) of
        Just Refl → return (SomeExpr (Dip x ∷ Expr (Stack (a, b)) (Stack (a, xF))), DynamicType (Proxy ∷ Proxy (a, xF)))
        Nothing   → cannotCastAs (CannotUnify (Proxy ∷ Proxy xS) (Proxy ∷ Proxy b))

    -- Deal with annotations here?
    U.Fail  → return (SomeExpr (Fail ∷ Expr (Stack stkTy) (Stack ())), stk)

    U.Nop   → return (SomeExpr (Nop ∷ Expr (Stack stkTy) (Stack stkTy)), stk)

    U.Nil ty  → do
      (DynamicType (_ ∷ Proxy tyL)) <- return (liftType ty)
      return (SomeExpr (Nil ∷ Expr (Stack stkTy) (Stack (List tyL, stkTy))), DynamicType (Proxy ∷ Proxy (List tyL, stkTy)))

    U.Now       → return (SomeExpr (Now ∷ Expr (Stack stkTy) (Stack (Timestamp, stkTy))), DynamicType (Proxy ∷ Proxy (Timestamp, stkTy)))
    U.Balance   → return (SomeExpr (Balance ∷ Expr (Stack stkTy) (Stack (Tez, stkTy))), DynamicType (Proxy ∷ Proxy (Tez, stkTy)))
    U.Amount    → return (SomeExpr (Amount ∷ Expr (Stack stkTy) (Stack (Tez, stkTy))), DynamicType (Proxy ∷ Proxy (Tez, stkTy)))

    _ -> notImplemented

{-  Lift a Michelson type into a Haskell type existential.  -}

liftType ∷ U.Type → DynamicType
liftType U.UnitT          = toDynamicType (Proxy ∷ Proxy ())
liftType U.KeyT           = toDynamicType (Proxy ∷ Proxy Key)
liftType U.HashT          = toDynamicType (Proxy ∷ Proxy Hash)
liftType U.IntT           = toDynamicType (Proxy ∷ Proxy Integer)
liftType U.TezT           = toDynamicType (Proxy ∷ Proxy Tez)
liftType U.BoolT          = toDynamicType (Proxy ∷ Proxy Bool)
liftType U.StringT        = toDynamicType (Proxy ∷ Proxy Text)
liftType U.OperationT     = toDynamicType (Proxy ∷ Proxy Operation)
liftType (U.EitherT a b)  =
  case (liftType a, liftType b) of
    (DynamicType (Proxy ∷ Proxy a), DynamicType (Proxy ∷ Proxy b)) → toDynamicType (Proxy ∷ Proxy (Union a b))
liftType (U.OptionT a) =
  case liftType a of
    DynamicType (Proxy ∷ Proxy a) → toDynamicType (Proxy ∷ Proxy (Option a))
liftType (U.ListT a) =
  case liftType a of
    DynamicType (Proxy ∷ Proxy a) → toDynamicType (Proxy ∷ Proxy (List a))
liftType (U.PairT a b) =
  case (liftType a, liftType b) of
    (DynamicType (Proxy ∷ Proxy x), DynamicType (Proxy ∷ Proxy y)) → toDynamicType (Proxy ∷ Proxy (Pair x y))
liftType (U.LamT a b) =
  case (liftType a, liftType b) of
    (DynamicType (Proxy ∷ Proxy x), DynamicType (Proxy ∷ Proxy y)) → toDynamicType (Proxy ∷ Proxy (x → y))

{- Starting stack type. -}

typeToStack ∷ U.Type → DynamicType
typeToStack ty =
  case liftType ty of
    DynamicType (Proxy ∷ Proxy t) → DynamicType (Proxy ∷ Proxy (t, ()))

stackToStack ∷ U.Stack → DynamicType
stackToStack []     = DynamicType (Proxy ∷ Proxy ())
stackToStack (h:tl) =
  case stackToStack tl of
     DynamicType (Proxy :: Proxy tt) ->
        case liftType (snd h) of
          DynamicType (Proxy :: Proxy ht) -> DynamicType (Proxy :: Proxy (ht, tt))
