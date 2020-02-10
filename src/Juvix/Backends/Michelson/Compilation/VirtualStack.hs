-- |
-- - Serves as a virtual stack over Michelson
-- - This stack has a few properties
--   + The values on this stack may or may not be on the real
--     stack. However for convention this should be largely ignored,
--     except when you wish to do an operation like pair
--     * This can be fixed in the future
--     * Until then, one should filter out the virtual stack items
-- - We keep virtual items on the ="stack"= as that makes the details
--   on whether something is constant propagation or not act
--   consistently with each other.
--   + After all, what may not be a constant now, may be in the
--     future, or vice versa!
-- - Import with qualified and the name of =VStack=
module Juvix.Backends.Michelson.Compilation.VirtualStack where

import qualified Data.Set as Set
import qualified Juvix.Backends.Michelson.Parameterisation as Parameterisation
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import Juvix.Library hiding (Type, drop, take)
import qualified Juvix.Library.HashMap as Map
import qualified Michelson.Untyped as Untyped
import qualified Michelson.Untyped.Instr as Instr
import Prelude (error)

--------------------------------------------------------------------------------
-- T operation Type
--------------------------------------------------------------------------------

data T
  = T
      { stack' ∷ [(Elem, Untyped.Type)],
        size ∷ Int
      }
  deriving (Show, Eq)

data Elem
  = VarE (Set.Set Symbol) (Maybe Val)
  | Val Val
  deriving (Show, Eq, Generic)

varE ∷ Symbol → Maybe Val → Elem
varE x t = VarE (Set.singleton x) t

varNone ∷ Symbol → Elem
varNone x = VarE (Set.singleton x) Nothing

data LamPartial
  = LamPartial
      { ops ∷ [Parameterisation.Op],
        captures ∷ [Symbol], -- note: semantically this should be a set :)
        remArgs ∷ [Symbol],
        body ∷ Parameterisation.Term,
        ty ∷ ErasedAnn.Type Parameterisation.PrimTy Parameterisation.PrimVal
      }
  deriving (Show, Eq, Generic)

data Val
  = ConstE Parameterisation.Value
  | FuncResultE
  | LamPartialE LamPartial
  deriving (Show, Eq, Generic)

data NotInStack
  = Val' Untyped.Value
  | Lam' LamPartial

--------------------------------------------------------------------------------
-- T Instances
--------------------------------------------------------------------------------

instance Semigroup T where
  (T pres size) <> (T posts size') = T (pres <> posts) (size + size')

instance Monoid T where
  mempty = T [] 0

--------------------------------------------------------------------------------
-- T operation functions
--------------------------------------------------------------------------------

ins ∷ (Elem, Untyped.Type) → (Int → Int) → T → T
ins v f (T stack' size) = T (v : stack') (f size)

-- | 'inT' determines if the given element is on the real stack or not
inT ∷ Elem → Bool
inT (VarE _ (Just FuncResultE)) = True
inT (VarE _ (Just (ConstE _))) = False
inT (VarE _ (Just (LamPartialE _))) = False
inT (VarE _ Nothing) = True
inT (Val (ConstE _)) = False
inT (Val FuncResultE) = True
inT (Val (LamPartialE _)) = False

-- invariant ¬ inT = valueOf is valid!
notInStackOf ∷ Elem → NotInStack
notInStackOf (VarE _ (Just (LamPartialE l))) = Lam' l
notInStackOf (Val (LamPartialE l)) = Lam' l
notInStackOf (VarE _ (Just (ConstE i))) = Val' i
notInStackOf (Val (ConstE i)) = Val' i
notInStackOf (Val FuncResultE) = error "called valueOf with a stored value"
notInStackOf (VarE _ Nothing) = error "called valueOf with a stored value"
notInStackOf (VarE _ (Just FuncResultE)) = error "called valueOf with a stored value"

-- | 'car' gets the first element off the stack
-- may return an error
car ∷ T → (Elem, Untyped.Type)
car (T (s : _) _) = s
car (T [] _) = error "Called car on an empty list"

-- | 'cdr' removes the first element of the list
cdr ∷ T → T
cdr (T (s : ss) size)
  | inT (fst s) = T ss (pred size)
  | otherwise = T ss size
cdr (T [] size) = T [] size

-- | 'cons' is like 'consT', however it works ont ehs tack directly,
-- not from within a monad
cons ∷ (Elem, Untyped.Type) → T → T
cons v = ins v f
  where
    f
      | inT (fst v) = succ
      | otherwise = identity

nil ∷ T
nil = mempty

isNil ∷ T → Bool
isNil = (nil ==)

-- | 'consT', cons on a value v to our representation of the stack
-- This stack may have more values tha the real one, as we store
-- constants on this stack for resolution, however these will not appear
-- in the real michelson stack
consT ∷ HasState "stack" T m ⇒ (Elem, Untyped.Type) → m ()
consT = modify @"stack" . cons

take ∷ Int → T → T
take _ (T [] i) = T [] i -- i should be 0
take n stack@(T (_ : _) _)
  | n <= 0 = nil
  | otherwise = cons (car stack) (take (pred n) (cdr stack))

fromList ∷ Foldable t ⇒ t (Elem, Untyped.Type) → T
fromList = foldr cons nil

append ∷ T → T → T
append = (<>)

appendDrop ∷ T → T → T
appendDrop prefix = append prefix . cdr

lookupType ∷ Symbol → T → Maybe Untyped.Type
lookupType n (T stack' _) = go stack'
  where
    go ((VarE n' _, typ) : _)
      | Set.member n n' = Just typ
    go ((_, _) : xs) = go xs
    go [] = Nothing

promote ∷ Int → T → ([Instr.ExpandedOp], T)
promote _n stack
  | isNil stack = ([], stack)
promote 0 stack = ([], stack)
promote n stack =
  let (insts, newStack) = promote (pred n) (cdr stack)
   in let pushVal v t = Instr.PrimEx (Instr.PUSH "" t v) : insts
       in case car stack of
            (Val (ConstE v), t) →
              (pushVal v t, cons (Val FuncResultE, t) newStack)
            (VarE x (Just (ConstE v)), t) →
              (pushVal v t, cons (VarE x (Just FuncResultE), t) newStack)
            -- TODO ∷ add a case for lambda
            --        What do we dispatch to, to promote it?
            --        Maybe have to move this function into utils!?
            a →
              (insts, cons a newStack)

drop ∷ Int → T → T
drop n xs
  | n <= 0 = xs
  | otherwise = drop (pred n) (cdr xs)

data Lookup
  = Value NotInStack
  | Position Natural

-- | 'lookup' looks up a symbol from the stack
-- May return None if the symbol does not exist at all on the stack
-- Otherwise, the function returns Either
-- a Value if the symbol is not stored on the stack
-- or the position, if the value is stored on the stack
lookup ∷ Symbol → T → Maybe Lookup
lookup n (T stack' _) = go stack' 0
  where
    go ((v@(VarE n' _), _) : _) acc
      | Set.member n n' && inT v =
        Just (Position acc)
      | Set.member n n' =
        Just (Value (notInStackOf v))
    go ((v, _) : vs) acc
      | inT v = go vs (succ acc)
      | otherwise = go vs acc
    go [] _ = Nothing

dropFirst ∷ Symbol → T → [(Elem, Untyped.Type)] → T
dropFirst n (T stack' size) = go stack'
  where
    go ((v@(VarE n' _), _) : xs) acc
      | Set.member n n' && inT v =
        T (reverse acc <> xs) (pred size)
      | Set.member n n' =
        T (reverse acc <> xs) size
    go (v : vs) acc =
      go vs (v : acc)
    go [] _ = T stack' size

symbolsInT ∷ [Symbol] → T → [Symbol]
symbolsInT symbs (T stack' _) =
  filter f symbs
  where
    vars =
      Map.fromList
        ( concatMap
            ( \(x, _) →
                case x of
                  VarE s t → (\s → (s, t)) <$> Set.toList s
                  _ → []
            )
            stack'
        )
    f x =
      case Map.lookup x vars of
        Just _ → True
        Nothing → False

insertAt ∷ Foldable t ⇒ Int → t (Elem, Untyped.Type) → T → T
insertAt n xs stack =
  foldr cons (foldr cons postDrop xs) (stack' dropped)
  where
    postDrop = drop n stack
    dropped = take n stack
