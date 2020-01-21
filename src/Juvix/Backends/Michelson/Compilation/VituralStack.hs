-- |
-- - Serves as a virtual stack over Michelson
-- - This stack has a few properties
--   + The values on this stack may or may not be on the real
--     stack. However for convention this should be largely ignored,
--     except when you wish to do an operation like pair
--     * This can be fix in the future
--     * Until then, one should filter out the virtual stack items
-- - We keep virtual items on the ="stack"= as that makes the details
--   on whether something is constant propagation or not act
--   consistently with each other.
--   + After all, what may not be a constant now, may be in the
--     future, or vice versa!
-- - Import with qualified and the name of =VT=
module Juvix.Backends.Michelson.Compilation.VituralStack where

import qualified Juvix.Backends.Michelson.Parameterisation as Parameterisation
import Juvix.Library hiding (Type, drop, take)
import qualified Michelson.Untyped as Untyped
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
  = VarE Symbol (Maybe Val)
  | Val Val
  deriving (Show, Eq, Generic)

data Val
  = ConstE Parameterisation.Value
  | FuncResultE
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- T operation functions
--------------------------------------------------------------------------------

ins ∷ (Elem, Untyped.Type) → (Int → Int) → T → T
ins v f (T stack' size) = T (v : stack') (f size)

-- | 'inT' determines if the given
inT ∷ Elem → Bool
inT (VarE _ (Just FuncResultE)) = True
inT (VarE _ (Just (ConstE _))) = False
inT (VarE _ Nothing) = True
inT (Val (ConstE _)) = False
inT (Val FuncResultE) = True

-- invariant ¬ inT = valueOf is valid!
valueOfErr ∷ Elem → Parameterisation.Value
valueOfErr (VarE _ (Just (ConstE i))) = i
valueOfErr (Val (ConstE i)) = i
valueOfErr (Val FuncResultE) = error "called valueOf with a stored value"
valueOfErr (VarE _ Nothing) = error "called valueOf with a stored value"
valueOfErr (VarE _ (Just FuncResultE)) = error "called valueOf with a stored value"

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
nil = T [] 0

isNil ∷ T → Bool
isNil = (T [] 0 ==)

-- | 'consT', cons on a value v to our representation of the stack
-- This stack may have more values tha the real one, as we store
-- constants on this stack for resolution, however these will not appear
-- in the real michelson stack
consT ∷ HasState "stack" T m ⇒ (Elem, Untyped.Type) → m ()
consT = modify @"stack" . cons

take ∷ Int → T → T
take _ (T [] i) = T [] i -- i should be 0
take n stack@(T (_ : _) _)
  | n <= 0 = stack
  | otherwise = cons (car stack) (take (pred n) (cdr stack))

fromList ∷ Foldable t ⇒ t (Elem, Untyped.Type) → T
fromList = foldr cons (T [] 0)

append ∷ T → T → T
append (T pres size) (T posts size') = T (pres <> posts) (size + size')

appendDrop ∷ T → T → T
appendDrop prefix = append prefix . cdr

lookupType ∷ Symbol → T → Maybe Untyped.Type
lookupType n (T stack' _) = go stack'
  where
    go ((VarE n' _, typ) : _)
      | n' == n = Just typ
    go ((_, _) : xs) = go xs
    go [] = Nothing

drop ∷ (Ord t, Num t, Enum t) ⇒ t → T → T
drop n xs
  | n <= 0 = xs
  | otherwise = drop (pred n) (cdr xs)

data Lookup
  = Value Untyped.Value
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
      | n' == n && inT v =
        Just (Position acc)
      | n' == n =
        Just (Value (valueOfErr v))
    go ((v, _) : vs) acc
      | inT v = go vs (succ acc)
      | otherwise = go vs acc
    go [] _ = Nothing

dropFirst ∷ Symbol → T → [(Elem, Untyped.Type)] → T
dropFirst n (T stack' size) = go stack'
  where
    go ((v@(VarE n' _), _) : xs) acc
      | n' == n && inT v =
        T (reverse acc <> xs) (pred size)
      | n' == n =
        T (reverse acc <> xs) size
    go (v : vs) acc =
      go vs (v : acc)
    go [] _ = T stack' size
