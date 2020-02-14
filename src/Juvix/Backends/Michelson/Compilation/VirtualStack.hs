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
import qualified Juvix.Backends.Michelson.Compilation.Types as Types
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Type, drop, take)
import qualified Juvix.Library.HashMap as Map
import qualified Michelson.Untyped as Untyped
import qualified Michelson.Untyped.Instr as Instr
import Prelude (error)

--------------------------------------------------------------------------------
-- T operation Type
--------------------------------------------------------------------------------

data T lamType
  = T
      { stack' ∷ [(Elem lamType, Untyped.Type)],
        size ∷ Int
      }
  deriving (Show, Eq)

data Elem lamType
  = VarE (Set.Set Symbol) Usage.T (Maybe (Val lamType))
  | Val (Val lamType)
  deriving (Show, Eq, Generic)

varE ∷ Symbol → Maybe (Val lamType) → Elem lamType
varE x t = VarE (Set.singleton x) Usage.Omega t

varNone ∷ Symbol → Elem lamType
varNone x = VarE (Set.singleton x) Usage.Omega Nothing

data LamPartial
  = LamPartial
      { ops ∷ [Types.Op],
        captures ∷ [Symbol], -- note: semantically this should be a set :)
        remArgs ∷ [Symbol],
        body ∷ Types.Term,
        ty ∷ Types.Type
      }
  deriving (Show, Eq, Generic)

data Val lamType
  = ConstE Types.Value
  | FuncResultE
  | LamPartialE lamType
  deriving (Show, Eq, Generic)

data NotInStack lamType
  = Val' Untyped.Value
  | Lam' lamType

--------------------------------------------------------------------------------
-- T Instances
--------------------------------------------------------------------------------

instance Semigroup (T lamType) where
  (T pres size) <> (T posts size') = T (pres <> posts) (size + size')

instance Monoid (T lamType) where
  mempty = T [] 0

--------------------------------------------------------------------------------
-- T operation functions
--------------------------------------------------------------------------------

ins ∷ (Elem lamType, Untyped.Type) → (Int → Int) → T lamType → T lamType
ins v f (T stack' size) = T (v : stack') (f size)

-- | 'inT' determines if the given element is on the real stack or not
inT ∷ Elem lamType → Bool
inT (VarE _ _ (Just FuncResultE)) = True
inT (VarE _ _ (Just (ConstE _))) = False
inT (VarE _ _ (Just (LamPartialE _))) = False
inT (VarE _ _ Nothing) = True
inT (Val (ConstE _)) = False
inT (Val FuncResultE) = True
inT (Val (LamPartialE _)) = False

-- invariant ¬ inT = valueOf is valid!
notInStackOf ∷ Elem lamType → NotInStack lamType
notInStackOf (VarE _ _ (Just (LamPartialE l))) = Lam' l
notInStackOf (Val (LamPartialE l)) = Lam' l
notInStackOf (VarE _ _ (Just (ConstE i))) = Val' i
notInStackOf (Val (ConstE i)) = Val' i
notInStackOf (Val FuncResultE) = error "called valueOf with a stored value"
notInStackOf (VarE _ _ Nothing) = error "called valueOf with a stored value"
notInStackOf (VarE _ _ (Just FuncResultE)) = error "called valueOf with a stored value"

-- | 'car' gets the first element off the stack
-- may return an error
car ∷ T lamType → (Elem lamType, Untyped.Type)
car (T (s : _) _) = s
car (T [] _) = error "Called car on an empty list"

-- | 'cdr' removes the first element of the list
cdr ∷ T lamType → T lamType
cdr (T (s : ss) size)
  | inT (fst s) = T ss (pred size)
  | otherwise = T ss size
cdr (T [] size) = T [] size

-- | 'cons' is like 'consT', however it works ont ehs tack directly,
-- not from within a monad
cons ∷ (Elem lamType, Untyped.Type) → T lamType → T lamType
cons v = ins v f
  where
    f
      | inT (fst v) = succ
      | otherwise = identity

nil ∷ T lamType
nil = mempty

isNil ∷ Eq lamType ⇒ T lamType → Bool
isNil = (nil ==)

-- | 'consT', cons on a value v to our representation of the stack
-- This stack may have more values tha the real one, as we store
-- constants on this stack for resolution, however these will not appear
-- in the real michelson stack
consT ∷ HasState "stack" (T lamType) m ⇒ (Elem lamType, Untyped.Type) → m ()
consT = modify @"stack" . cons

take ∷ Int → T lamType → T lamType
take _ (T [] i) = T [] i -- i should be 0
take n stack@(T (_ : _) _)
  | n <= 0 = nil
  | otherwise = cons (car stack) (take (pred n) (cdr stack))

fromList ∷ Foldable t ⇒ t (Elem lamType, Untyped.Type) → T lamType
fromList = foldr cons nil

append ∷ T lamType → T lamType → T lamType
append = (<>)

appendDrop ∷ T lamType → T lamType → T lamType
appendDrop prefix = append prefix . cdr

lookupType ∷ Symbol → T lamType → Maybe Untyped.Type
lookupType n (T stack' _) = go stack'
  where
    go ((VarE n' _ _, typ) : _)
      | Set.member n n' = Just typ
    go ((_, _) : xs) = go xs
    go [] = Nothing

promote ∷ Eq lamType ⇒ Int → T lamType → ([Instr.ExpandedOp], T lamType)
promote _n stack
  | isNil stack = ([], stack)
promote 0 stack = ([], stack)
promote n stack =
  let (insts, newStack) = promote (pred n) (cdr stack)
   in let pushVal v t = Instr.PrimEx (Instr.PUSH "" t v) : insts
       in case car stack of
            (Val (ConstE v), t) →
              (pushVal v t, cons (Val FuncResultE, t) newStack)
            (VarE x i (Just (ConstE v)), t) →
              (pushVal v t, cons (VarE x i (Just FuncResultE), t) newStack)
            -- TODO ∷ add a case for lambda
            --        What do we dispatch to, to promote it?
            --        Maybe have to move this function into utils!?
            a →
              (insts, cons a newStack)

nameTop ∷ Symbol → T lamType → T lamType
nameTop sym t =
  case hd of
    (Val i, ty) → cons (varE sym (Just i), ty) rest
    (VarE s u mb, ty) → cons (VarE (Set.insert sym s) u mb, ty) rest
  where
    hd = car t
    rest = cdr t

drop ∷ Int → T lamType → T lamType
drop n xs
  | n <= 0 = xs
  | otherwise =
    let c = car xs
    in case c of
      (VarE x i _, _ )
        | i /= mempty → drop (pred n) (updateUsage x (Usage.pred i) xs)
      _ →
        drop (pred n) (cdr xs)

updateUsageList :: Set Symbol → Usage.T → [(Elem lamType, b)] → [(Elem lamType, b)]
updateUsageList symbs usage stack = f stack
  where
    f ((VarE s i ele ,ty) : xs)
      | not (Set.disjoint symbs s) = (VarE s (i <> usage) ele, ty) : xs
    f (x : xs) = x : f xs
    f [] = []


updateUsage ∷ Set.Set Symbol → Usage.T → T lamType → T lamType
updateUsage symbs usage (T stack i) = T (updateUsageList symbs usage stack) i

data Lookup lamType
  = Value (NotInStack lamType)
  | Position Usage.T Natural

-- | 'lookup' looks up a symbol from the stack
-- May return None if the symbol does not exist at all on the stack
-- Otherwise, the function returns Either
-- a Value if the symbol is not stored on the stack
-- or the position, if the value is stored on the stack
lookup ∷ Symbol → T lamType → Maybe (Lookup lamType)
lookup n (T stack' _) = go stack' 0
  where
    go ((v@(VarE n' usage _), _) : _) acc
      | Set.member n n' && inT v =
        Just (Position usage acc)
      | Set.member n n' =
        Just (Value (notInStackOf v))
    go ((v, _) : vs) acc
      | inT v = go vs (succ acc)
      | otherwise = go vs acc
    go [] _ = Nothing

dig ∷ Int → T lamType → T lamType
dig i (T stack' n) =
  case splitAt i stack' of
    (xs, []) → T xs n
    (xs, y : ys) → T (y : xs <> ys) n

dupDig ∷ Int → T lamType → T lamType
dupDig i (T stack' n) =
  case splitAt i stack' of
    (xs, []) →
      T xs n
    (xs, (y, ty) : ys) →
      cons (predUsage y, ty) (T (xs <> ((usageOne y, ty) : ys)) n)

dropFirst ∷ Symbol → T lamType → [(Elem lamType, Untyped.Type)] → T lamType
dropFirst n (T stack' size) = go stack'
  where
    -- in case c of
    --   (VarE x i _, _ )
    --     | i /= mempty → drop (pred n) (updateUsage x (Usage.pred i) xs)
    go ((v@(VarE n' usages _), _) : xs) acc
      | Set.member n n' && inT v && usages /= mempty =
        T (reverse acc <> (updateUsageList n' (Usage.pred usages) xs)) (pred size)
      | Set.member n n' && inT v =
        T (reverse acc <> xs) (pred size)
      | Set.member n n' =
        T (reverse acc <> xs) size
    go (v : vs) acc =
      go vs (v : acc)
    go [] _ = T stack' size

symbolsInT ∷ [Symbol] → T lamType → [Symbol]
symbolsInT symbs (T stack' _) =
  filter f symbs
  where
    vars =
      Map.fromList
        ( concatMap
            ( \(x, _) →
                case x of
                  VarE s _u t → (\s → (s, t)) <$> Set.toList s
                  _ → []
            )
            stack'
        )
    f x =
      case Map.lookup x vars of
        Just _ → True
        Nothing → False

insertAt ∷ Foldable t ⇒ Int → t (Elem lamType, Untyped.Type) → T lamType → T lamType
insertAt n xs stack =
  foldr cons (foldr cons postDrop xs) (stack' dropped)
  where
    postDrop = drop n stack
    dropped = take n stack

--------------------------------------------------------------------------------
-- Usage Manipulation
--------------------------------------------------------------------------------

predUsage ∷ Elem lamType → Elem lamType
predUsage v@(Val {}) = v
predUsage (VarE s usage val) = VarE s (Usage.pred usage) val

usageOne ∷ Elem lamType → Elem lamType
usageOne v@(Val {}) = v
usageOne (VarE s _ val) = VarE s one val
