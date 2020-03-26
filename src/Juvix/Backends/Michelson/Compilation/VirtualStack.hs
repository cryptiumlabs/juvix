{-# LANGUAGE TupleSections #-}

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
import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Type, drop, take)
import qualified Juvix.Library.HashMap as Map
import qualified Michelson.Untyped as Untyped
import qualified Michelson.Untyped.Instr as Instr
import Prelude (error)

-- TODO ∷ consolidate the various recursions into a generic combinator

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

varEName ∷ Symbol → Usage.T → Maybe (Val lamType) → Elem lamType
varEName x = VarE (Set.singleton x)

varE ∷ Symbol → Maybe (Val lamType) → Elem lamType
varE x = VarE (Set.singleton x) Usage.Omega

var1E ∷ Symbol → Maybe (Val lamType) → Elem lamType
var1E x = VarE (Set.singleton x) one

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
  | MichelsonLambda
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

realItems ∷ T lamType → Int
realItems (T _ size) = size

dropAllVirtual ∷ T lamType → T lamType
dropAllVirtual (T stack _) = T allReal (fromIntegral $ length allReal)
  where
    allReal = filter (inT . fst) stack

ins ∷ (Elem lamType, Untyped.Type) → (Int → Int) → T lamType → T lamType
ins v f (T stack' size) = T (v : stack') (f size)

-- | 'inT' determines if the given element is on the real stack or not
inT ∷ Elem lamType → Bool
inT (VarE _ _ (Just FuncResultE)) = True
inT (VarE _ _ (Just (ConstE _))) = False
inT (VarE _ _ (Just (LamPartialE _))) = False
inT (VarE _ _ (Just MichelsonLambda)) = True
inT (VarE _ _ Nothing) = True
inT (Val (ConstE _)) = False
inT (Val FuncResultE) = True
inT (Val (LamPartialE _)) = False
inT (Val MichelsonLambda) = True

-- invariant ¬ inT = valueOf is valid!
notInStackOf ∷ Elem lamType → NotInStack lamType
notInStackOf (VarE _ _ (Just (LamPartialE l))) = Lam' l
notInStackOf (Val (LamPartialE l)) = Lam' l
notInStackOf (VarE _ _ (Just (ConstE i))) = Val' i
notInStackOf (Val (ConstE i)) = Val' i
notInStackOf (Val FuncResultE) = notInError
notInStackOf (VarE _ _ Nothing) = notInError
notInStackOf (Val MichelsonLambda) = notInError
notInStackOf (VarE _ _ (Just FuncResultE)) = notInError
notInStackOf (VarE _ _ (Just MichelsonLambda)) = notInError

notInError ∷ a
notInError = error "called valueOf with a stored value"

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

isNil ∷ T lamType → Bool
isNil (T (_ : _) _) = False
isNil (T [] 0) = True
isNil (T [] _) = False

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

promote ∷
  ∀ m lamType.
  Monad m ⇒
  Int →
  T lamType →
  (lamType → m [Instr.ExpandedOp]) →
  m ([Instr.ExpandedOp], T lamType)
promote _n stack _
  | isNil stack = pure ([], stack)
promote 0 stack _ = pure ([], stack)
promote n stack f = do
  (insts, newStack) ← promote (pred n) (cdr stack) f
  let pushVal v t = Instructions.push t v : insts
  case car stack of
    (Val (ConstE v), t) →
      pure (pushVal v t, cons (Val FuncResultE, t) newStack)
    (VarE x i (Just (ConstE v)), t) →
      pure (pushVal v t, cons (VarE x i (Just FuncResultE), t) newStack)
    (VarE x i (Just (LamPartialE lamType)), t) → do
      insts ← f lamType
      pure (insts, cons (VarE x i (Just MichelsonLambda), t) newStack)
    a →
      pure (insts, cons a newStack)

addName ∷ Symbol → Symbol → T lamType → T lamType
addName toFind toAdd (T stack i) = T (f <$> stack) i
  where
    f (VarE x i t, type')
      | Set.member toFind x = (VarE (Set.insert toAdd x) i t, type')
    f t = t

nameTop ∷ Symbol → Usage.T → T lamType → T lamType
nameTop sym usage t =
  case hd of
    (Val i, ty) → cons (varEName sym usage (Just i), ty) rest
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
          (VarE x i _, _)
            | i /= mempty → drop (pred n) (updateUsage x (Usage.pred i) (cdr xs))
          _ →
            drop (pred n) (cdr xs)

-- This does not propagate usages!
dropPos ∷ Natural → T lamType → T lamType
dropPos 0 xs
  | not (isNil xs) && inT (fst (car xs)) = cdr xs
  | not (isNil xs) = dropPos 0 (cdr xs)
  | otherwise = xs
dropPos n xs
  | not (isNil xs) && inT (fst (car xs)) = cons (car xs) (dropPos (pred n) (cdr xs))
  | not (isNil xs) = cons (car xs) (dropPos n (cdr xs))
  | otherwise = xs

updateUsageList ∷ Set Symbol → Usage.T → [(Elem lamType, b)] → [(Elem lamType, b)]
updateUsageList symbs usage = f
  where
    f ((VarE s i ele, ty) : xs)
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

-- TODO ∷ Turn into a filter map!
lookupAllPos ∷ Symbol → T lamType → [Lookup lamType]
lookupAllPos n (T stack' _) = go stack' 0
  where
    go ((v@(VarE n' usage _), _) : xs) acc
      | Set.member n n' && inT v =
        Position usage acc : go xs (succ acc)
    go ((v, _) : vs) acc
      | inT v = go vs (succ acc)
      | otherwise = go vs acc
    go [] _ = []

-- | 'predValueUsage reduces usage of a constant by 1, and deletes said constant
-- if it goes over its usage. This function does nothing to items in the stack
predValueUsage ∷ Symbol → T lamType → T lamType
predValueUsage n s@(T stack' i) = go stack' []
  where
    go ((v@(VarE n' usage val), ty) : xs) acc
      | Set.member n n' && inT v =
        s
      | Set.member n n' && usage == one =
        T (reverse acc <> xs) i
      | otherwise =
        T (reverse acc <> ((VarE n' (Usage.pred usage) val, ty) : xs)) i
    go (x : xs) acc = go xs (x : acc)
    go [] _ = T stack' i

dig ∷ Int → T lamType → T lamType
dig i (T stack' n) =
  case splitAt i stack' of
    (xs, []) → T xs n
    (xs, y : ys) → T (y : xs <> ys) n

-- | 'dupDig' duplicates the element at position i to the front of the stack
-- with the full amount of usages leaving a one usage var left at the previous location
-- A Precondition is that at position n the 'usageOf y' >= 1
dupDig ∷ Int → T lamType → T lamType
dupDig i (T stack' n) =
  case splitAt i stack' of
    (xs, []) →
      T xs n
    (xs, (y, ty) : ys) →
      cons (predUsage y, ty) (T (xs <> ((usageOneOmega y, ty) : ys)) n)

dropFirst ∷ Symbol → T lamType → [(Elem lamType, Untyped.Type)] → T lamType
dropFirst n (T stack' size) = go stack'
  where
    go ((v@(VarE n' usages _), _) : xs) acc
      | Set.member n n' && inT v && usages /= mempty =
        T (reverse acc <> updateUsageList n' (Usage.pred usages) xs) (pred size)
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
                  VarE s _u t → (,t) <$> Set.toList s
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

usageOf ∷ Elem lamType → Usage.T
usageOf Val {} = Usage.Omega
usageOf (VarE _ usage _) = usage

predUsage ∷ Elem lamType → Elem lamType
predUsage v@Val {} = v
predUsage (VarE s usage val) = VarE s (Usage.pred usage) val

-- Sets the usage of a var to 1 unless it's omega
-- in which case we keep it at omega!
usageOneOmega ∷ Elem lamType → Elem lamType
usageOneOmega v@Val {} = v
usageOneOmega (VarE s Omega val) = VarE s Omega val
usageOneOmega (VarE s _ val) = VarE s one val
