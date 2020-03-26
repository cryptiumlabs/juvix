import Juvix.Library hiding (Nat, foldM, map, one, pred, succ, zero)

newtype Nat = Nat {unNat :: forall r. r -> (Nat -> r) -> r}

zero :: Nat
zero = Nat $ \z -> \s -> z

succ :: Nat -> Nat
succ = \n -> Nat $ \z -> \s -> s n

cfun :: forall r. Nat -> r -> (Nat -> r) -> r
cfun = \n -> \a -> \b -> unNat n a b

one :: Nat
one = succ zero

two :: Nat
two = succ one

three :: Nat
three = succ two

four :: Nat
four = succ three

isEven :: Nat -> Bool
isEven = \n -> cfun n True (\p -> not (isEven p))

add :: Nat -> Nat -> Nat
add = \n -> \m -> cfun n m (\x -> succ (add x m))

pred :: Nat -> Nat
pred = \n -> cfun n zero (\x -> x)

toNum :: Nat -> Int
toNum = \n -> cfun n 0 (\x -> 1 + toNum x)

newtype List a = List {unList :: forall r. r -> (a -> List a -> r) -> r}

nil :: forall a. List a
nil = List (\n c -> n)

cons :: forall a. a -> List a -> List a
cons x xs = List (\n c -> c x xs)

isNull :: forall a. List a -> Bool
isNull l = unList l True (const (const False))

map :: forall a b. (a -> b) -> List a -> List b
map f l = unList l nil (\x xs -> cons (f x) (map f xs))
