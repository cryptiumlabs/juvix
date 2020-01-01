Juvix's frontend syntax is primarily inspired by Haskell & Idris, with alterations for explicit usage accounting & (in the future) for resource accounting.

```
zipWith : n (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
zipWith _ Nil       Nil       = Nil
zipWith f (x :: xs) (y :: ys) = f x y :: zipWith f xs ys

zipAdd : Num a => Vect n a -> Vect n a -> Vect n a
zipAdd = zipWith (+)

test_zip_add : zipAdd (1 :: 2 :: 3) = 6
test_zip_add = Refl
```

elaborates to

```
zipWith : (a : 0 Type) -> (b : 0 Type) -> (c : 0 Type) -> (n : 0 Nat) -> n (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
zipWith (a : 0 Type) (b : 0 Type) (c : 0 Type) (Z : 0 Nat)   _ Nil        Nil       = Nil
zipWith (a : 0 Type) (b : 0 Type) (c : 0 Type) (S k : 0 Nat) f (x :: xs)  (y :: ys) = f x y :: zipWith a b c k f xs ys

zipAdd : (a : 0 Type) -> (n : 0 Nat) -> (d : 0 (Num a)) -> Vect n a -> Vect n a -> Vect n a
zipAdd a n d xs ys = zipWith a a a n ((+) d) xs ys
```

Usage annotations are optional. Implicit usage arguments or constraints are inferred where possible, but explicit annotations may sometimes be required.

```
f : 2 (x : 3 Int) -> Double

f : w (x : 2 Int -> y : () -> IO ())
```

By default, no usage annotation is equivalent to an implicit usage argument. The unification algorithm will attempt to infer constraints
involving multiple usage arguments where possible.
