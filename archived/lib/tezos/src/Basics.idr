module Basics

%access public export
%default total

Not : Type -> Type
Not a = a -> Void

id : a -> a
id x = x

the : (a : Type) -> (value : a) -> a
the _ = id

const : a -> b -> a
const x = \value => x

fst : (a, b) -> a
fst (x, y) = x

snd : (a, b) -> b
snd (x, y) = y

infixr 9 .

(.) : (b -> c) -> (a -> b) -> a -> c
(.) f g = \x => f (g x)

flip : (f : a -> b -> c) -> b -> a -> c
flip f x y = f y x

apply : (a -> b) -> a -> b
apply f a = f a

cong : {f : t -> u} -> (a = b) -> f a = f b
cong Refl = Refl

data Dec : Type -> Type where
  Yes : (prf : prop) -> Dec prop
  No  : (contra : prop -> Void) -> Dec prop
