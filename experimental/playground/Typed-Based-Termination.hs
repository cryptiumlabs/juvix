import Juvix.Library hiding (Nat)

inl :: t1 → (t1 → t2) → p → t2
inl = \x k _l -> k x

inr :: t1 → p → (t1 → t2) → t2
inr = \y _k l -> l y

case' :: (t1 → t2 → t3) → t1 → t2 → t3
case' = \i k l -> i k l

type Plus x y = forall z. (x → z) → (y → z) → z

type Times x y = forall z. (x → y → z) → z

type Exists f = forall z. (forall x. f x → z) → z

type Mu f = (forall a. (f a → a) → a)

type One = forall x. x → x

type Nat' x = (Plus One x)

type Nat = Mu Nat'

-- Manually creating Mu
type Huffman b c = forall a. ((Plus c (b → a) → a) → a)

--leaf :: c -> Huffman b c
--leaf = inl

--zero'' :: Nat
--zero'' = undefined

zero' = inl ()

--one' :: Nat
one' = inr zero'

--succ = fix inr

newtype Huffman' b c = Huffman' (forall a. forall z. ((c → z) → (b → a → z) → z) → a)

leaf :: c → Huffman b c
leaf = inl
