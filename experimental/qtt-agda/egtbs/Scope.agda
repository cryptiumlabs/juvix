open import Prelude
open Relation hiding (_∈_)

private
 variable
  𝓀 ℓ : Level
  A B I : Set ℓ
  w x y z : A
  ov : Bool


data Scope (A : Set ℓ) : Set ℓ where
  ◇   : Scope A
  _⍪_ : (xs : Scope A) (x : A) → Scope A
infixl 5 _⍪_
private variable ws xs ys zs : Scope A

Scoped : ∀ (A : Set ℓ) 𝓀 → Set _
Scoped A 𝓀 = Scope A → Set 𝓀
private variable S : Scoped A 𝓀

_↠_ : (I → Set ℓ) → (I → Set 𝓀) → Set _
S ↠ T = ∀ {i} → S i → T i
infixr 0 _↠_

_++_ : Op₂ (Scope A)
xs ++ ◇        = xs
xs ++ (ys ⍪ y) = (xs ++ ys) ⍪ y
infixl 5 _++_

[_] : A → Scope A
[ x ] = ◇ ⍪ x

map : (A → B) → Scope A → Scope B
map f ◇        = ◇
map f (xs ⍪ x) = map f xs ⍪ f x

foldl : (B → A → B) → B → Scope A → B
foldl f z ◇        = z
foldl f z (xs ⍪ x) = f (foldl f z xs) x


data _≤_ {A : Set ℓ} : Scope A → Scope A → Set where
  ◇  : ◇  ≤ ◇
  _✓ : xs ≤ ys → (xs ⍪ z) ≤ (ys ⍪ z)
  _✘ : xs ≤ ys → xs       ≤ (ys ⍪ z)
infix 2 _≤_
infixl 1000 _✓ _✘
private variable θ φ ψ : xs ≤ ys

≤-At : (A : Set ℓ) → Rel (Scope A) lzero
≤-At A = _≤_ {A = A}

pattern _✓[_] θ z = _✓ {z = z} θ
pattern _✘[_] θ z = _✘ {z = z} θ
infixl 1000 _✓[_] _✘[_]

_∈_ : A → Scope A → Set
x ∈ xs = [ x ] ≤ xs


data _⨟_↦_ {A : Set ℓ} :
  {xs ys zs : Scope A} (θ : xs ≤ ys) (φ : ys ≤ zs) (ψ : xs ≤ zs) → Set
 where
  ◇    : ◇ ⨟ ◇ ↦ ◇
  _✓✓✓ : θ ⨟ φ ↦ ψ → θ ✓ ⨟ φ ✓ ↦ ψ ✓[ z ]
  _✘✓✘ : θ ⨟ φ ↦ ψ → θ ✘ ⨟ φ ✓ ↦ ψ ✘[ z ]
  _-✘✘ : θ ⨟ φ ↦ ψ → θ   ⨟ φ ✘ ↦ ψ ✘[ z ]
infix 0 _⨟_↦_
infixl 1000 _✓✓✓ _✘✓✘ _-✘✘

_⨟_ : (θ : xs ≤ ys) (φ : ys ≤ zs) → ∃[ ψ ] (θ ⨟ φ ↦ ψ)
θ   ⨟ φ ✘ = let ψ , p = θ ⨟ φ in ψ ✘ , p -✘✘
θ ✓ ⨟ φ ✓ = let ψ , p = θ ⨟ φ in ψ ✓ , p ✓✓✓
θ ✘ ⨟ φ ✓ = let ψ , p = θ ⨟ φ in ψ ✘ , p ✘✓✘
◇   ⨟ ◇   = ◇ , ◇
infix 5 _⨟_

_⨟′_ : xs ≤ ys → ys ≤ zs → xs ≤ zs
θ ⨟′ φ = (θ ⨟ φ) .proj₁
infix 5 _⨟′_

instance ≤-refl : Reflexive $ ≤-At A
≤-refl {x = ◇}      = ◇
≤-refl {x = xs ⍪ _} = ≤-refl ✓

instance ≤-min : ◇ ≤ xs
≤-min {xs = ◇}      = ◇
≤-min {xs = xs ⍪ _} = ≤-min ✘


data Cover {A : Set ℓ} :
  {xs ys zs : Scope A} →
  (ov : Bool) (θ : xs ≤ zs) (φ : ys ≤ zs) → Set
 where
  ◇   : Cover ov ◇ ◇
  _✓✓ : Cover ov θ φ → Cover true (θ ✓) (φ ✓[ z ])
  _✓✘ : Cover ov θ φ → Cover ov   (θ ✓) (φ ✘[ z ])
  _✘✓ : Cover ov θ φ → Cover ov   (θ ✘) (φ ✓[ z ])

Partition : (θ : xs ≤ zs) (φ : ys ≤ zs) → Set
Partition = Cover false


record Coprod {A : Set ℓ} {xs ys zs : Scope A}
              (θ : xs ≤ zs) (φ : ys ≤ zs) : Set ℓ where
  constructor makeCp
  field
    {lub}   : Scope A
    {sub}   : lub ≤ zs
    {left}  : xs ≤ lub
    {right} : ys ≤ lub
    left-⨟  : left ⨟ sub ↦ θ
    right-⨟ : right ⨟ sub ↦ φ
    cover   : Cover true left right
open Coprod public

_⊕_ : (θ : xs ≤ zs) (φ : ys ≤ zs) → Coprod θ φ
◇   ⊕ ◇   = makeCp ◇ ◇ ◇
θ ✓ ⊕ φ ✓ = let makeCp l r c = θ ⊕ φ in makeCp (l ✓✓✓) (r ✓✓✓) (c ✓✓)
θ ✓ ⊕ φ ✘ = let makeCp l r c = θ ⊕ φ in makeCp (l ✓✓✓) (r ✘✓✘) (c ✓✘)
θ ✘ ⊕ φ ✓ = let makeCp l r c = θ ⊕ φ in makeCp (l ✘✓✘) (r ✓✓✓) (c ✘✓)
θ ✘ ⊕ φ ✘ = let makeCp l r c = θ ⊕ φ in makeCp (l -✘✘) (r -✘✘) c
infix 9 _⊕_


_+++_ : ws ≤ xs → ys ≤ zs → (ws ++ ys) ≤ (xs ++ zs)
θ +++ ◇   = θ
θ +++ φ ✓ = (θ +++ φ) ✓
θ +++ φ ✘ = (θ +++ φ) ✘
infixl 5 _+++_


data All {A : Set ℓ} (P : A → Set 𝓀) : Scoped A 𝓀 where
  ◇   : All P ◇
  _⍪_ : All P xs → P x → All P (xs ⍪ x)
