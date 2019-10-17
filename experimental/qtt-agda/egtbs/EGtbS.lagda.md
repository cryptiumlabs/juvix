---
title: Everybody's Got to be Somewhere
...

<!--
  this file can be loaded into agda too, you just have to
  'M-x agda-mode' yourself

  to generate html:
  $ agda --html --html-highlight=auto EGtbS.lagda.md
  $ pandoc html/EGtbS.md -s -o html/EGtbS.html
-->

Link to paper: <https://arxiv.org/abs/1807.04085>

```
open import Prelude
  hiding (_⟨_⟩_ ; _∘′_ ; _∘_) renaming (id to idᶠ)
open import Data.List as List
  using (List ; _∷_ ; [] ; [_]) renaming (_++_ to _++ᴸ_)
open import Data.List.Relation.Unary.All as All using (All ; _∷_ ; [])

private
 variable
  a b c : Level
  A B : Set a
  @0 ov : Bool
  @0 x y z : A
  @0 ws ws′ xs xs′ ys ys′ zs zs′ : List A
```

## Scoped types & thinnings

A `Scoped A` is just a type parameterised over a list (or scope) of `A`s.
In the paper, `Scoped A` is written as Ā.

```
Scoped : ∀ {a} (A : Set a) → Set (lsuc a)
Scoped {a} A = @0 List A → Set a
private variable S T U : Scoped A

_↠_ : {A : Set a} → (@0 A → Set b) → (@0 A → Set b) → Set _
S ↠ T = ∀ {@0 x} → S x → T x
infixr -1000 _↠_
```

An _order-preserving embedding_, or _thinning_, is a proof that one
list is a subsequence of another. For example, [1, 3] is a subsequence
of [1, 2, 3, 4, 5].

```
data _≼_ {A : Set a} : Rel (List A) lzero where
  oz : [] ≼ []
  os : xs ≼ ys → z ∷ xs ≼ z ∷ ys
  o′ : xs ≼ ys →     xs ≼ z ∷ ys
infix 4 _≼_
private variable @0 θ θ′ θ″ φ φ′ φ″ ψ ψ′ ψ″ : xs ≼ ys

_ : (List ℕ ∋ 1 ∷ 3 ∷ []) ≼ 1 ∷ 2 ∷ 3 ∷ 4 ∷ 5 ∷ []
_ = os (o′ (os (o′ (o′ oz))))
```

```
id : {xs : List A} → xs ≼ xs
id {xs = []}    = oz
id {xs = _ ∷ _} = os id

data _∘_↦_ {A : Set a} :
  {@0 xs ys zs : List A} →
  (θ : ys ≼ zs) (φ : xs ≼ ys) (ψ : xs ≼ zs) → Set a
 where
  ozzz : oz ∘ oz ↦ oz
  osss : θ ∘ φ ↦ ψ → os {z = z} θ ∘ os φ ↦ os ψ
  os′′ : θ ∘ φ ↦ ψ → os {z = z} θ ∘ o′ φ ↦ o′ ψ
  o′-′ : θ ∘ φ ↦ ψ → o′ {z = z} θ ∘ φ ↦ o′ ψ


_∘_ : (θ : ys ≼ zs) (φ : xs ≼ ys) → ∃₀ (θ ∘ φ ↦_)
oz   ∘ oz   = oz , erase ozzz
os θ ∘ os φ = Σ.map₀ os osss $ θ ∘ φ
os θ ∘ o′ φ = Σ.map₀ o′ os′′ $ θ ∘ φ
o′ θ ∘ φ    = Σ.map₀ o′ o′-′ $ θ ∘ φ
infixl 9 _∘_

_∘′_ : (θ : ys ≼ zs) (φ : xs ≼ ys) → xs ≼ zs
θ ∘′ φ = (θ ∘ φ) .proj₁
```

`_≼_` forms a category (the objects being scopes, i.e. elements of
`List A`) with `id` as the identity and `_∘′_` as composition.

```
@0 id-∘ : (φ : xs ≼ ys) → id ∘′ φ ≡ φ
id-∘ oz = refl
id-∘ (os φ) rewrite id-∘ φ = refl
id-∘ (o′ φ) rewrite id-∘ φ = refl

@0 ∘-id : (θ : xs ≼ ys) → θ ∘′ id ≡ θ
∘-id oz = refl
∘-id (os θ) rewrite ∘-id θ = refl
∘-id (o′ θ) rewrite ∘-id θ = refl

@0 ∘-∘ : (θ : ys ≼ zs) (φ : xs ≼ ys) (ψ : ws ≼ xs) →
         (θ ∘′ φ) ∘′ ψ ≡ θ ∘′ (φ ∘′ ψ)
∘-∘ oz     oz     oz     = refl
∘-∘ (os θ) (os φ) (os ψ) rewrite ∘-∘ θ φ ψ = refl
∘-∘ (os θ) (os φ) (o′ ψ) rewrite ∘-∘ θ φ ψ = refl
∘-∘ (os θ) (o′ φ) ψ      rewrite ∘-∘ θ φ ψ = refl
∘-∘ (o′ θ) φ      ψ      rewrite ∘-∘ θ φ ψ = refl
```

`[]` is an initial object in this category.

```
empty : {xs : List A} → [] ≼ xs
empty {xs = []}    = oz
empty {xs = _ ∷ _} = o′ empty

@0 empty-unique : (θ : [] ≼ xs) → θ ≡ empty
empty-unique oz     = refl
empty-unique (o′ θ) = ≡.cong o′ $ empty-unique θ

```

`_⇑_` packages a value up with a thinning, so that `T ⇑ xs` means "`T
xs′`, for some `xs′ ≼ xs`". Using identity & composition of the
thinnings you can form a monad.

```
module ⇑ where
  record _⇑_ {A : Set a} (T : Pred (List A) b) (@0 scope : List A) :
    Set (a ⊔ b)
   where
    constructor _↑_
    field
      @0 {support} : List A
      thing        : T support
      thinning     : support ≼ scope
  open _⇑_ public
  infixl 0 _⇑_
  infixl 10 _↑_

  thin : xs ≼ ys → T ⇑ xs → T ⇑ ys
  thin θ (t ↑ φ) = t ↑ θ ∘′ φ

  return : T ↠ T ⇑_
  return t = t ↑ id

  map : (T ↠ U) → T ⇑_ ↠ U ⇑_
  map f (t ↑ θ) = f t ↑ θ

  join : (T ⇑_) ⇑_ ↠ T ⇑_
  join (t ↑ θ) = thin θ t

  _=<<_ : (T ↠ U ⇑_) → T ⇑_ ↠ U ⇑_
  k =<< t = join $ map k t
  infixr 1 _=<<_

  _<=<_ : (T ↠ U ⇑_) → (S ↠ T ⇑_) → (S ↠ U ⇑_)
  (g <=< f) x = g =<< f x
  infixl 1 _<=<_

  module Laws where
    open ≡ using (_≗_)

    private variable f g h : S ↠ T ⇑_

    @0 return-<=< : (return <=< f) ≗ f {xs}
    return-<=< {f = f} t rewrite ∘-id (f t .thinning) = refl

    @0 <=<-return : (f <=< return) ≗ f {xs}
    <=<-return {f = f} t rewrite id-∘ (f t .thinning) = refl

    @0 <=<-<=< : (f <=< (g <=< h)) ≗ ((f <=< g) <=< h) {xs}
    <=<-<=< {f = f} {g = g} {h = h} t =
      let u ↑ θ = h t ; v ↑ φ = g u ; w ↑ ψ = f v in
      ≡.cong (w ↑_) (∘-∘ θ φ ψ)
open ⇑ public using (_⇑_ ; _↑_ ; support ; thing ; thinning)
```

## Covers & partitions

Two thinnings of a scope `xs` are a _cover_ if between them they
mention all variables in `xs`. A _partition_ is a cover where no
variables are shared between the two (this is tracked by the `Bool`
argument to `Cover′`).

```
data Cover′ {A : Set a} :
  {@0 xs ys zs : List A} → Bool → xs ≼ zs → ys ≼ zs → Set
 where
  czz : Cover′ ov oz oz
  css : Cover′ ov θ φ → Cover′ true (os {z = z} θ) (os φ)
  cs′ : Cover′ ov θ φ → Cover′ ov   (os {z = z} θ) (o′ φ)
  c′s : Cover′ ov θ φ → Cover′ ov   (o′ {z = z} θ) (os φ)

Cover Partition : {A : Set a} {@0 xs ys zs : List A} →
                  xs ≼ zs → ys ≼ zs → Set
Cover     = Cover′ true
Partition = Cover′ false

toCover : Cover′ ov θ φ → Cover θ φ
toCover czz     = czz
toCover (css c) = css c
toCover (cs′ c) = cs′ (toCover c)
toCover (c′s c) = c′s (toCover c)
```

Subscopes of a scope `zs` can be form the _slice_ category over `zs`:

- The **objects** are pairs of a subscope and a thinning from `zs`
- A **morphism** `(ys , φ) →/ (xs , θ)` is a thinning `ψ : ys ≼ xs`
  which composes with θ to produce φ.

The thinning from `[]` is an initial object here too

```
Subscope : List A → Set _
Subscope zs = ∃₀[ ys ] (ys ≼ zs)

_→/_ : ∀ {xs ys zs : List A} → ys ≼ zs → xs ≼ zs → Set _
_→/_ {xs = xs} {ys = ys} φ θ = Σ₀[ ψ ∈ ys ≼ xs ] (θ ∘ ψ ↦ φ)
infixr -1000 _→/_

empty/ : (θ : xs ≼ ys) → empty →/ θ
empty/ θ with θ ∘ empty
... | θ′ , p rewrite empty-unique θ′ = empty , p
```

The coproducts in this slice category merge subscopes:
If `xs, ys ≼ zs`, then the coproduct `ws` contains all the variables
from `xs` and `ys` (and no others), and `ws ≼ zs`.

```
record Coprod {A : Set a} {@0 xs ys zs : List A}
              (θ : xs ≼ zs) (φ : ys ≼ zs) : Set a where
  constructor makeCp
  field
    @0 {lub}    : List A
    {sub}       : lub ≼ zs
    @0 {left}   : xs ≼ lub
    @0 {right}  : ys ≼ lub
    @0 leftTri  : sub ∘ left ↦ θ
    @0 rightTri : sub ∘ right ↦ φ
    @0 cover    : Cover left right
open Coprod

_⊕_ : (θ : xs ≼ zs) (φ : ys ≼ zs) → Coprod θ φ
oz   ⊕ oz   = makeCp ozzz ozzz czz
os θ ⊕ os φ = let makeCp l r c = θ ⊕ φ in makeCp (osss l) (osss r) (css c)
os θ ⊕ o′ φ = let makeCp l r c = θ ⊕ φ in makeCp (osss l) (os′′ r) (cs′ c)
o′ θ ⊕ os φ = let makeCp l r c = θ ⊕ φ in makeCp (os′′ l) (osss r) (c′s c)
o′ θ ⊕ o′ φ = let makeCp l r c = θ ⊕ φ in makeCp (o′-′ l) (o′-′ r) c
infixl 9 _⊕_

@0 ⊕-univ′ : ψ  ∘ θ′ ↦ θ → Cover′ ov θ′ φ′ → ψ ∘ φ′ ↦ φ →
             ψ′ ∘ θ″ ↦ θ →   -- θ →/ ψ′
             ψ′ ∘ φ″ ↦ φ →   -- φ →/ ψ′
             ψ →/ ψ′
⊕-univ′ ozzz     czz     ozzz     ozzz     ozzz     = oz ,₀ ozzz
⊕-univ′ (osss L) (css C) (osss R) (osss A) (osss B) =
  Σ.map₀ os osss $ ⊕-univ′ L C R A B
⊕-univ′ (osss L) (cs′ C) (os′′ R) (osss A) (os′′ B) =
  Σ.map₀ os osss $ ⊕-univ′ L C R A B
⊕-univ′ (os′′ L) (c′s C) (osss R) (os′′ A) (osss B) =
  Σ.map₀ os osss $ ⊕-univ′ L C R A B
⊕-univ′ (o′-′ L) C       (o′-′ R) (os′′ A) (os′′ B) =
  Σ.map₀ o′ os′′ $ ⊕-univ′ L C R A B
⊕-univ′ (o′-′ L) C (o′-′ R) (o′-′ A) (o′-′ B) =
  Σ.map₀ idᶠ o′-′ $ ⊕-univ′ L C R A B

@0 ⊕-univ : {θ φ : xs ≼ ys} (c : Coprod θ φ) → (θ ⊕ φ) .sub →/ c .sub
⊕-univ {θ = θ} {φ = φ} c =
  let module ⊕ = Coprod (θ ⊕ φ) ; module c = Coprod c in
  ⊕-univ′ ⊕.leftTri ⊕.cover ⊕.rightTri c.leftTri c.rightTri
```

## Pieces of syntax

Based on these concepts we can make a kit for defining syntaxes with
binding.

Constants have no variables in their scope.

```
data ⊤ᴿ {A : Set a} : Scoped A where tt : ⊤ᴿ []

ttᴿ : ⊤ᴿ ⇑ xs
ttᴿ = tt ↑ empty
```

Binary operators have two subterms and the scope is the union of that
of the two subterms. (This is where coproducts come in.)

```
record _×ᴿ_ {A : Set a} (S T : Scoped A) (zs : List A) : Set a where
  constructor pairᴿ
  field
    outˡ : S ⇑ zs
    outʳ : T ⇑ zs
    @0 cover : Cover (outˡ .thinning) (outʳ .thinning)
open _×ᴿ_
infixr 2 _×ᴿ_

_,ᴿ_ : S ⇑ xs → T ⇑ xs → (S ×ᴿ T) ⇑ xs
(s ↑ θ) ,ᴿ (t ↑ φ) =
  let module c = Coprod (θ ⊕ φ) in
  pairᴿ (s ↑ c.left) (t ↑ c.right) (c.cover) ↑ c.sub
infixr 4 _,ᴿ_
```

Binding structures have extra variables in the scope of their subterm,
so they need to be able to split a thinning `xs ≼ ys ++ᴸ zs` into some
`xs₁ ≼ ys` and `xs₂ ≼ zs`.

```
_++_ : (ws ≼ xs) → (ys ≼ zs) → (ws ++ᴸ ys ≼ xs ++ᴸ zs)
oz   ++ φ = φ
os θ ++ φ = os (θ ++ φ)
o′ θ ++ φ = o′ (θ ++ φ)

record Split {A : Set a} {@0 xs : List A} ys {@0 zs}
             (ψ : xs ≼ ys ++ᴸ zs) : Set a where
  constructor makeSplit
  field
    @0 {first second} : List A
    firstSub  : first ≼ ys
    secondSub : second ≼ zs
    @0 listEq : xs ≡ first ++ᴸ second
    @0 subEq  : firstSub ++ secondSub ≡ ≡.subst _ listEq ψ

_⊣_ : ∀ (ys : List A) (ψ : xs ≼ ys ++ᴸ zs) → Split ys ψ
[]       ⊣ ψ    = makeSplit oz ψ refl refl
(y ∷ ys) ⊣ os ψ with ys ⊣ ψ
(y ∷ ys) ⊣ os .(θ ++ φ) | makeSplit θ φ refl refl = makeSplit (os θ) φ refl refl
(y ∷ ys) ⊣ o′ ψ with ys ⊣ ψ
(y ∷ ys) ⊣ o′ .(θ ++ φ) | makeSplit θ φ refl refl = makeSplit (o′ θ) φ refl refl
infix 10 _⊣_
```

The binder itself records which of the bound variables are actually
used with the `xs ≼ ys` argument.

```
data _⊢_ (ys : List A) (T : Scoped A) : Scoped A where
  _\\_ : xs ≼ ys → T (xs ++ᴸ zs) → (ys ⊢ T) zs
infix 10 _⊢_
infix 20 _\\_

_\\ᴿ_ : (ys : List A) → T ⇑ (ys ++ᴸ zs) → (ys ⊢ T) ⇑ zs
ys \\ᴿ (t ↑ ψ) with ys ⊣ ψ
ys \\ᴿ (t ↑ .(θ ++ φ)) | makeSplit θ φ refl refl = θ \\ t ↑ φ
infix 20 _\\ᴿ_
```

Occurrences of a variable have only that variable in their scope.

```
data Vaᴿ (x : A) : Scoped A where
  only : Vaᴿ x [ x ]

_∈_ : A → List A → Set _
x ∈ xs = [ x ] ≼ xs

vaᴿ : x ∈ xs → Vaᴿ x ⇑ xs
vaᴿ e = only ↑ e
```

As an example, untyped λ-calculus:

```
-- de Bruijn
data Lamᴰ {A : Set a} : Scoped A where
  `_    : x ∈ xs → Lamᴰ xs
  _∙_   : (f s : Lamᴰ xs) → Lamᴰ xs
  Λ[_]_ : ∀ x → (t : Lamᴰ (x ∷ xs)) → Lamᴰ xs

-- "co-de Bruijn"
data Lamᴿ {A : Set a} : Scoped A where
  `_    : Vaᴿ x ↠ Lamᴿ
  app   : (Lamᴿ ×ᴿ Lamᴿ) ↠ Lamᴿ
  Λ[_]_ : ∀ x → ([ x ] ⊢ Lamᴿ) ↠ Lamᴿ

-- convert dB to co-dB
lamᴿ : Lamᴰ xs → Lamᴿ ⇑ xs
lamᴿ (` x)      = ⇑.map `_        (vaᴿ x)
lamᴿ (f ∙ s)    = ⇑.map app       (lamᴿ f ,ᴿ lamᴿ s)
lamᴿ (Λ[ a ] t) = ⇑.map (Λ[ a ]_) (_ \\ᴿ lamᴿ t)
```

## Universe of syntaxes

These definitions support multi-sorted syntax with higher-order
metavariables. A `Kind` indicates the `sort` of a variable as well as
the kinds of any extra variables in scope.

```
record Kind (A : Set a) : Set a where
  inductive
  constructor _⇒_
  field
    scope : List (Kind A)
    sort  : A
open Kind
```

A syntax description over a type of sorts `A` is a function `A → Desc A`.

```
data Desc (A : Set a) : Set (lsuc a) where
  -- recursive occurrence of sort κ .sort
  -- (may bind other variables in κ .scope)
  Recᵈ : (κ : Kind A) → Desc A
  -- some data, and a subterm (which may depend on it)
  Σᵈ   : (S : Set a) (T : S → Desc A) → Desc A
  ⊤ᵈ   : Desc A
  _×ᵈ_ : (S T : Desc A) → Desc A
private variable @0 d : Desc A ; @0 D : A → Desc A
```

Example description for uλc:

```
data LamTag : Set where
  app Λ : LamTag

LamDesc : ⊤ → Desc ⊤
LamDesc tt = Σᵈ LamTag λ where
  -- two subterms, no extra bindings
  app → Recᵈ ([] ⇒ tt) ×ᵈ Recᵈ ([] ⇒ tt)
  -- one subterm, with one extra binding
  Λ   → Recᵈ ([ [] ⇒ tt ] ⇒ tt)
```

(Variables aren't mentioned because "use of variables in scope
pertains not to the specific syntax, but to the general notion of what
it is to be a syntax".)


## Interpretation of descriptions

`Tmᴰ D x ys` is a de Bruijn interpretation of the syntax of sort `x`
in the description `D`, with `ys` as bound variables. `Tmᴿ D x ys` is
the same, but co-de Bruijn encoded.

```
Spineᵈ : List (Kind A) → Desc A
Spineᵈ [] = ⊤ᵈ
Spineᵈ (z ∷ zs) = Recᵈ z ×ᵈ Spineᵈ zs

⟦_∥_⟧ᴰ : Desc A → (A → Scoped (Kind A)) → Scoped (Kind A)
⟦ Recᵈ κ ∥ R ⟧ᴰ zs = R (κ .sort) (κ .scope ++ᴸ zs)
⟦ Σᵈ S T ∥ R ⟧ᴰ zs = Σ[ s ∈ S ] (⟦ T s ∥ R ⟧ᴰ zs)
⟦ ⊤ᵈ ∥ R ⟧ᴰ zs = Lift _ ⊤
⟦ S ×ᵈ T ∥ R ⟧ᴰ zs = ⟦ S ∥ R ⟧ᴰ zs × ⟦ T ∥ R ⟧ᴰ zs

data Tmᴰ (D : A → Desc A) (x : A) : Scoped (Kind A) where
  _#$_ : ∀ {zs} → (zs ⇒ x) ∈ xs → ⟦ Spineᵈ zs ∥ Tmᴰ D ⟧ᴰ xs → Tmᴰ D x xs
  ⟨_⟩  : ⟦ D x ∥ Tmᴰ D ⟧ᴰ xs → Tmᴰ D x xs
infixl 100 _#$_

Lamᴰ′ : Scoped (Kind ⊤)
Lamᴰ′ = Tmᴰ LamDesc tt


⟦_∥_⟧ᴿ : Desc A → (A → Scoped (Kind A)) → Scoped (Kind A)
⟦ Recᵈ κ ∥ R ⟧ᴿ = κ .scope ⊢ R (κ .sort)
⟦ Σᵈ S T ∥ R ⟧ᴿ xs = Σ[ s ∈ S ] (⟦ T s ∥ R ⟧ᴿ xs)
⟦ ⊤ᵈ ∥ R ⟧ᴿ = ⊤ᴿ
⟦ S ×ᵈ T ∥ R ⟧ᴿ = ⟦ S ∥ R ⟧ᴿ ×ᴿ ⟦ T ∥ R ⟧ᴿ

data Tmᴿ (D : A → Desc A) (x : A) : Scoped (Kind A) where
  # : ∀ {zs} → (Vaᴿ (zs ⇒ x) ×ᴿ ⟦ Spineᵈ zs ∥ Tmᴿ D ⟧ᴿ) xs → Tmᴿ D x xs
  ⟨_⟩ : ⟦ D x ∥ Tmᴿ D ⟧ᴿ xs → Tmᴿ D x xs

Lamᴿ′ : Scoped (Kind ⊤)
Lamᴿ′ = Tmᴿ LamDesc tt
```

It's possible to generically convert from dB to co-dB.

```
code : Tmᴰ D x xs → Tmᴿ D x ⇑ xs
codes : ∀ S → ⟦ S ∥ Tmᴰ D ⟧ᴰ xs → ⟦ S ∥ Tmᴿ D ⟧ᴿ ⇑ xs
code (x #$ ts) = ⇑.map # (vaᴿ x ,ᴿ codes (Spineᵈ _) ts)
code ⟨ x ⟩ = ⇑.map ⟨_⟩ (codes _ x)
codes (Recᵈ κ) t = κ .scope \\ᴿ code t
codes (Σᵈ S T) (s , t) = ⇑.map (s ,_) (codes (T s) t)
codes ⊤ᵈ _ = ttᴿ
codes (S ×ᵈ T) (s , t) = codes S s ,ᴿ codes T t
```

## Hereditary substitution, generically

Hereditary substitution is a form of substitution where β-redexes are
collapsed immediately as they're created.

```
record HSub {A : Set a} D (src trg bnd : List (Kind A)) : Set a where
  constructor makeHSub
  field
    @0 {pass act} : List (Kind A)
    @0 {passive}  : pass ≼ src
    @0 {active}   : act  ≼ src
    parti   : Partition passive active
    passTrg : pass ≼ trg
    actBnd  : act  ≼ bnd
    images  : All (λ κ → κ .scope ⊢ Tmᴿ D (κ .sort) ⇑ trg) act
open HSub
private variable @0 src src′ trg trg′ bnd bnd′ : List (Kind A)

wkHSub : ∀ {xs} {@0 ys} → HSub D src trg bnd → xs ≼ ys →
         HSub D (xs ++ᴸ src) (ys ++ᴸ trg) bnd
wkHSub {xs = xs} {ys = ys} h φ =
  makeHSub (bindPassive xs) (φ ++ h .passTrg) (h .actBnd)
           (All.map (⇑.thin (empty ++ id)) (h .images))
 where
  bindPassive :
    ∀ xs → Partition (id {xs = xs} ++ h .passive) (empty ++ h .active)
  bindPassive []       = h .parti
  bindPassive (x ∷ xs) = cs′ (bindPassive xs)

record SelPart {A : Set a} {@0 xs′ ys′ zs zs′ : List A}
               {@0 θ′ : xs′ ≼ zs′} {@0 φ′ : ys′ ≼ zs′}
               (ψ : zs ≼ zs′) (p : Partition θ′ φ′) : Set a where
  constructor makeSelPart
  field
    @0 {left right} : List A
    {leftSub} : left ≼ zs
    {rightSub} : right ≼ zs
    leftUniv : left ≼ xs′
    rightUniv : right ≼ ys′
    parti : Partition leftSub rightSub
open SelPart

all-≼ : {P : A → Set b} → xs ≼ ys → All P ys → All P xs
all-≼ oz     []       = []
all-≼ (os θ) (p ∷ ps) = p ∷ all-≼ θ ps
all-≼ (o′ θ) (_ ∷ ps) = all-≼ θ ps

selPart : (ψ : zs ≼ zs′) (p : Partition θ′ φ′) → SelPart ψ p
selPart oz czz = makeSelPart oz oz czz
selPart (os ψ) (cs′ p) =
  let makeSelPart lu ru p = selPart ψ p in
  makeSelPart (os lu) ru (cs′ p)
selPart (os ψ) (c′s p) =
  let makeSelPart lu ru p = selPart ψ p in
  makeSelPart lu (os ru) (c′s p)
selPart (o′ ψ) (cs′ p) =
  let makeSelPart lu ru p = selPart ψ p in
  makeSelPart (o′ lu) ru p
selPart (o′ ψ) (c′s p) =
  let makeSelPart lu ru p = selPart ψ p in
  makeSelPart lu (o′ ru) p

selHSub : src ≼ src′ → HSub D src′ trg bnd → HSub D src trg bnd
selHSub ψ (makeHSub p pt ab i) =
  let makeSelPart lu ru p = selPart ψ p in
  makeHSub p (pt ∘′ lu) (ab ∘′ ru) (all-≼ ru i)


allLeft : {a : [] ≼ xs} {p : ys ≼ xs} → Partition p a → ys ≡ xs
allLeft czz = refl
allLeft (cs′ p) rewrite allLeft p = refl

hSub : ∀ {D x} → HSub D src trg bnd → Tmᴿ D x src → Tmᴿ D x ⇑ trg
hSubs : (S : Desc A) → HSub D src trg bnd → ⟦ S ∥ Tmᴿ D ⟧ᴿ src →
        ⟦ S ∥ Tmᴿ D ⟧ᴿ ⇑ trg
hSubs/ : (S : Desc A) → HSub D src trg bnd → ⟦ S ∥ Tmᴿ D ⟧ᴿ ⇑ src →
         ⟦ S ∥ Tmᴿ D ⟧ᴿ ⇑ trg
hered : (ys ⊢ Tmᴿ D x) ⇑ trg → [ ys ⇒ x ] ≼ bnd →
        ⟦ Spineᵈ ys ∥ Tmᴿ D ⟧ᴿ ⇑ trg → Tmᴿ D x ⇑ trg

hSub {D = D} {x = x} h ⟨ t ⟩ = ⇑.map ⟨_⟩ (hSubs (D x) h t)
hSub h (# {zs = zs} (pairᴿ (only ↑ θ) r _)) with selHSub θ h
... | makeHSub (cs′ czz) pt ab i = ⇑.map # (vaᴿ pt ,ᴿ hSubs/ _ h r)
... | makeHSub (c′s czz) pt ab (i ∷ []) = hered i ab (hSubs/ _ h r)

hSubs (Recᵈ κ) h (θ \\ T) = κ .scope \\ᴿ hSub (wkHSub h θ) T
hSubs (Σᵈ S T) h (s , t) = ⇑.map (s ,_) (hSubs (T s) h t)
hSubs ⊤ᵈ h _ = ttᴿ
hSubs (S ×ᵈ T) h (pairᴿ l r c) = hSubs/ S h l ,ᴿ hSubs/ T h r

hSubs/ S h (T ↑ θ) with selHSub θ h
hSubs/ S h (T ↑ θ) | makeHSub p pt ab [] rewrite allLeft p = T ↑ pt
hSubs/ S h (T ↑ θ) | h′@(makeHSub _ _ _ (_ ∷ _)) = hSubs S h′ T

hered i (o′ θ) s = hered i θ s
hered ((φ \\ t) ↑ ψ) (os θ) s =
  let _ , _ , c = part _ in
  hSub (makeHSub c ψ φ (all-≼ φ (spAll s))) t
 where
  spAll : {κs : List (Kind A)} → ⟦ Spineᵈ κs ∥ Tmᴿ D ⟧ᴿ ⇑ trg →
          All (λ κ → κ .scope ⊢ Tmᴿ D (κ .sort) ⇑ trg) κs
  spAll {κs = []}    (tt ↑ _) = []
  spAll {κs = _ ∷ _} (pairᴿ l r c ↑ θ) =
    ⇑.thin θ l ∷ spAll (⇑.thin θ r)
  part : (xs {ys} : List A) →
         Σ[ θ ∈ ys ≼ (xs ++ᴸ ys) ] Σ[ φ ∈ xs ≼ (xs ++ᴸ ys) ]
           Partition θ φ
  part []       = id , empty , p-id-empty _ where
    p-id-empty : ∀ ys → Partition (id {xs = ys}) (empty {xs = ys})
    p-id-empty []       = czz
    p-id-empty (y ∷ ys) = cs′ (p-id-empty ys)
  part (x ∷ xs) = let θ , φ , p = part xs in o′ θ , os φ , c′s p
```

---
header-includes: |
  <style>
    body, pre, code { font-family: PragmataPro }
    body { max-width: 60em; margin: auto }
    pre { margin-left: 2em }
    h1, h2, h3, h4, h5 {
      font-feature-settings: "ss08";
      letter-spacing: 95%
    }
  </style>
  <link rel=stylesheet href=Agda.css>
  <link rel=stylesheet href=/stylesheets/fonts.css>
...
