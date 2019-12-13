open import Prelude as P hiding (Σ ; ⊤ ; _×_)
open import Relation.Binary.HeterogeneousEquality as ≅ using (_≅_ ; refl)

open import Scope

private
 variable
  𝓀 𝓀₁ 𝓀₂ ℓ : Level
  A B : Set ℓ
  x y z : A
  xs ys zs : Scope A
  S T : Scoped A 𝓀
  v : Bool

-- Sorts are indexed by a `Bool`, which says whether variables of that
-- sort are allowed.
Sort : Set₁
Sort = Bool → Set
private variable 𝒮 : Sort

-- A kind is the sort of a subterm, along with any extra variables
-- which are bound. The bound variables also have full kinds, allowing
-- "higher-order" holes.
record Kind (𝒮 : Sort) v : Set where
  constructor _⇒_
  inductive
  field
    scope : Scope (Kind 𝒮 true)
    sort  : 𝒮 v
open Kind public
infix 30 _⇒_
private variable ι κ : Kind 𝒮 v ; ιs κs : Scope (Kind 𝒮 v)

KScoped : (𝒮 : Sort) (ℓ : Level) (v : Bool) → Set (lsuc ℓ)
KScoped 𝒮 ℓ v = Scoped (Kind 𝒮 v) ℓ


data Desc′ (𝒮 : Sort) (ℓ : Level) : Set (lsuc ℓ) where
  Rec : (κ : Kind 𝒮 v) → Desc′ 𝒮 ℓ
  Σ   : (A : Set ℓ) (ℬ : A → Desc′ 𝒮 ℓ) → Desc′ 𝒮 ℓ
  ⊤   : Desc′ 𝒮 ℓ
  _×_ : (𝒟 ℰ : Desc′ 𝒮 ℓ) → Desc′ 𝒮 ℓ
syntax Σ A (λ x → B) = Σ[ x ∈ A ] B
infixr 20 _×_ Σ

Desc : (𝒮 : Sort) (ℓ : Level) → Set (lsuc ℓ)
Desc 𝒮 ℓ = ∀ {v} → 𝒮 v → Desc′ 𝒮 ℓ
private variable 𝒟 : Desc 𝒮 ℓ



record _⇑_ {A : Set ℓ} (S : Scoped A 𝓀) (κs : Scope A) : Set (ℓ ⊔ 𝓀) where
  constructor _↑_
  field
    {scope}  : Scope A
    thing    : S scope
    thinning : scope ≤ κs
open _⇑_ public
infix 10 _⇑_
infix 15 _↑_

map-⇑ : (S ↠ T) → S ⇑_ ↠ T ⇑_
map-⇑ f (s ↑ θ) = f s ↑ θ


data Vaᴿ (x : A) : Scoped A lzero where
  only : Vaᴿ x [ x ]

vaᴿ : x ∈ xs → Vaᴿ x ⇑ xs
vaᴿ θ = only ↑ θ


data ⊤ᴿ {A : Set ℓ} : Scoped A lzero where instance tt : ⊤ᴿ ◇

instance ttᴿ : ⊤ᴿ ⇑ xs
ttᴿ = tt ↑ ≤-min


record _×ᴿ_ {A : Set ℓ}
            (S : Scoped A 𝓀₁) (T : Scoped A 𝓀₂) (xs : Scope A) :
  Set (ℓ ⊔ 𝓀₁ ⊔ 𝓀₂)
 where
  constructor pair
  field
    proj₁ : S ⇑ xs
    proj₂ : T ⇑ xs
    cover : Cover true (proj₁ .thinning) (proj₂ .thinning)
open _×ᴿ_ public
infixr 10 _×ᴿ_

_,ᴿ_ : S ⇑ xs → T ⇑ xs → (S ×ᴿ T) ⇑ xs
(s ↑ θ) ,ᴿ (t ↑ φ) =
  let c = θ ⊕ φ in
  pair (s ↑ c .left) (t ↑ c .right) (c .cover) ↑ c .sub
infixr 15 _,ᴿ_


record Split {A : Set ℓ} ({xs ys} zs : Scope A)
             (ψ : xs ≤ (ys ++ zs)) :
  Set ℓ
 where
  constructor makeSplit
  field
    {first second} : Scope A
    firstSub       : first ≤ ys
    secondSub      : second ≤ zs
    scopeEq        : xs ≡ first ++ second
    subEq          : ψ ≅ firstSub +++ secondSub

split : (zs : Scope A) (ψ : xs ≤ (ys ++ zs)) → Split zs ψ
split ◇        ψ = makeSplit ψ ◇ refl refl
split (zs ⍪ z) (ψ ✓) with split zs ψ
... | makeSplit θ φ refl refl = makeSplit θ (φ ✓) refl refl
split (zs ⍪ z) (ψ ✘) with split zs ψ
... | makeSplit θ φ refl refl = makeSplit θ (φ ✘) refl refl

data _⊢_ {A : Set ℓ} (ys : Scope A) (T : Scoped A 𝓀) : Scoped A (ℓ ⊔ 𝓀) where
  _\\_ : (θ : xs ≤ ys) (t : T (zs ++ xs)) → (ys ⊢ T) zs
infix 10 _⊢_
infix 20 _\\_

_\\ᴿ_ : (ys : Scope A) (t : T ⇑ (zs ++ ys)) → (ys ⊢ T) ⇑ zs
ys \\ᴿ (t ↑ ψ) with split ys ψ
... | makeSplit θ φ refl refl = (φ \\ t) ↑ θ
infix 20 _\\ᴿ_

◇-\\ : T ⇑ xs → (◇ ⊢ T) ⇑ xs
◇-\\ = map-⇑ (◇ \\_)


Sp : Scope (Kind 𝒮 v) → Desc′ 𝒮 ℓ
Sp = foldl (λ 𝒟 κ → 𝒟 × Rec κ) ⊤

⟦_/_⟧ᴿ : Desc′ 𝒮 ℓ → (∀ {v} → 𝒮 v → KScoped 𝒮 ℓ true) → KScoped 𝒮 ℓ true
⟦ Rec κ / R ⟧ᴿ    = κ .scope ⊢ R (κ .sort)
⟦ Σ A ℬ / R ⟧ᴿ κs = P.Σ[ x ∈ A ] (⟦ ℬ x / R ⟧ᴿ κs)
⟦ ⊤     / R ⟧ᴿ κs = Lift _ (⊤ᴿ κs)
⟦ 𝒟 × ℰ / R ⟧ᴿ    = ⟦ 𝒟 / R ⟧ᴿ ×ᴿ ⟦ ℰ / R ⟧ᴿ

data Termᴿ (𝒟 : Desc 𝒮 ℓ) : 𝒮 v → KScoped 𝒮 ℓ true where
  #   : {s : 𝒮 true} → (Vaᴿ (ιs ⇒ s) ×ᴿ ⟦ Sp ιs / Termᴿ 𝒟 ⟧ᴿ) ↠ Termᴿ 𝒟 s
  ⟨_⟩ : {s : 𝒮 v}    → ⟦ 𝒟 s / Termᴿ 𝒟 ⟧ᴿ ↠ Termᴿ 𝒟 s
