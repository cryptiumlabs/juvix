open import Usage as U

module Type {ℓʲ ℓʲ′ ℓᵗ ℓᵗ′ ℓᵗ″} (usages : Usages ℓʲ ℓʲ′ ℓᵗ ℓᵗ′ ℓᵗ″) where

open import Prelude
open ℕ using (_<_)
open import Relation.Binary.Construct.Closure.ReflexiveTransitive
open import Relation.Binary.PropositionalEquality

open import QTT usages
open import Eval usages

private
 variable
  n : ℕ
  u v : Universe
  x : Var n
  σ : Usageʲ
  π ρ ρ′ ζ : Usageᵗ
  R S T T′ s t : Tm n
  e f : Elim n


-- a "context" here contains only types, not usages,
-- which are handled by Skel
data Ctx : ℕ → Set ℓᵗ where
  ε : Ctx 0
  _⨟_ : (Γ : Ctx n) (S : Typ n) → Ctx (suc n)
infixl 5 _⨟_
private variable Γ : Ctx n

lookup : Ctx n → Fin n → Typ n
lookup (Γ ⨟ S) zero    = weakᵗ S
lookup (Γ ⨟ S) (suc x) = weakᵗ $ lookup Γ x


-- a "skeleton" is a sequence of usages corresponding to
-- the elements of a context
data Skel : ℕ → Set ℓᵗ where
  ε : Skel 0
  _⨟_ : (Φ : Skel n) (ρ : Usageᵗ) → Skel (suc n)
private variable Φ Φ₁ Φ₂ : Skel n


-- defining these inductively rather than just using ≡ makes them
-- easier to work with later

-- all elements are zero
data Zero : Skel n → Set (ℓᵗ ⊔ ℓᵗ′) where
  ε   : Zero ε
  _⨟_ : Zero Φ → ζ ≈ᵗ 0#ᵗ → Zero (Φ ⨟ ζ)

-- for Only σ x Φ, all elements of Φ are zero
-- except for x, which is equal to ⟦ σ ⟧
data Only : Usageʲ → Fin n → Skel n → Set (ℓᵗ ⊔ ℓᵗ′) where
  _⨟[_] : Zero Φ     → ρ ≈ᵗ ⟦ σ ⟧ → Only σ zero (Φ ⨟ ρ)
  _⨟_   : Only σ x Φ → ζ ≈ᵗ 0#ᵗ   → Only σ (suc x) (Φ ⨟ ζ)
infixl 5 _⨟[_]


-- these three are named so that a (semi)circle faces
-- sequence arguments and not scalars

-- pairwise addition
_⊕_ : Skel n → Skel n → Skel n
ε ⊕ ε = ε
(Φ₁ ⨟ ρ) ⊕ (Φ₂ ⨟ π) = Φ₁ ⊕ Φ₂ ⨟ ρ + π
infixl 6 _⊕_

-- premultiplication with a scalar
_⨵_ : Usageᵗ → Skel n → Skel n
π ⨵ ε = ε
π ⨵ (Φ ⨟ ρ) = π ⨵ Φ ⨟ π * ρ
infixl 7 _⨵_

-- postmultiplication
_⨴_ : Skel n → Usageᵗ → Skel n
_⨴_ = flip _⨵_


data _⊢_-_∋_▷_ : Ctx n → Usageʲ → Typ n → Tm n → Skel n → Set (ℓᵗ ⊔ ℓᵗ′ ⊔ ℓᵗ″)
data _⊢_-_∈_▷_ : Ctx n → Usageʲ → Elim n → Typ n → Skel n → Set (ℓᵗ ⊔ ℓᵗ′ ⊔ ℓᵗ″)
infix 0 _⊢_-_∋_▷_ _⊢_-_∈_▷_

-- the constructors are given the same names as in IGPON.
-- the seemingly-useless equalities (e.g. lookup Γ x ≡ S rather than
-- just using lookup Γ x in the conclusion), if you're unfamiliar,
-- are to avoid difficult unification problems

data _⊢_-_∋_▷_ where
  pre : T ⟿ᵗ R →
        Γ ⊢ σ - R ∋ t ▷ Φ →
        Γ ⊢ σ - T ∋ t ▷ Φ
  sort : u < v → Zero Φ →
         Γ ⊢ 0# - sort v ∋ sort u ▷ Φ
  fun : Zero (Φ ⨟ ζ) →
        Γ ⊢ 0# - sort u ∋ S ▷ Φ →
        Γ ⨟ S ⊢ 0# - sort u ∋ T ▷ Φ ⨟ ζ →
        Γ ⊢ 0# - sort u ∋ Π[ π / S ] T ▷ Φ
  lam : ρ′ ≾ᵗ ⟦ σ ⟧ * π →
        Γ ⨟ S ⊢ σ - T ∋ t ▷ Φ ⨟ ρ′ →
        Γ ⊢ σ - Π[ π / S ] T ∋ Λ t ▷ Φ
  elim : S ⩿ T →
         Γ ⊢ σ - e ∈ S ▷ Φ →
         Γ ⊢ σ - T ∋ [ e ] ▷ Φ

data _⊢_-_∈_▷_ where
  post : S ⟿ᵗ R →
         Γ ⊢ σ - e ∈ S ▷ Φ →
         Γ ⊢ σ - e ∈ R ▷ Φ
  var : lookup Γ x ≡ S → Only σ x Φ →
        Γ ⊢ σ - ` x ∈ S ▷ Φ
    -- var just uses whatever σ it's told. lam will check that it's ok later.
  app : Φ ≡ Φ₁ ⊕ π ⨵ Φ₂ →
        T′ ≡ substᵗ T (s ⦂ S) → 
        Γ ⊢ σ - f ∈ Π[ π / S ] T ▷ Φ₁ →
        Γ ⊢ σ - S ∋ s ▷ Φ₂ →
        Γ ⊢ σ - f ∙ s ∈ T′ ▷ Φ
    -- app does the multiplication in the conclusion like the QTT paper,
    -- so it's compatible with {0,1}-only judgements
  cut : Zero Φ₁ →
        Γ ⊢ 0# - sort u ∋ S ▷ Φ₁ →
        Γ ⊢ σ - S ∋ s ▷ Φ₂ →
        Γ ⊢ σ - s ⦂ S ∈ S ▷ Φ₂

pre* : T ⟿* R → Γ ⊢ σ - R ∋ t ▷ Φ → Γ ⊢ σ - T ∋ t ▷ Φ
pre* ε = id
pre* (r ◅ rs) = pre r ∘ pre* rs

post* : S ⟿* R → Γ ⊢ σ - e ∈ S ▷ Φ → Γ ⊢ σ - e ∈ R ▷ Φ
post* ε = id
post* (r ◅ rs) = post* rs ∘ post r
