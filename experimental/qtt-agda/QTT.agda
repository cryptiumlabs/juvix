open import Usage as U

module QTT {ℓʲ ℓʲ′ ℓᵗ ℓᵗ′ ℓᵗ″} (usages : Usages ℓʲ ℓʲ′ ℓᵗ ℓᵗ′ ℓᵗ″) where

open import Algebra.Structures
open import Level using (0ℓ ; _⊔_)
open import Function
open import Data.Nat as ℕ using (ℕ ; zero ; suc)
open import Data.Fin as Fin using (Fin ; zero ; suc)
open import Data.Product hiding (Σ)
open import Relation.Binary
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary

private variable m n : ℕ

open Usages usages hiding (zero)
private variable π π′ ρ ρ′ : Usageᵗ ; σ σ′ : Usageʲ

Var = Fin
private variable x y : Var n

Universe = ℕ
private variable u v : Universe

data Tm n : Set ℓᵗ
data Elim n : Set ℓᵗ
Typ = Tm

data Tm n where
  sort : (u : Universe) → Typ n
  Π    : (π : Usageᵗ) (S : Typ n) (T : Typ (suc n)) → Typ n
  Λ    : (t : Tm (suc n)) → Tm n
  [_]  : (e : Elim n) → Tm n
private variable s s′ t t′ : Tm n ; S S′ T T′ R R′ : Typ n

data Elim n where
  `_  : (x : Var n) → Elim n
  _∙_ : (f : Elim n) (s : Tm n) → Elim n
  _⦂_ : (s : Tm n) (S : Typ n) → Elim n
infix 1000 `_ ; infixl 200 _∙_ ; infix 100 _⦂_
private variable e e′ f f′ : Elim n


data _≼_ : Rel (Typ n) 0ℓ where
  sort : u ℕ.≤ v → sort u ≼ sort {n} v
  Π    : S′ ≼ S → T ≼ T′ → Π π S T ≼ Π π S′ T′
  refl : S ≼ S
  -- (maybe recurse into other structures?)
infix 4 _≼_

weakᵗ′ : Var (suc n) → Tm n → Tm (suc n)
weakᵉ′ : Var (suc n) → Elim n → Elim (suc n)
weakᵗ′ i (sort u)  = sort u
weakᵗ′ i (Π π S T) = Π π (weakᵗ′ i S) (weakᵗ′ (suc i) T)
weakᵗ′ i (Λ t)     = Λ (weakᵗ′ (suc i) t)
weakᵗ′ i [ e ]     = [ weakᵉ′ i e ]
weakᵉ′ i (` x)     = ` Fin.punchIn i x
weakᵉ′ i (f ∙ s)   = weakᵉ′ i f ∙ weakᵗ′ i s
weakᵉ′ i (s ⦂ S)   = weakᵗ′ i s ⦂ weakᵗ′ i S

weakᵗ : Tm n → Tm (suc n)
weakᵗ = weakᵗ′ zero
weakᵉ : Elim n → Elim (suc n)
weakᵉ = weakᵉ′ zero

substᵗ′ : Var (suc n) → Tm (suc n) → Elim n → Tm n
substᵉ′ : Var (suc n) → Elim (suc n) → Elim n → Elim n
substᵗ′ i (sort u) e = sort u
substᵗ′ i (Π π S T) e = Π π (substᵗ′ i S e) (substᵗ′ (suc i) T (weakᵉ′ i e))
substᵗ′ i (Λ t) e = Λ (substᵗ′ (suc i) t (weakᵉ′ i e))
substᵗ′ i [ f ] e = [ substᵉ′ i f e ]
substᵉ′ i (` x) e =
  case i Fin.≟ x of λ{(yes _) → e ; (no i≢x) → ` Fin.punchOut i≢x}
substᵉ′ i (f ∙ s) e = substᵉ′ i f e ∙ substᵗ′ i s e
substᵉ′ i (s ⦂ S) e = substᵗ′ i s e ⦂ substᵗ′ i S e

substᵗ : Tm (suc n) → Elim n → Tm n
substᵗ = substᵗ′ zero
substᵉ : Elim (suc n) → Elim n → Elim n
substᵉ = substᵉ′ zero

data _⟿ᵗ_ : Rel (Tm n) 0ℓ
data _⟿ᵉ_ : Rel (Elim n) 0ℓ
infix 1 _⟿ᵗ_ _⟿ᵉ_

data _⟿ᵗ_ where
  υ : [ t ⦂ T ] ⟿ᵗ t
  Πˡ : S ⟿ᵗ S′ → Π π S T ⟿ᵗ Π π S′ T
  -- reducing under binders?
  Πʳ : T ⟿ᵗ T′ → Π π S T ⟿ᵗ Π π S T′
  Λ : t ⟿ᵗ t′ → Λ t ⟿ᵗ Λ t′
  [_] : e ⟿ᵉ e′ → [ e ] ⟿ᵗ [ e′ ]

data _⟿ᵉ_ where
  β : (Λ t ⦂ Π π S T) ∙ s ⟿ᵉ substᵉ (t ⦂ T) (s ⦂ S)
  ∙ˡ : f ⟿ᵉ f′ → f ∙ s ⟿ᵉ f′ ∙ s
  ∙ʳ : s ⟿ᵗ s′ → f ∙ s ⟿ᵉ f ∙ s′
  ⦂ˡ : s ⟿ᵗ s′ → s ⦂ S ⟿ᵉ s′ ⦂ S
  ⦂ʳ : S ⟿ᵗ S′ → s ⦂ S ⟿ᵉ s ⦂ S′


data Ctx : ℕ → Set ℓᵗ where
  ε : Ctx 0
  _⨟_ : (Γ : Ctx n) (S : Typ n) → Ctx (suc n)
infixl 5 _⨟_
private variable Γ : Ctx n

lookup : Ctx n → Fin n → Typ n
lookup (Γ ⨟ S) zero    = weakᵗ S
lookup (Γ ⨟ S) (suc x) = weakᵗ $ lookup Γ x


data Skel : ℕ → Set ℓᵗ where
  ε : Skel 0
  _⨟_ : (Σ : Skel n) (ρ : Usageᵗ) → Skel (suc n)
private variable Σ Σ₁ Σ₂ : Skel n

data Zero : Skel n → Set where
  ε   : Zero ε
  _⨟0 : Zero Σ → Zero (Σ ⨟ 0#ᵗ)
infixl 5 _⨟0

data Only : Usageʲ → Fin n → Skel n → Set ℓᵗ where
  _⨟_ : Zero Σ → ρ ≡ ⟦ σ ⟧ → Only σ zero (Σ ⨟ ρ)
  _⨟0 : Only σ x Σ → Only σ (suc x) (Σ ⨟ 0#ᵗ)

_⊕_ : Skel n → Skel n → Skel n
ε ⊕ ε = ε
(Σ₁ ⨟ ρ) ⊕ (Σ₂ ⨟ π) = Σ₁ ⊕ Σ₂ ⨟ ρ + π
infixl 6 _⊕_

_⨵_ : Usageᵗ → Skel n → Skel n
π ⨵ ε = ε
π ⨵ (Σ ⨟ ρ) = π ⨵ Σ ⨟ π * ρ
infixl 7 _⨵_ 


data _⊢_-_∋_▷_ : Ctx n → Usageʲ → Typ n → Tm n → Skel n → Set (ℓᵗ ⊔ ℓᵗ″)
data _⊢_-_∈_▷_ : Ctx n → Usageʲ → Elim n → Typ n → Skel n → Set (ℓᵗ ⊔ ℓᵗ″)
infix 0 _⊢_-_∋_▷_ _⊢_-_∈_▷_

data _⊢_-_∋_▷_ where
  pre : T ⟿ᵗ R →
        Γ ⊢ σ - R ∋ t ▷ Σ →
        Γ ⊢ σ - T ∋ t ▷ Σ
  sort : u ℕ.< v → Zero Σ →
         Γ ⊢ 0# - sort v ∋ sort u ▷ Σ
  fun : Zero Σ →
        Γ ⊢ 0# - sort u ∋ S ▷ Σ →
        Γ ⨟ S ⊢ 0# - sort u ∋ T ▷ Σ ⨟ 0#ᵗ →
        Γ ⊢ 0# - sort u ∋ Π π S T ▷ Σ
  lam : ρ′ ≾ᵗ ρ * π →
        Γ ⨟ S ⊢ σ - T ∋ t ▷ Σ ⨟ ρ′ →
        Γ ⊢ σ - Π π S T ∋ Λ t ▷ Σ
  elim : S ≼ T →
         Γ ⊢ σ - e ∈ S ▷ Σ →
         Γ ⊢ σ - T ∋ [ e ] ▷ Σ

data _⊢_-_∈_▷_ where
  post : Γ ⊢ σ - e ∈ S ▷ Σ → S ⟿ᵗ R →
         Γ ⊢ σ - e ∈ R ▷ Σ
  var : lookup Γ x ≡ S → Only σ x Σ →
        Γ ⊢ σ - ` x ∈ S ▷ Σ
    -- var just uses whatever σ it's told. lam will check that it's ok later.
  app : Σ ≡ Σ₁ ⊕ π ⨵ Σ₂ →
        T′ ≡ substᵗ T (s ⦂ S) → 
        Γ ⊢ σ - f ∈ Π π S T ▷ Σ₁ →
        Γ ⊢ σ - S ∋ s ▷ Σ₂ →
        Γ ⊢ σ - f ∙ s ∈ T′ ▷ Σ
  cut : Zero Σ₁ →
        Γ ⊢ 0# - sort u ∋ S ▷ Σ₁ →
        Γ ⊢ σ - S ∋ s ▷ Σ₂ →
        Γ ⊢ σ - s ⦂ S ∈ S ▷ Σ₂
