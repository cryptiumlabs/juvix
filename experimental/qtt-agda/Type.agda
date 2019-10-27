open import Usage as U

module Type where

open import Prelude
open ℕ using (_<_)
open import Relation.Binary.Construct.Closure.ReflexiveTransitive
open import Relation.Binary.PropositionalEquality

open import QTT
open import Eval

private
 variable
  n : ℕ
  u v : Universe
  x : Var n
  σ π ρ ρ′ ζ : Usage n
  R S S′ T T′ s t : Term n
  e f : Elim n


data Ctx : ℕ → Set where
  ε : Ctx 0
  _⨟_ : (Γ : Ctx n) (S : Type n) → Ctx (suc n)
infixl 5 _⨟_
private variable Γ Γ′ : Ctx n

data _‼_↦_ : (Γ : Ctx n) (x : Var n) (S : Type n) → Set where
  here  : Γ ⨟ S ‼ 0 ↦ weakᵗ S
  there : Γ ‼ x ↦ S → Γ ⨟ T ‼ suc x ↦ weakᵗ S
infix 0 _‼_↦_

_‼_ : (Γ : Ctx n) (x : Var n) → ∃ (Γ ‼ x ↦_)
(Γ ⨟ S) ‼ zero  = weakᵗ S , here
(Γ ⨟ S) ‼ suc x = Σ.map weakᵗ there $ Γ ‼ x
infix 10 _‼_

Skel = Ctx
private variable Φ Φ₁ Φ₂ Φ₂′ : Skel n

data _⟿ᶜ_ : Rel (Ctx n) lzero where
  here  : (ss : S ⟿ᵗ S′) → (Γ ⨟ S) ⟿ᶜ (Γ  ⨟ S′)
  there : (γγ : Γ ⟿ᶜ Γ′) → (Γ ⨟ S) ⟿ᶜ (Γ′ ⨟ S)

open module Evalᶜ = Eval.Derived (λ {n} → _⟿ᶜ_ {n}) public using ()
  renaming (_⟿+_ to _⟿ᶜ+_ ; _⟿*_ to _⟿ᶜ*_ ; _⟿!_ to _⟿ᶜ!_ ;
            ⟿+-At to ⟿ᶜ+-At ; ⟿*-At to ⟿ᶜ*-At ; ⟿!-At to ⟿ᶜ!-At ;
            _⇓ to _⇓ᶜ ; _≋_ to _≋ᶜ_ ; ≋-At to ≋ᶜ-At)


data Zero : (Φ : Skel n) → Set where
  ε   : Zero ε
  _⨟_ : (Z : Zero Φ) (E : ζ ≋ᵗ 0ᵘ) → Zero (Φ ⨟ ζ)

zeroᶜ : ∃ (Zero {n})
zeroᶜ {zero}  = -, ε
zeroᶜ {suc n} = -, zeroᶜ .proj₂ ⨟ Evalᵗ.≋-refl

data Only : (Φ : Skel n) (x : Var n) (π : Usage n) → Set where
  here  : Zero Φ     → Only (Φ ⨟ ρ) 0       (weakᵗ ρ)
  there : Only Φ x ρ → Only (Φ ⨟ π) (suc x) (weakᵗ ρ)

data _+ᶜ_↦_ : (Φ₁ Φ₂ Φ : Skel n) → Set where
  ε   : ε +ᶜ ε ↦ ε
  _⨟_ : (A : Φ₁ +ᶜ Φ₂ ↦ Φ) (E : π +ᵘ ρ ≋ᵗ σ) →
        (Φ₁ ⨟ π) +ᶜ (Φ₂ ⨟ ρ) ↦ (Φ ⨟ σ)
infix 1 _+ᶜ_↦_

_+ᶜ_ : (Φ₁ Φ₂ : Skel n) → ∃ (Φ₁ +ᶜ Φ₂ ↦_)
ε        +ᶜ ε        = -, ε
(Φ₁ ⨟ π) +ᶜ (Φ₂ ⨟ ρ) = Σ.map (_⨟ π +ᵘ ρ) (_⨟ Evalᵗ.≋-refl) (Φ₁ +ᶜ Φ₂)
infix 300 _+ᶜ_


private variable π′ : Usage n

data _*ᶜ_↦_ : (π : Usage n) (Φ₁ Φ : Skel n) → Set where
  ε    : π *ᶜ ε ↦ ε
  zero : (Z : Zero Φ) (C : chopᵗ π ≡ nothing) → π *ᶜ Φ₁ ↦ Φ
  cons : (C : chopᵗ π ≡ just π′) (M : π′ *ᶜ Φ₁ ↦ Φ) (E : π′ *ᵘ ρ ≋ᵗ σ) →
         π *ᶜ (Φ₁ ⨟ ρ) ↦ (Φ ⨟ σ)
syntax cons C M E = M ⨟[ C ] E
infix 0 _*ᶜ_↦_
infixl 5 cons

_*ᶜ_ : (π : Usage n) (Φ₁ : Skel n) → ∃ (π *ᶜ Φ₁ ↦_)
π *ᶜ ε        = -, ε
π *ᶜ (Φ₁ ⨟ ρ) with chopᵗ π | inspect chopᵗ π
π *ᶜ (Φ₁ ⨟ ρ) | just π′ | [ eq ] = -, (π′ *ᶜ Φ₁) .proj₂ ⨟[ eq ] Evalᵗ.≋-refl
π *ᶜ (Φ₁ ⨟ ρ) | nothing | [ eq ] = -, zero (zeroᶜ .proj₂) eq
infix 310 _*ᶜ_


data _≾ᵘ_ : Rel (Usage n) lzero where
  refl : π ≋ᵗ ρ  → π ≾ᵘ ρ
  -≾ω  : ρ ≋ᵗ ωᵘ → π ≾ᵘ ρ
infix 4 _≾ᵘ_

≾ᵘ-At : ∀ n → Rel (Usage n) _
≾ᵘ-At _ = _≾ᵘ_

module _ where
  open Relation
  open Evalᵗ

  ≾ᵘ-refl : Reflexive $ ≾ᵘ-At n
  ≾ᵘ-refl = refl ≋-refl

  ≾ᵘ-antisym : Antisymmetric _≋_ $ ≾ᵘ-At n
  ≾ᵘ-antisym (refl E) (refl F) = E
  ≾ᵘ-antisym (refl E) (-≾ω V)  = E
  ≾ᵘ-antisym (-≾ω W)  (refl F) = ≋-sym F
  ≾ᵘ-antisym (-≾ω W)  (-≾ω V)  = ≋-trans V (≋-sym W)

  ≾ᵘ-trans : Transitive $ ≾ᵘ-At n
  ≾ᵘ-trans (refl E) (refl F) = refl $ ≋-trans E F
  ≾ᵘ-trans (refl _) (-≾ω V)  = -≾ω V
  ≾ᵘ-trans (-≾ω W)  (refl F) = -≾ω (≋-trans (≋-sym F) W)
  ≾ᵘ-trans (-≾ω W)  (-≾ω V)  = -≾ω V

  ≾ᵘ-isPO : IsPartialOrder _≋_ $ ≾ᵘ-At n
  ≾ᵘ-isPO =
    record {
      isPreorder = record {
        isEquivalence = ≋-isEquiv ;
        reflexive = refl ;
        trans = ≾ᵘ-trans
      } ;
      antisym = ≾ᵘ-antisym
    }

  ≾ᵘ-poset : ℕ → Poset _ _ _
  ≾ᵘ-poset n = record { isPartialOrder = ≾ᵘ-isPO {n} }


data _⊢_-_∋_▷_ : Ctx n → Usage n → Type n → Term n → Skel n → Set
data _⊢_-_∈_▷_ : Ctx n → Usage n → Elim n → Type n → Skel n → Set
infix 0 _⊢_-_∋_▷_ _⊢_-_∈_▷_

data _⊢_-_∋_▷_ where
  ty-pre : T ⟿ᵗ R →
           Γ ⊢ σ - R ∋ t ▷ Φ →
           Γ ⊢ σ - T ∋ t ▷ Φ
  ty-⋆ : u < v → Zero Φ →
         Γ ⊢ 0ᵘ - ⋆ v ∋ ⋆ u ▷ Φ
  ty-𝚷 : Zero (Φ ⨟ ζ) →
         Γ ⊢ 0ᵘ - ⋆ u ∋ S ▷ Φ →
         Γ ⨟ S ⊢ 0ᵘ - ⋆ u ∋ T ▷ Φ ⨟ ζ →
         Γ ⊢ 0ᵘ - ⋆ u ∋ 𝚷[ π / S ] T ▷ Φ
  ty-𝛌 : ρ′ ≾ᵘ σ *ᵘ π →
         Γ ⨟ S ⊢ weakᵗ σ - T ∋ t ▷ Φ ⨟ ρ′ →
         Γ ⊢ σ - 𝚷[ π / S ] T ∋ 𝛌 t ▷ Φ
  ty-[] : S ⩿ T →
          Γ ⊢ σ - e ∈ S ▷ Φ →
          Γ ⊢ σ - T ∋ [ e ] ▷ Φ

data _⊢_-_∈_▷_ where
  ty-post : S ⟿ᵗ R →
            Γ ⊢ σ - e ∈ S ▷ Φ →
            Γ ⊢ σ - e ∈ R ▷ Φ
  ty-` : Γ ‼ x ↦ S → Only Φ x σ →
         Γ ⊢ σ - ` x ∈ S ▷ Φ
    -- ty-` just uses whatever σ it's told. lam will check that it's ok later.
  ty-∙ : π *ᶜ Φ₂ ↦ Φ₂′ →
         Φ₁ +ᶜ Φ₂′ ↦ Φ →
         T′ ≡ substᵗ T (s ⦂ S) →
         Γ ⊢ σ - f ∈ 𝚷[ π / S ] T ▷ Φ₁ →
         Γ ⊢ σ - S ∋ s ▷ Φ₂ →
         Γ ⊢ σ - f ∙ s ∈ T′ ▷ Φ
    -- ty-∙ does the multiplication in the conclusion like the QTT paper,
    -- so it's compatible with {0,1}-only judgements
  ty-⦂ : Zero Φ₁ →
         Γ ⊢ 0ᵘ - ⋆ u ∋ S ▷ Φ₁ →
         Γ ⊢ σ - S ∋ s ▷ Φ₂ →
         Γ ⊢ σ - s ⦂ S ∈ S ▷ Φ₂
