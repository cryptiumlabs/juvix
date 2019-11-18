module Type where

open import Prelude
open ℕ using (_<_)
open import Relation.Binary.Construct.Closure.ReflexiveTransitive
open import Relation.Binary.PropositionalEquality

open import QTT
open import Eval

private
 variable
  ℓ : Level
  n : ℕ
  u v : Universe
  x : Var n
  σ σ′ π ρ ρ′ ρᵀ ρᵀ′ ζ : Usage n
  R S S′ T T′ s Tˢ t z Tᶻ d Tᵈ w Tʷ Tᵉ : Term n
  e f : Elim n
  o : BinOp


data Ctx′ (F : ℕ → Set ℓ) : ℕ → Set ℓ where
  ε : Ctx′ F 0
  _⨟_ : (Γ : Ctx′ F n) (S : F n) → Ctx′ F (suc n)
infixl 5 _⨟_

Ctx  = Ctx′ Type
Skel = Ctx′ Usageω
private variable Γ Γ′ : Ctx n ; Φ Φ′ Φ₀ Φ₁ Φ₂ Φ₂′ : Skel n

data _‼_↦_ : (Γ : Ctx n) (x : Var n) (S : Type n) → Set where
  here  : Γ ⨟ S ‼ 0 ↦ weakᵗ S
  there : Γ ‼ x ↦ S → Γ ⨟ T ‼ suc x ↦ weakᵗ S
infix 0 _‼_↦_

_‼_ : (Γ : Ctx n) (x : Var n) → ∃ (Γ ‼ x ↦_)
(Γ ⨟ S) ‼ zero  = weakᵗ S , here
(Γ ⨟ S) ‼ suc x = Σ.map weakᵗ there $ Γ ‼ x
infix 10 _‼_

data _⟿ᶜ_ : Rel (Ctx n) lzero where
  here  : (ss : S ⟿ᵗ S′) → (Γ ⨟ S) ⟿ᶜ (Γ  ⨟ S′)
  there : (γγ : Γ ⟿ᶜ Γ′) → (Γ ⨟ S) ⟿ᶜ (Γ′ ⨟ S)

stepᶜ : (Γ : Ctx n) → Dec (∃[ Γ′ ] (Γ ⟿ᶜ Γ′))
stepᶜ ε       = no λ()
stepᶜ (Γ ⨟ S) with stepᵗ S
... | yes (_ , RS) = yes (-, here RS)
... | no  ¬RS with stepᶜ Γ
... | yes (_ , RΓ) = yes (-, there RΓ)
... | no  ¬RΓ = no λ where
  (_ , here  RS) → ¬RS (-, RS)
  (_ , there RΓ) → ¬RΓ (-, RΓ)

open module Evalᶜ = Eval.Derived (λ {n} → _⟿ᶜ_ {n}) stepᶜ public using ()
  renaming (_⟿+_ to _⟿ᶜ+_ ; _⟿*_ to _⟿ᶜ*_ ; _⟿!_ to _⟿ᶜ!_ ;
            ⟿+-At to ⟿ᶜ+-At ; ⟿*-At to ⟿ᶜ*-At ; ⟿!-At to ⟿ᶜ!-At ;
            _⇓ to _⇓ᶜ ; _≋_ to _≋ᶜ_ ; ≋-At to ≋ᶜ-At)


data Zero : (Φ : Skel n) → Set where
  ε   : Zero ε
  _⨟_ : (Z : Zero Φ) (E : ζ ≋ᵗ ↑ 0ᵘ) → Zero (Φ ⨟ ζ)

zeroᶜ : ∃ (Zero {n})
zeroᶜ {zero}  = -, ε
zeroᶜ {suc n} = -, zeroᶜ .proj₂ ⨟ Evalᵗ.≋-refl

data Only : (Φ : Skel n) (x : Var n) (π : Usageω n) → Set where
  here  : Zero Φ                  → Only (Φ ⨟ ρ) 0       (weakᵗ ρ)
  there : Only Φ x ρ → π ≋ᵗ ↑ 0ᵘ → Only (Φ ⨟ π) (suc x) (weakᵗ ρ)

data _+ᶜ_↦_ : (Φ₁ Φ₂ Φ : Skel n) → Set where
  ε   : ε +ᶜ ε ↦ ε
  _⨟_ : (A : Φ₁ +ᶜ Φ₂ ↦ Φ) (E : π +ʷ ρ ≋ᵗ σ) →
        (Φ₁ ⨟ π) +ᶜ (Φ₂ ⨟ ρ) ↦ (Φ ⨟ σ)
infix 1 _+ᶜ_↦_

_+ᶜ_ : (Φ₁ Φ₂ : Skel n) → ∃ (Φ₁ +ᶜ Φ₂ ↦_)
ε        +ᶜ ε        = -, ε
(Φ₁ ⨟ π) +ᶜ (Φ₂ ⨟ ρ) = Σ.map (_⨟ π +ʷ ρ) (_⨟ Evalᵗ.≋-refl) (Φ₁ +ᶜ Φ₂)
infix 300 _+ᶜ_


private variable π′ : Usage n

data _*ᶜ_↦_ : (π : Usageω n) (Φ₁ Φ : Skel n) → Set where
  ε    : π *ᶜ ε ↦ ε
  zero : (Z : Zero Φ) (C : chopᵗ π ≡ nothing) → π *ᶜ Φ₁ ↦ Φ
  cons : (C : chopᵗ π ≡ just π′) (M : π′ *ᶜ Φ₁ ↦ Φ) (E : π′ *ʷ ρ ≋ᵗ σ) →
         π *ᶜ (Φ₁ ⨟ ρ) ↦ (Φ ⨟ σ)
syntax cons C M E = M ⨟[ C ] E
infix 0 _*ᶜ_↦_
infixl 5 cons

_*ᶜ_ : (π : Usageω n) (Φ₁ : Skel n) → ∃ (π *ᶜ Φ₁ ↦_)
π *ᶜ ε        = -, ε
π *ᶜ (Φ₁ ⨟ ρ) with chopᵗ π | inspect chopᵗ π
π *ᶜ (Φ₁ ⨟ ρ) | just π′ | [ eq ] = -, (π′ *ᶜ Φ₁) .proj₂ ⨟[ eq ] Evalᵗ.≋-refl
π *ᶜ (Φ₁ ⨟ ρ) | nothing | [ eq ] = -, zero (zeroᶜ .proj₂) eq
infix 310 _*ᶜ_


data _≾ᵘ_ : Rel (Usageω n) lzero where
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


binOpTy : BinOp → Type n
binOpTy `+ = 𝓤
binOpTy `* = 𝓤
binOpTy `+ʷ = 𝓤ω
binOpTy `*ʷ = 𝓤ω

data _⊢_-_∋_▷_ : Ctx n → Usageω n → Type n → Term n → Skel n → Set
data _⊢_-_∈_▷_ : Ctx n → Usageω n → Elim n → Type n → Skel n → Set
infix 0 _⊢_-_∋_▷_ _⊢_-_∈_▷_

data _⊢_-_∋_▷_ where
  ty-pre-ty :
    T ⟿ᵗ+ R →
    Γ ⊢ σ - R ∋ t ▷ Φ →
    Γ ⊢ σ - T ∋ t ▷ Φ

  ty-pre-use :
    σ ⟿ᵗ+ σ′ →
    Γ ⊢ σ′ - T ∋ t ▷ Φ →
    Γ ⊢ σ  - T ∋ t ▷ Φ

  ty-pre-ctx :
    Γ ⟿ᶜ+ Γ′ →
    Γ′ ⊢ σ - T ∋ t ▷ Φ →
    Γ  ⊢ σ - T ∋ t ▷ Φ

  ty-pre-skel :
    Φ ⟿ᶜ+ Φ′ →
    Γ ⊢ σ - T ∋ t ▷ Φ′ →
    Γ ⊢ σ - T ∋ t ▷ Φ

  ty-⋆ :
    u < v →
    Zero Φ →
    Γ ⊢ 0ᵘ - ⋆ v ∋ ⋆ u ▷ Φ

  ty-𝓤 :
    Zero Φ →
    Γ ⊢ 0ᵘ - ⋆ u ∋ 𝓤 ▷ Φ

  ty-𝓤ω :
    Zero Φ →
    Γ ⊢ 0ᵘ - ⋆ u ∋ 𝓤ω ▷ Φ

  ty-𝚷 :
    Zero (Φ ⨟ ζ) →
    Γ     ⊢ 0ᵘ - 𝓤   ∋ π            ▷ Φ →
    Γ     ⊢ 0ᵘ - ⋆ u ∋ S            ▷ Φ →
    Γ ⨟ S ⊢ 0ᵘ - ⋆ u ∋ T            ▷ Φ ⨟ ζ →
    Γ     ⊢ 0ᵘ - ⋆ u ∋ 𝚷[ π / S ] T ▷ Φ

  ty-𝛌 :
    ρ′ ≾ᵘ σ *ʷ π →
    σ′ ≡ weakᵗ σ →
    Γ ⨟ S ⊢ σ′ -            T ∋   t ▷ Φ ⨟ ρ′ →
    Γ     ⊢ σ  - 𝚷[ π / S ] T ∋ 𝛌 t ▷ Φ

  ty-0ᵘ :
    Zero Φ →
    Γ ⊢ σ - 𝓤 ∋ 0ᵘ ▷ Φ

  ty-sucᵘ :
    Γ ⊢ σ - 𝓤 ∋      π ▷ Φ →
    Γ ⊢ σ - 𝓤 ∋ sucᵘ π ▷ Φ

  ty-↑ :
    Γ ⊢ σ - 𝓤 ∋    π ▷ Φ →
    Γ ⊢ σ - 𝓤ω ∋ ↑ π ▷ Φ

  ty-ωᵘ :
    Zero Φ →
    Γ ⊢ σ - 𝓤ω ∋ ωᵘ ▷ Φ

  ty-bin :
    Φ₁ +ᶜ Φ₂ ↦ Φ →
    T ≡ binOpTy o →
    Γ ⊢ σ - T ∋ π         ▷ Φ₁ →
    Γ ⊢ σ - T ∋ ρ         ▷ Φ₂ →
    Γ ⊢ σ - T ∋ π ⟪ o ⟫ ρ ▷ Φ

  ty-[] :
    S ⩿ T →
    Γ ⊢ σ - e ∈ S ▷ Φ →
    Γ ⊢ σ - T ∋ [ e ] ▷ Φ

data _⊢_-_∈_▷_ where
  ty-post-ty :
    S ⟿ᵗ+ R →
    Γ ⊢ σ - e ∈ S ▷ Φ →
    Γ ⊢ σ - e ∈ R ▷ Φ

  ty-post-use :
    σ ⟿ᵗ+ σ′ →
    Γ ⊢ σ  - e ∈ S ▷ Φ →
    Γ ⊢ σ′ - e ∈ S ▷ Φ

  ty-post-ctx :
    Γ ⟿ᶜ+ Γ′ →
    Γ  ⊢ σ - e ∈ S ▷ Φ →
    Γ′ ⊢ σ - e ∈ S ▷ Φ

  ty-post-skel :
    Φ ⟿ᶜ+ Φ′ →
    Γ ⊢ σ - e ∈ S ▷ Φ →
    Γ ⊢ σ - e ∈ S ▷ Φ′

  ty-` :
    Γ ‼ x ↦ S →
    Only Φ x σ →
    Γ ⊢ σ - ` x ∈ S ▷ Φ

  ty-∙ :
    π *ᶜ Φ₂ ↦ Φ₂′ →
    Φ₁ +ᶜ Φ₂′ ↦ Φ →
    T′ ≡ substᵗ T (s ⦂ S) →
    Γ ⊢ σ - f ∈ 𝚷[ π / S ] T ▷ Φ₁ →
    Γ ⊢ σ - S ∋ s ▷ Φ₂ →
    Γ ⊢ σ - f ∙ s ∈ T′ ▷ Φ

  ty-𝓤-elim :
    Zero Φ₀ →
    Φ₁ +ᶜ Φ₂ ↦ Φ →
    Tᶻ ≡ substᵗ T (0ᵘ ⦂ 𝓤) →
    Tˢ ≡ weakᵗ (substᵗ (weakᵗ′ 1 T) (sucᵘ (‶ 0) ⦂ 𝓤)) →
    Tᵉ ≡ substᵗ T (π ⦂ 𝓤) →
    σ′ ≡ weakᵗ (weakᵗ σ) →
    ρ′ ≡ ρ *ʷ σ →
    ρᵀ′ ≡ weakᵗ (ρᵀ *ʷ σ) →
    Γ ⨟ 𝓤     ⊢ 0ᵘ - ⋆ u ∋ T ▷ Φ₀ →
    Γ         ⊢ σ  - Tᶻ ∋ z ▷ Φ₁ →
    Γ ⨟ 𝓤 ⨟ T ⊢ σ′ - Tˢ ∋ s ▷ Φ₁ ⨟ ρ′ ⨟ ρᵀ′ →
    Γ         ⊢ σ  - 𝓤  ∋ π ▷ Φ₂ →
    Γ         ⊢ σ  - 𝓤-elim T ρ ρᵀ z s π ∈ Tᵉ ▷ Φ

  ty-𝓤ω-elim :
    Zero Φ₀ →
    Φ₁ +ᶜ Φ₂ ↦ Φ →
    Tᵈ ≡ substᵗ (weakᵗ′ 1 T) (↑ ‶ 0 ⦂ 𝓤ω) →
    Tʷ ≡ substᵗ T (ωᵘ ⦂ 𝓤ω) →
    Tᵉ ≡ substᵗ T (π  ⦂ 𝓤ω) →
    σ′ ≡ weakᵗ σ →
    ρ′ ≡ ρ *ʷ σ →
    Γ ⨟ 𝓤ω ⊢ 0ᵘ - ⋆ u ∋ T ▷ Φ₀ →
    Γ ⨟ 𝓤  ⊢ σ′ - Tᵈ ∋ d ▷ Φ₁ ⨟ ρ′ →
    Γ      ⊢ σ  - Tʷ ∋ w ▷ Φ₁ →
    Γ      ⊢ σ  - 𝓤ω ∋ π ▷ Φ₂ →
    Γ      ⊢ σ  - 𝓤ω-elim T ρ d w π ∈ Tᵉ ▷ Φ

  ty-⦂ :
    Zero Φ₁ →
    Γ ⊢ 0ᵘ - ⋆ u   ∋ S  ▷ Φ₁ →
    Γ ⊢ σ  - S     ∋ s  ▷ Φ₂ →
    Γ ⊢ σ  - s ⦂ S ∈ S′ ▷ Φ₂


pattern ty-𝛌′ C tT = ty-𝛌 C refl tT

pattern ty-bin′ P tπ tρ = ty-bin P refl tπ tρ

pattern ty-∙′ M P tf ts = ty-∙ M P refl tf ts

pattern ty-𝓤-elim′ Z P tT tz ts tπ =
  ty-𝓤-elim Z P refl refl refl refl refl refl tT tz ts tπ

pattern ty-𝓤ω-elim′ Z P tT td tw tπ =
  ty-𝓤ω-elim Z P refl refl refl refl refl tT td tw tπ
