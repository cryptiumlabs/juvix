module Metatheory where

open import Prelude
open import QTT
open import Hole
open import Eval
open import Type

open Relation
open import Relation.Binary.Construct.Closure.Symmetric
open import Relation.Binary.Construct.Closure.ReflexiveTransitive as RT
open import Relation.Binary.Construct.Closure.Transitive as T

private
 variable
  n : ℕ
  s t t₁ t₂ : Term n
  e e₁ e₂ : Elim n
  B B₁ B₂ : Binder n
  𝔗 : CoreType


-- ⩿ is a partial order
module _ where
  open Relation

  ⩿-antisym : Antisymmetric _≡_ $ ⩿-At n
  ⩿-antisym (⩿-⋆ uv)  (⩿-⋆ vu)    = ≡.cong  _ (ℕ.≤-antisym uv vu)
  ⩿-antisym (⩿-𝚷 s t) (⩿-𝚷 s′ t′) = ≡.cong₂ _ (⩿-antisym s′ s) (⩿-antisym t t′)
  ⩿-antisym _         ⩿-refl      = refl
  ⩿-antisym ⩿-refl    _           = refl

  ⩿-trans : Transitive $ ⩿-At n
  ⩿-trans (⩿-⋆ uv)    (⩿-⋆ vw)    = ⩿-⋆ (ℕ.≤-trans uv vw)
  ⩿-trans (⩿-𝚷 s₁ t₁) (⩿-𝚷 s₂ t₂) = ⩿-𝚷 (⩿-trans s₂ s₁) (⩿-trans t₁ t₂)
  ⩿-trans A           ⩿-refl      = A
  ⩿-trans ⩿-refl      B           = B

  ⩿-isPO : IsPartialOrder _≡_ $ ⩿-At n
  ⩿-isPO =
    record {
      isPreorder = record {
        isEquivalence = ≡.isEquivalence ;
        reflexive = λ{refl → ⩿-refl} ;
        trans = ⩿-trans
      } ;
      antisym = ⩿-antisym
    }

  ⩿-poset : ℕ → Poset _ _ _
  ⩿-poset n = record { isPartialOrder = ⩿-isPO {n} }


module _ {𝒯 : ℕ → Set} (⟿-At : ∀ n → Rel (𝒯 n) lzero) where
  open Eval.Derived ⟿-At      

  -- weak church-rosser:
  --        X
  --     ↙   ↘
  --   X₁       X₂
  --     ↘   ↙
  --        *
  --      ∃ X′
  --
  -- strong c-r is the same but with *s on all the arrows
  --
  -- nb. _≋_ is already defined as the convergence in the bottom half
  WeakCR StrongCR : Set _
  WeakCR =
    ∀ {n} (X {X₁ X₂} : 𝒯 n) →
    X ⟿ X₁ → X ⟿ X₂ → X₁ ≋ X₂
  StrongCR =
    ∀ {n} (X {X₁ X₂} : 𝒯 n) →
    X ⟿* X₁ → X ⟿* X₂ → X₁ ≋ X₂

CORE-⇓ : CORE {n} 𝔗 ⇓ᵗ
CORE-⇓ (_ , make-cs ■ ■ ())

0ᵘ-⇓ : 0ᵘ {n} ⇓ᵗ
0ᵘ-⇓ (_ , make-cs ■ ■ ())

ωᵘ-⇓ : ωᵘ {n} ⇓ᵗ
ωᵘ-⇓ (_ , make-cs ■ ■ ())

weakCRᵗ′ : WeakCR ⟿ᵗ-At′
weakCRᵗ′ _ υ     υ     = make-≋ ε ε
weakCRᵗ′ _ +-0   +-0   = make-≋ ε ε
weakCRᵗ′ _ +-s   +-s   = make-≋ ε ε
weakCRᵗ′ _ *-0   *-0   = make-≋ ε ε
weakCRᵗ′ _ *-s   *-s   = make-≋ ε ε
weakCRᵗ′ _ +ʷ-↑  +ʷ-↑  = make-≋ ε ε
weakCRᵗ′ _ +ʷ-ωˡ +ʷ-ωˡ = make-≋ ε ε
weakCRᵗ′ _ +ʷ-ωˡ +ʷ-ωʳ = make-≋ ε ε
weakCRᵗ′ _ +ʷ-ωʳ +ʷ-ωˡ = make-≋ ε ε
weakCRᵗ′ _ +ʷ-ωʳ +ʷ-ωʳ = make-≋ ε ε
weakCRᵗ′ _ *ʷ-↑  *ʷ-↑  = make-≋ ε ε
weakCRᵗ′ _ *ʷ-0ω *ʷ-0ω = make-≋ ε ε
weakCRᵗ′ _ *ʷ-ω0 *ʷ-ω0 = make-≋ ε ε
weakCRᵗ′ _ *ʷ-sω *ʷ-sω = make-≋ ε ε
weakCRᵗ′ _ *ʷ-ωs *ʷ-ωs = make-≋ ε ε
weakCRᵗ′ _ *ʷ-ωω *ʷ-ωω = make-≋ ε ε

weakCRᵗ : WeakCR ⟿ᵗ-At
weakCRᵗ t R₁ R₂ = {!t!}

weakCRᵉ′ : WeakCR ⟿ᵉ-At′
weakCRᵉ′ _ β-∙    β-∙    = make-≋ ε ε
weakCRᵉ′ _ β-𝓤-0  β-𝓤-0  = make-≋ ε ε
weakCRᵉ′ _ β-𝓤-s  β-𝓤-s  = make-≋ ε ε
weakCRᵉ′ _ β-𝓤ω-↑ β-𝓤ω-↑ = make-≋ ε ε
weakCRᵉ′ _ β-𝓤ω-ω β-𝓤ω-ω = make-≋ ε ε

weakCRᵉ : WeakCR ⟿ᵉ-At
weakCRᵉ e R₁ R₂ = {!!}

weakCRᵇ : WeakCR ⟿ᵇ-At
weakCRᵇ B R₁ R₂ = {!!}

weakCRᶜ : WeakCR ⟿ᶜ-At
weakCRᶜ _ (here RS₁)  (here RS₂)  =
  let make-≋ RSs₁ RSs₂ = weakCRᵗ _ RS₁ RS₂ in
  make-≋ (RT.gmap _ here RSs₁) (RT.gmap _ here RSs₂)
weakCRᶜ _ (here RS)   (there RΓ)  =
  make-≋ (there RΓ ◅ ε) (here RS ◅ ε)
weakCRᶜ _ (there RΓ)  (here RS)   =
  make-≋ (here RS ◅ ε) (there RΓ ◅ ε)
weakCRᶜ _ (there RΓ₁) (there RΓ₂) =
  let make-≋ RΓs₁ RΓs₂ = weakCRᶜ _ RΓ₁ RΓ₂ in
  make-≋ (RT.gmap _ there RΓs₁) (RT.gmap _ there RΓs₂)


strongCRᵗ : StrongCR ⟿ᵗ-At
strongCRᵗ t R₁ R₂ = {!!}

strongCRᵉ : StrongCR ⟿ᵉ-At
strongCRᵉ e R₁ R₂ = {!!}

strongCRᵇ : StrongCR ⟿ᵇ-At
strongCRᵇ b R₁ R₂ = {!!}

strongCRᶜ : StrongCR ⟿ᶜ-At
strongCRᶜ b R₁ R₂ = {!!}


module ≋-Equiv {𝒯 : ℕ → Set}
               {⟿-At : ∀ n → Rel (𝒯 n) lzero}
               (strongCR : StrongCR ⟿-At) where
  open Eval.Derived ⟿-At hiding (make-≋)

  -- because of the definition of ≋ this is just strong c-r!
  --
  -- S     T     U
  --  ↘ ↙ ↘ ↙
  --    *     *
  --    V     W
  --     ↘ ↙
  --       *
  --       X -- we need this

  ≋-trans : Transitive $ ≋-At n
  ≋-trans (make-≋ SV TV) (make-≋ TW UW) =
    let make-≋ VX WX = strongCR _ TV TW in
    make-≋ (SV ◅◅ VX) (UW ◅◅ WX)

  ≋-equiv : IsEquivalence $ ≋-At n
  ≋-equiv = record { refl = ≋-refl ; sym = ≋-sym ; trans = ≋-trans }

  ≋-setoid : ℕ → Setoid _ _
  ≋-setoid n = record { isEquivalence = ≋-equiv {n} }

module ≋ᵗ-Equiv = ≋-Equiv (λ {n} → strongCRᵗ {n})
module ≋ᵉ-Equiv = ≋-Equiv (λ {n} → strongCRᵗ {n})
module ≋ᵇ-Equiv = ≋-Equiv (λ {n} → strongCRᵗ {n})
module ≋ᶜ-Equiv = ≋-Equiv (λ {n} → strongCRᶜ {n})
