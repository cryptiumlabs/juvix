{-# OPTIONS --allow-unsolved-metas #-} -- FIXME

module Metatheory.ChurchRosser where

open import Prelude
open import QTT
open import Eval
open import Type

open Relation
open import Relation.Binary.Construct.Closure.ReflexiveTransitive


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

weakCRᵗ′ : WeakCR ⟿ᵗ-At′
weakCRᵗ′ _ υ υ = make-≋ ε ε

weakCRᵗ : WeakCR ⟿ᵗ-At
weakCRᵗ t R₁ R₂ = {!t!}

weakCRᵉ′ : WeakCR ⟿ᵉ-At′
weakCRᵉ′ _ β-∙    β-∙    = make-≋ ε ε
weakCRᵉ′ _ +-0    +-0    = make-≋ ε ε
weakCRᵉ′ _ +-s    +-s    = make-≋ ε ε
weakCRᵉ′ _ *-0    *-0    = make-≋ ε ε
weakCRᵉ′ _ *-s    *-s    = make-≋ ε ε
weakCRᵉ′ _ +ʷ-↑   +ʷ-↑   = make-≋ ε ε
weakCRᵉ′ _ +ʷ-ωˡ  +ʷ-ωˡ  = make-≋ ε ε
weakCRᵉ′ _ +ʷ-ωˡ  +ʷ-ωʳ  = make-≋ ε ε
weakCRᵉ′ _ +ʷ-ωʳ  +ʷ-ωˡ  = make-≋ ε ε
weakCRᵉ′ _ +ʷ-ωʳ  +ʷ-ωʳ  = make-≋ ε ε
weakCRᵉ′ _ *ʷ-↑   *ʷ-↑   = make-≋ ε ε
weakCRᵉ′ _ *ʷ-0ω  *ʷ-0ω  = make-≋ ε ε
weakCRᵉ′ _ *ʷ-ω0  *ʷ-ω0  = make-≋ ε ε
weakCRᵉ′ _ *ʷ-sω  *ʷ-sω  = make-≋ ε ε
weakCRᵉ′ _ *ʷ-ωs  *ʷ-ωs  = make-≋ ε ε
weakCRᵉ′ _ *ʷ-ωω  *ʷ-ωω  = make-≋ ε ε
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
  make-≋ (gmap _ here RSs₁) (gmap _ here RSs₂)
weakCRᶜ _ (here RS)   (there RΓ)  =
  make-≋ (there RΓ ◅ ε) (here RS ◅ ε)
weakCRᶜ _ (there RΓ)  (here RS)   =
  make-≋ (here RS ◅ ε) (there RΓ ◅ ε)
weakCRᶜ _ (there RΓ₁) (there RΓ₂) =
  let make-≋ RΓs₁ RΓs₂ = weakCRᶜ _ RΓ₁ RΓ₂ in
  make-≋ (gmap _ there RΓs₁) (gmap _ there RΓs₂)


strongCRᵗ : StrongCR ⟿ᵗ-At
strongCRᵗ t R₁ R₂ = {!!}

strongCRᵉ : StrongCR ⟿ᵉ-At
strongCRᵉ e R₁ R₂ = {!!}

strongCRᵇ : StrongCR ⟿ᵇ-At
strongCRᵇ b R₁ R₂ = {!!}

strongCRᶜ : StrongCR ⟿ᶜ-At
strongCRᶜ b R₁ R₂ = {!!}
