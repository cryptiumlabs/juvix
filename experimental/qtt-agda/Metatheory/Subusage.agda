{-# OPTIONS --allow-unsolved-metas #-} -- FIXME

module Metatheory.Subusage where

open import Prelude
open import QTT
open import Type

private
 variable
  n : ℕ
  t : Term n
  T : Type n
  σ σ′ : Usageω n
  e : Elim n
  Γ : Ctx n
  Φ : Skel n

subusageᵗ : σ ⦂ 𝓤ω ≾ᵘ σ′ ⦂ 𝓤ω → Γ ⊢ σ - T ∋ t ▷ Φ → Γ ⊢ σ′ - T ∋ t ▷ Φ
subusageᵉ : σ ⦂ 𝓤ω ≾ᵘ σ′ ⦂ 𝓤ω → Γ ⊢ σ - e ∈ T ▷ Φ → Γ ⊢ σ′ - e ∈ T ▷ Φ

subusageᵗ S D = {!!}

subusageᵉ S D = {!!}
