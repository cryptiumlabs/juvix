{-# OPTIONS --allow-unsolved-metas #-} -- FIXME

module Metatheory.Terminates where

open import Prelude
open import QTT
open import Eval
open import Type

open import Codata.Delay


private
 variable
  n : ℕ
  t : Term n
  T : Type n
  e : Elim n
  σ : Usageω n
  Γ : Ctx n
  Φ : Skel n


evalᵗ-⇓ : Γ ⊢ σ - T ∋ t ▷ Φ → evalᵗ t ⇓
evalᵉ-⇓ : Γ ⊢ σ - e ∈ T ▷ Φ → evalᵉ e ⇓

evalᵗ-⇓ D = {!!}

evalᵉ-⇓ D = {!!}


evalᵗ! : Γ ⊢ σ - T ∋ t ▷ Φ → ∃[ t′ ] (t ⟿ᵗ! t′)
evalᵗ! = extract ∘ evalᵗ-⇓

evalᵉ! : Γ ⊢ σ - e ∈ T ▷ Φ → ∃[ e′ ] (e ⟿ᵉ! e′)
evalᵉ! = extract ∘ evalᵉ-⇓
