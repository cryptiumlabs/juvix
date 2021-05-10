{-# OPTIONS --allow-unsolved-metas #-} -- FIXME

module Metatheory.TypeSafety where

open import Prelude
open import QTT
open import Eval
open import Type
open import Metatheory.Value

private
 variable
  n : ℕ
  t t′ : Term n
  T : Type n
  σ : Usageω n
  e e′ : Elim n
  Γ : Ctx n
  Φ : Skel n


progressᵗ : Γ ⊢ σ - T ∋ t ▷ Φ → Value   t ⊎ (∃[ t′ ] (t ⟿ᵗ t′))
progressᵉ : Γ ⊢ σ - e ∈ T ▷ Φ → Neutral e ⊎ (∃[ e′ ] (e ⟿ᵉ e′))

progressᵗ D = {!!}
progressᵉ D = {!!}


preservationᵗ : t ⟿ᵗ t′ → Γ ⊢ σ - T ∋ t ▷ Φ → Γ ⊢ σ - T  ∋ t′ ▷ Φ
preservationᵉ : e ⟿ᵉ e′ → Γ ⊢ σ - e ∈ T ▷ Φ → Γ ⊢ σ - e′ ∈ T  ▷ Φ

preservationᵗ R D = {!!}
preservationᵉ R D = {!!}
