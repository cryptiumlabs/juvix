{-# OPTIONS --allow-unsolved-metas #-} -- FIXME

module Metatheory.Terminates where

open import Prelude
open import QTT
open import SynEquality
open import Eval
open import Type

open import Metatheory.ChurchRosser
open import Metatheory.Eval hiding (_⇓)

open import Relation.Binary.Construct.Closure.ReflexiveTransitive
open import Codata.Delay


private
 variable
  n : ℕ
  s t : Term n
  T : Type n
  e f : Elim n
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


-- since evaluation terminates on well-typed inputs it can be used to
-- check convertibility
eval-≋ᵗ : Γ ⊢ σ - T ∋ s ▷ Φ →
          Γ ⊢ σ - T ∋ t ▷ Φ →
          Dec (s ≋ᵗ t)
eval-≋ᵗ Ds Dt =
  let s′ , ss′ = evalᵗ! Ds ; t′ , tt′ = evalᵗ! Dt in
  ≟-to-≋? strongCRᵗ ss′ tt′ (s′ ≟ᵗ t′)

eval-≋ᵉ : Γ ⊢ σ - e ∈ T ▷ Φ →
          Γ ⊢ σ - f ∈ T ▷ Φ →
          Dec (e ≋ᵉ f)
eval-≋ᵉ De Df =
  let e′ , ee′ = evalᵉ! De ; f′ , ff′ = evalᵉ! Df in
  ≟-to-≋? strongCRᵉ ee′ ff′ (e′ ≟ᵉ f′)
