open import Usage as U

module Eval {ℓʲ ℓʲ′ ℓᵗ ℓᵗ′ ℓᵗ″} (usages : Usages ℓʲ ℓʲ′ ℓᵗ ℓᵗ′ ℓᵗ″) where

open import Prelude
open import Relation.Binary.Construct.Closure.ReflexiveTransitive
open import QTT usages

private
 variable
  n : ℕ
  π : Usageᵗ
  s s′ t t′ S S′ T T′ : Term n
  e e′ f f′ : Elim n

data _⟿ᵗ_ : Rel (Term n) lzero
data _⟿ᵉ_ : Rel (Elim n) lzero
infix 1 _⟿ᵗ_ _⟿ᵉ_

data _⟿ᵗ_ where
  υ : [ t ⦂ T ] ⟿ᵗ t
  Πˡ : S ⟿ᵗ S′ → Π[ π / S ] T ⟿ᵗ Π[ π / S′ ] T
  Πʳ : T ⟿ᵗ T′ → Π[ π / S ] T ⟿ᵗ Π[ π / S ] T′
  Λ- : t ⟿ᵗ t′ → Λ t ⟿ᵗ Λ t′
  [_] : e ⟿ᵉ e′ → [ e ] ⟿ᵗ [ e′ ]

data _⟿ᵉ_ where
  β : (Λ t ⦂ Π[ π / S ] T) ∙ s ⟿ᵉ substᵉ (t ⦂ T) (s ⦂ S)
  ∙ˡ : f ⟿ᵉ f′ → f ∙ s ⟿ᵉ f′ ∙ s
  ∙ʳ : s ⟿ᵗ s′ → f ∙ s ⟿ᵉ f ∙ s′
  ⦂ˡ : s ⟿ᵗ s′ → s ⦂ S ⟿ᵉ s′ ⦂ S
  ⦂ʳ : S ⟿ᵗ S′ → s ⦂ S ⟿ᵉ s ⦂ S′


_⟿*_ : Rel (Term n) _
_⟿*_ = Star _⟿ᵗ_
infix 1 _⟿*_

_⇓ : Term n → Set _
T ⇓ = ∀ {T′} → ¬ (T ⟿ᵗ T′)
infix 10 _⇓

_⟿!_ : Rel (Term n) _
S ⟿! T = (S ⟿* T) × (T ⇓)
infix 1 _⟿!_
