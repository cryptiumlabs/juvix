module Metatheory.Subtyping where

open import Prelude
open import Type
open Relation

private variable n : ℕ


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
