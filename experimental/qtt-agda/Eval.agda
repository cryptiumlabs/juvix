open import Usage as U

module Eval where

open import Prelude
open import Relation.Binary.Construct.Closure.ReflexiveTransitive
open import QTT

private
 variable
  n : ℕ
  s s′ t t′ z z′ w w′ : Term n
  S S′ T T′ : Type n
  π π′ ρ ρ′ : Usage n
  e e′ f f′ : Elim n

data _⟿ᵗ_ : Rel (Term n) lzero
data _⟿ᵉ_ : Rel (Elim n) lzero
infix 1 _⟿ᵗ_ _⟿ᵉ_

data _⟿ᵗ_ where
  υ : [ t ⦂ T ] ⟿ᵗ t

  𝚷₁ : π ⟿ᵗ π′ → 𝚷[ π / S ] T ⟿ᵗ 𝚷[ π′ / S  ] T
  𝚷₂ : S ⟿ᵗ S′ → 𝚷[ π / S ] T ⟿ᵗ 𝚷[ π  / S′ ] T
  𝚷₃ : T ⟿ᵗ T′ → 𝚷[ π / S ] T ⟿ᵗ 𝚷[ π  / S  ] T′

  𝛌- : t ⟿ᵗ t′ → 𝛌 t ⟿ᵗ 𝛌 t′

  sucᵘ : π ⟿ᵗ π′ → sucᵘ π ⟿ᵗ sucᵘ π′
  sucᵘ-ω : sucᵘ ωᵘ ⟿ᵗ ωᵘ {n}

  +ᵘˡ : π ⟿ᵗ π′ → π +ᵘ ρ ⟿ᵗ π′ +ᵘ ρ
  +ᵘʳ : ρ ⟿ᵗ ρ′ → π +ᵘ ρ ⟿ᵗ π  +ᵘ ρ′
  +ᵘ-0   : 0ᵘ     +ᵘ ρ ⟿ᵗ ρ
  +ᵘ-suc : sucᵘ π +ᵘ ρ ⟿ᵗ sucᵘ (π +ᵘ ρ)
  +ᵘ-ω   : ωᵘ     +ᵘ ρ ⟿ᵗ ωᵘ

  *ᵘˡ : π ⟿ᵗ π′ → π *ᵘ ρ ⟿ᵗ π′ *ᵘ ρ
  *ᵘʳ : ρ ⟿ᵗ ρ′ → π *ᵘ ρ ⟿ᵗ π  *ᵘ ρ′
  *ᵘ-0   : 0ᵘ     *ᵘ ρ      ⟿ᵗ 0ᵘ
  *ᵘ-suc : sucᵘ π *ᵘ ρ      ⟿ᵗ ρ +ᵘ (π *ᵘ ρ)
  *ᵘ-ω0  : ωᵘ     *ᵘ 0ᵘ     ⟿ᵗ 0ᵘ {n}
  *ᵘ-ωs  : ωᵘ     *ᵘ sucᵘ ρ ⟿ᵗ ωᵘ
  *ᵘ-ωω  : ωᵘ     *ᵘ ωᵘ     ⟿ᵗ ωᵘ {n}

  [_] : e ⟿ᵉ e′ → [ e ] ⟿ᵗ [ e′ ]

data _⟿ᵉ_ where
  β-∙ : (𝛌 t ⦂ 𝚷[ π / S ] T) ∙ s ⟿ᵉ substᵉ (t ⦂ T) (s ⦂ S)
  ∙ˡ : f ⟿ᵉ f′ → f ∙ s ⟿ᵉ f′ ∙ s
  ∙ʳ : s ⟿ᵗ s′ → f ∙ s ⟿ᵉ f ∙ s′

  β-𝓤0 : 𝓤-elim T z s w 0ᵘ ⟿ᵉ z ⦂ substᵗ T (0ᵘ ⦂ 𝓤)
  -- FIXME i think this is right?
  β-𝓤s : 𝓤-elim T z s w (sucᵘ π) ⟿ᵉ
         let s′ = substᵗ s (π ⦂ 𝓤) ; T′ = substᵗ T (sucᵘ π ⦂ 𝓤) in
         (s′ ⦂ T′) ∙ [ 𝓤-elim T z s w π ]
  β-𝓤ω : 𝓤-elim T z s w ωᵘ ⟿ᵉ w ⦂ substᵗ T (ωᵘ ⦂ 𝓤)
  𝓤-elim₁ : T ⟿ᵗ T′ → 𝓤-elim T z s w π ⟿ᵉ 𝓤-elim T′ z  s  w  π
  𝓤-elim₂ : z ⟿ᵗ z′ → 𝓤-elim T z s w π ⟿ᵉ 𝓤-elim T  z′ s  w  π
  𝓤-elim₃ : s ⟿ᵗ s′ → 𝓤-elim T z s w π ⟿ᵉ 𝓤-elim T  z  s′ w  π
  𝓤-elim₄ : w ⟿ᵗ w′ → 𝓤-elim T z s w π ⟿ᵉ 𝓤-elim T  z  s  w′ π
  𝓤-elim₅ : π ⟿ᵗ π′ → 𝓤-elim T z s w π ⟿ᵉ 𝓤-elim T  z  s  w  π′

  ⦂ˡ : s ⟿ᵗ s′ → s ⦂ S ⟿ᵉ s′ ⦂ S
  ⦂ʳ : S ⟿ᵗ S′ → s ⦂ S ⟿ᵉ s ⦂ S′


_⟿*_ : Rel (Term n) _
_⟿*_ = Star _⟿ᵗ_
infix 1 _⟿*_

_⇓ : Pred (Term n) _
T ⇓ = ∀ {T′} → ¬ (T ⟿ᵗ T′)
infix 10 _⇓

_⟿!_ : Rel (Term n) _
S ⟿! T = (S ⟿* T) × (T ⇓)
infix 1 _⟿!_
