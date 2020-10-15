module SynEquality where

open import Prelude
open ℕ using () renaming (_≟_ to _≟ⁿ_)
open Fin using () renaming (_≟_ to _≟ᶠ_)
open Bool using (_∧_)
open Relation using (does ; proof ; ofʸ ; ofⁿ)

open import QTT

open import Relation.Nary using (≟-mapₙ)
open import Function.Nary.NonDependent using (Injectiveₙ)

private variable n : ℕ


`⋆-inj : Injectiveₙ 1 `⋆
`⋆-inj refl = refl

`𝚷-inj : Injectiveₙ 2 $ `𝚷[_/_] {n}
`𝚷-inj refl = refl , refl

fin-inj : Injectiveₙ 3 $ fin {n}
fin-inj refl = refl , refl , refl

inf-inj : Injectiveₙ 3 $ inf {n}
inf-inj refl = refl , refl , refl

CORE-inj : Injectiveₙ 1 $ CORE {n}
CORE-inj refl = refl

BIND-inj : Injectiveₙ 2 $ BIND {n}
BIND-inj refl = refl , refl

sucᵘ-inj : Injectiveₙ 1 $ sucᵘ {n}
sucᵘ-inj refl = refl

↑-inj : Injectiveₙ 1 $ ↑_ {n}
↑-inj refl = refl

[]-inj : Injectiveₙ 1 $ [_] {n}
[]-inj refl = refl

`-inj : Injectiveₙ 1 $ `_ {n}
`-inj refl = refl

∙-inj : Injectiveₙ 2 $ _∙_ {n}
∙-inj refl = refl , refl

bin-inj : Injectiveₙ 1 $ bin {n}
bin-inj refl = refl

𝓤-elim-inj : Injectiveₙ 6 $ 𝓤-elim {n}
𝓤-elim-inj refl = refl , refl , refl , refl , refl , refl

𝓤ω-elim-inj : Injectiveₙ 5 $ 𝓤ω-elim {n}
𝓤ω-elim-inj refl = refl , refl , refl , refl , refl

⦂-inj : Injectiveₙ 2 $ _⦂_ {n}
⦂-inj refl = refl , refl


_≟ᵗ_ : Decidable₂ $ ≡-At (Term   n)
_≟ᵉ_ : Decidable₂ $ ≡-At (Elim   n)
_≟ᵏ_ : Decidable₂ $ ≡-At CoreType
_≟ᵇ_ : Decidable₂ $ ≡-At (Binder n)
_≟ᵒ_ : Decidable₂ $ ≡-At (BinOp  n)
_≟ᵖ_ : Decidable₂ $ ≡-At BinOpKind
infixl 4 _≟ᵗ_ _≟ᵉ_ _≟ᵏ_ _≟ᵇ_ _≟ᵒ_

CORE K ≟ᵗ CORE L   = ≟-mapₙ 1 CORE CORE-inj (K ≟ᵏ L)
CORE _ ≟ᵗ BIND _ _ = no λ()
CORE _ ≟ᵗ 0ᵘ       = no λ()
CORE _ ≟ᵗ sucᵘ _   = no λ()
CORE _ ≟ᵗ (↑ _)    = no λ()
CORE _ ≟ᵗ ωᵘ       = no λ()
CORE _ ≟ᵗ [ _ ]    = no λ()

BIND B s ≟ᵗ BIND C t = ≟-mapₙ 2 BIND BIND-inj (B ≟ᵇ C) (s ≟ᵗ t)
BIND _ _ ≟ᵗ CORE _   = no λ()
BIND _ _ ≟ᵗ 0ᵘ       = no λ()
BIND _ _ ≟ᵗ sucᵘ _   = no λ()
BIND _ _ ≟ᵗ (↑ _)    = no λ()
BIND _ _ ≟ᵗ ωᵘ       = no λ()
BIND _ _ ≟ᵗ [ _ ]    = no λ()

0ᵘ ≟ᵗ 0ᵘ       = yes refl
0ᵘ ≟ᵗ CORE _   = no λ()
0ᵘ ≟ᵗ BIND _ _ = no λ()
0ᵘ ≟ᵗ sucᵘ _   = no λ()
0ᵘ ≟ᵗ (↑ _)    = no λ()
0ᵘ ≟ᵗ ωᵘ       = no λ()
0ᵘ ≟ᵗ [ _ ]    = no λ()

sucᵘ π ≟ᵗ sucᵘ ρ   = ≟-mapₙ 1 sucᵘ sucᵘ-inj (π ≟ᵗ ρ)
sucᵘ _ ≟ᵗ CORE _   = no λ()
sucᵘ _ ≟ᵗ BIND _ _ = no λ()
sucᵘ _ ≟ᵗ 0ᵘ       = no λ()
sucᵘ _ ≟ᵗ (↑ _)    = no λ()
sucᵘ _ ≟ᵗ ωᵘ       = no λ()
sucᵘ _ ≟ᵗ [ _ ]    = no λ()

↑ π ≟ᵗ ↑ ρ      = ≟-mapₙ 1 ↑_ ↑-inj (π ≟ᵗ ρ)
↑ _ ≟ᵗ CORE _   = no λ()
↑ _ ≟ᵗ BIND _ _ = no λ()
↑ _ ≟ᵗ 0ᵘ       = no λ()
↑ _ ≟ᵗ sucᵘ _   = no λ()
↑ _ ≟ᵗ ωᵘ       = no λ()
↑ _ ≟ᵗ [ _ ]    = no λ()

ωᵘ ≟ᵗ CORE K   = no λ()
ωᵘ ≟ᵗ BIND B x = no λ()
ωᵘ ≟ᵗ 0ᵘ       = no λ()
ωᵘ ≟ᵗ sucᵘ π   = no λ()
ωᵘ ≟ᵗ ↑ π      = no λ()
ωᵘ ≟ᵗ ωᵘ       = yes refl
ωᵘ ≟ᵗ [ e ]    = no λ()

[ e ] ≟ᵗ [ f ]    = ≟-mapₙ 1 [_] []-inj (e ≟ᵉ f)
[ _ ] ≟ᵗ CORE _   = no λ()
[ _ ] ≟ᵗ BIND _ _ = no λ()
[ _ ] ≟ᵗ 0ᵘ       = no λ()
[ _ ] ≟ᵗ sucᵘ _   = no λ()
[ _ ] ≟ᵗ ↑ _      = no λ()
[ _ ] ≟ᵗ ωᵘ       = no λ()

`⋆ u ≟ᵏ `⋆ v = ≟-mapₙ 1 `⋆ `⋆-inj (u ≟ⁿ v)
`⋆ _ ≟ᵏ `𝓤   = no λ()
`⋆ _ ≟ᵏ `𝓤ω  = no λ()
`𝓤   ≟ᵏ `⋆ _ = no λ()
`𝓤   ≟ᵏ `𝓤   = yes refl
`𝓤   ≟ᵏ `𝓤ω  = no λ()
`𝓤ω  ≟ᵏ `⋆ _ = no λ()
`𝓤ω  ≟ᵏ `𝓤   = no λ()
`𝓤ω  ≟ᵏ `𝓤ω  = yes refl

` x ≟ᵉ ` y                = ≟-mapₙ 1 `_ `-inj (x ≟ᶠ y)
` _ ≟ᵉ _ ∙ _              = no λ()
` _ ≟ᵉ bin _              = no λ()
` _ ≟ᵉ 𝓤-elim _ _ _ _ _ _ = no λ()
` _ ≟ᵉ 𝓤ω-elim _ _ _ _ _  = no λ()
` _ ≟ᵉ _ ⦂ _              = no λ()

e ∙ s ≟ᵉ f ∙ t              = ≟-mapₙ 2 _∙_ ∙-inj (e ≟ᵉ f) (s ≟ᵗ t)
_ ∙ _ ≟ᵉ ` _                = no λ()
_ ∙ _ ≟ᵉ bin _              = no λ()
_ ∙ _ ≟ᵉ 𝓤-elim _ _ _ _ _ _ = no λ()
_ ∙ _ ≟ᵉ 𝓤ω-elim _ _ _ _ _  = no λ()
_ ∙ _ ≟ᵉ _ ⦂ _              = no λ()

bin o ≟ᵉ bin p              = ≟-mapₙ 1 bin bin-inj (o ≟ᵒ p)
bin _ ≟ᵉ ` _                = no λ()
bin _ ≟ᵉ _ ∙ _              = no λ()
bin _ ≟ᵉ 𝓤-elim _ _ _ _ _ _ = no λ()
bin _ ≟ᵉ 𝓤ω-elim _ _ _ _ _  = no λ()
bin _ ≟ᵉ _ ⦂ _              = no λ()

𝓤-elim T₁ ρ₁ ρᵀ₁ z₁ s₁ 𝜋₁ ≟ᵉ 𝓤-elim T₂ ρ₂ ρᵀ₂ z₂ s₂ 𝜋₂ =
  ≟-mapₙ 6 𝓤-elim 𝓤-elim-inj
    (T₁ ≟ᵗ T₂) (ρ₁ ≟ᵗ ρ₂) (ρᵀ₁ ≟ᵗ ρᵀ₂) (z₁ ≟ᵗ z₂) (s₁ ≟ᵗ s₂) (𝜋₁ ≟ᵉ 𝜋₂)
𝓤-elim _ _ _ _ _ _ ≟ᵉ ` _               = no λ()
𝓤-elim _ _ _ _ _ _ ≟ᵉ _ ∙ _             = no λ()
𝓤-elim _ _ _ _ _ _ ≟ᵉ bin _             = no λ()
𝓤-elim _ _ _ _ _ _ ≟ᵉ 𝓤ω-elim _ _ _ _ _ = no λ()
𝓤-elim _ _ _ _ _ _ ≟ᵉ _ ⦂ _             = no λ()

𝓤ω-elim T₁ ρ₁ d₁ w₁ 𝜋₁ ≟ᵉ 𝓤ω-elim T₂ ρ₂ d₂ w₂ 𝜋₂ =
  ≟-mapₙ 5 𝓤ω-elim 𝓤ω-elim-inj
    (T₁ ≟ᵗ T₂) (ρ₁ ≟ᵗ ρ₂) (d₁ ≟ᵗ d₂) (w₁ ≟ᵗ w₂) (𝜋₁ ≟ᵉ 𝜋₂)
𝓤ω-elim _ _ _ _ _ ≟ᵉ ` _                = no λ()
𝓤ω-elim _ _ _ _ _ ≟ᵉ _ ∙ _              = no λ()
𝓤ω-elim _ _ _ _ _ ≟ᵉ bin _              = no λ()
𝓤ω-elim _ _ _ _ _ ≟ᵉ 𝓤-elim _ _ _ _ _ _ = no λ()
𝓤ω-elim _ _ _ _ _ ≟ᵉ _ ⦂ _              = no λ()

s ⦂ S ≟ᵉ t ⦂ T              = ≟-mapₙ 2 _⦂_ ⦂-inj (s ≟ᵗ t) (S ≟ᵗ T)
_ ⦂ _ ≟ᵉ ` _                = no λ()
_ ⦂ _ ≟ᵉ _ ∙ _              = no λ()
_ ⦂ _ ≟ᵉ bin _              = no λ()
_ ⦂ _ ≟ᵉ 𝓤-elim _ _ _ _ _ _ = no λ()
_ ⦂ _ ≟ᵉ 𝓤ω-elim _ _ _ _ _  = no λ()

`𝚷[ π / S ] ≟ᵇ `𝚷[ ρ / T ] = ≟-mapₙ 2 `𝚷[_/_] `𝚷-inj (π ≟ᵗ ρ) (S ≟ᵗ T)
`𝚷[ _ / _ ] ≟ᵇ `𝛌          = no λ()
`𝛌          ≟ᵇ `𝚷[ _ / _ ] = no λ()
`𝛌          ≟ᵇ `𝛌          = yes refl

fin • 𝜋₁ ρ₁ ≟ᵒ fin ▴ 𝜋₂ ρ₂ = ≟-mapₙ 3 fin fin-inj (• ≟ᵖ ▴) (𝜋₁ ≟ᵉ 𝜋₂) (ρ₁ ≟ᵗ ρ₂)
inf • 𝜋₁ 𝜌₁ ≟ᵒ inf ▴ 𝜋₂ 𝜌₂ = ≟-mapₙ 3 inf inf-inj (• ≟ᵖ ▴) (𝜋₁ ≟ᵉ 𝜋₂) (𝜌₁ ≟ᵉ 𝜌₂)
fin • _  _  ≟ᵒ inf ▴ _  _  = no λ()
inf • _  _  ≟ᵒ fin ▴ _  _  = no λ()

`+ ≟ᵖ `+ = yes refl
`+ ≟ᵖ `* = no λ()
`* ≟ᵖ `+ = no λ()
`* ≟ᵖ `* = yes refl
