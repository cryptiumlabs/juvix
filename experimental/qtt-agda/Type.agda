module Type where

open import Prelude
open ℕ using () renaming (_<_ to _<ᴺ_ ; _≤_ to _≤ᴺ_)
open import Relation.Binary.Construct.Closure.ReflexiveTransitive
open import Relation.Binary.PropositionalEquality

open import QTT
open import Eval

private
 variable
  𝓀 ℓ : Level
  n : ℕ
  u v : Universe
  x : Var n
  σ σ′ π ρ ρ′ ρᵀ ρᵀ′ ζ : Usage n
  R S S′ T T′ s Tˢ t z Tᶻ d Tᵈ w Tʷ Tᵉ : Term n
  e f 𝜋 𝜋′ 𝜌 𝜌′ 𝜌ᵀ′ 𝜎 𝜎′ 𝜁 : Elim n
  • : BinOpKind


-- subtyping wrt universe levels
data ⩿-At n : Rel (Type n) lzero

_⩿_ : Rel (Type n) _
_⩿_ = ⩿-At _
infix 4 _⩿_

data ⩿-At n where
  ⩿-⋆    : (uv : u ≤ᴺ v) → ⋆ u ⩿ ⋆ v
  -- contravariant in input, covariant in output
  ⩿-𝚷    : (ss : S′ ⩿ S) (tt : T ⩿ T′) → 𝚷[ π / S ] T ⩿ 𝚷[ π / S′ ] T′
  ⩿-refl : S ⩿ S
  -- (todo: maybe recurse into other structures?)


data Ctx′ (𝒯 : ℕ → Set ℓ) : ℕ → Set ℓ where
  ε : Ctx′ 𝒯 0
  _⨟_ : (Γ : Ctx′ 𝒯 n) (S : 𝒯 n) → Ctx′ 𝒯 (suc n)
infixl 5 _⨟_

Ctx  = Ctx′ Type
Skel = Ctx′ Usageωᴱ
private variable Γ Γ′ : Ctx n ; Φ Φ′ Φ₀ Φ₁ Φ₂ Φ₂′ : Skel n

data _‼_↦_ : (Γ : Ctx n) (x : Var n) (S : Type n) → Set where
  here  : Γ ⨟ S ‼ 0     ↦ weakᵗ S
  there : Γ     ‼ x     ↦ S →
          Γ ⨟ T ‼ suc x ↦ weakᵗ S
infix 0 _‼_↦_

_‼_ : (Γ : Ctx n) (x : Var n) → ∃ (Γ ‼ x ↦_)
(Γ ⨟ S) ‼ zero  = weakᵗ S , here
(Γ ⨟ S) ‼ suc x = let T , L = Γ ‼ x in weakᵗ T , there L
infix 10 _‼_

private
 module EvalCtx′
          {𝒯 : ℕ → Set}
          (⟿ˣ-At : ∀ n → Rel (𝒯 n) lzero)
          (stepˣ : ∀ {n} (t : 𝒯 n) → Dec (∃[ t′ ] (t ⟨ ⟿ˣ-At _ ⟩ t′)))
        where
  open Eval.Derived ⟿ˣ-At using () renaming (_⟿_ to _⟿ˣ_)

  private variable X X′ : 𝒯 n ; Θ Θ′ : Ctx′ 𝒯 n

  data ⟿-At : ∀ n → Rel (Ctx′ 𝒯 n) lzero

  private module E = Eval.Derived ⟿-At ; open E public

  data ⟿-At where
    here  : (RS : X ⟿ˣ X′) → (Θ ⨟ X) ⟿ (Θ  ⨟ X′)
    there : (RΓ : Θ ⟿  Θ′) → (Θ ⨟ X) ⟿ (Θ′ ⨟ X)

  step : (Θ : Ctx′ 𝒯 n) → Dec (∃[ Θ′ ] (Θ ⟿ Θ′))
  step ε       = no λ()
  step (Θ ⨟ X) with stepˣ X
  ... | yes (_ , RX) = yes $ -, here RX
  ... | no ¬RX with step Θ
  ... | yes (_ , RΘ) = yes $ -, there RΘ
  ... | no ¬RΘ = no λ where
    (_ , here  RX) → ¬RX $ -, RX
    (_ , there RΘ) → ¬RΘ $ -, RΘ

  open E.Eval step public

open EvalCtx′ public using (here ; there)

-- type contexts (contain terms)
open module Evalᶜ = EvalCtx′ ⟿ᵗ-At stepᵗ public using ()
  renaming (⟿-At to ⟿ᶜ-At ; _⟿_ to _⟿ᶜ_ ;
            _⟿+_ to _⟿ᶜ+_ ; _⟿*_ to _⟿ᶜ*_ ; _⟿!_ to _⟿ᶜ!_ ;
            ⟿+-At to ⟿ᶜ+-At ; ⟿*-At to ⟿ᶜ*-At ; ⟿!-At to ⟿ᶜ!-At ;
            _⇓ to _⇓ᶜ ; _≋_ to _≋ᶜ_ ; ≋-At to ≋ᶜ-At)

-- skeletons (contain Usageωᴱs, ie elims)
open module Evalˢ = EvalCtx′ ⟿ᵉ-At stepᵉ public using ()
  renaming (⟿-At to ⟿ˢ-At ; _⟿_ to _⟿ˢ_ ;
            _⟿+_ to _⟿ˢ+_ ; _⟿*_ to _⟿ˢ*_ ; _⟿!_ to _⟿ˢ!_ ;
            ⟿+-At to ⟿ˢ+-At ; ⟿*-At to ⟿ˢ*-At ; ⟿!-At to ⟿ˢ!-At ;
            _⇓ to _⇓ˢ ; _≋_ to _≋ˢ_ ; ≋-At to ≋ˢ-At)


data Zero : (Φ : Skel n) → Set where
  ε   : Zero ε
  _⨟_ : (Z : Zero Φ) (E : 𝜁 ≋ᵉ (↑ 0ᵘ ⦂ 𝓤ω)) → Zero (Φ ⨟ 𝜁)

zeroᶜ : Σ[ Φ ∈ Skel n ] (Zero Φ)
zeroᶜ {zero}  = ε , ε
zeroᶜ {suc n} =
  let Φ , Z = zeroᶜ {n} in
  (Φ ⨟ ↑ 0ᵘ ⦂ 𝓤ω) , (Z ⨟ Evalᵉ.≋-refl)

data Only : (Φ : Skel n) (x : Var n) (𝜌 : Usageωᴱ n) → Set where
  here  : Zero Φ                         → Only (Φ ⨟ 𝜌) 0       (weakᵉ 𝜌)
  there : Only Φ x 𝜌 → 𝜋 ≋ᵉ (↑ 0ᵘ ⦂ 𝓤ω) → Only (Φ ⨟ 𝜋) (suc x) (weakᵉ 𝜌)

data _+ᶜ_↦_ : (Φ₁ Φ₂ Φ : Skel n) → Set where
  ε   : ε +ᶜ ε ↦ ε
  _⨟_ : (A : Φ₁ +ᶜ Φ₂ ↦ Φ) (E : 𝜋 +ʷ 𝜌 ≋ᵉ 𝜎) →
        (Φ₁ ⨟ 𝜋) +ᶜ (Φ₂ ⨟ 𝜌) ↦ (Φ ⨟ 𝜎)
infix 1 _+ᶜ_↦_

_+ᶜ_ : (Φ₁ Φ₂ : Skel n) → ∃ (Φ₁ +ᶜ Φ₂ ↦_)
ε        +ᶜ ε        = ε , ε
(Φ₁ ⨟ π) +ᶜ (Φ₂ ⨟ ρ) =
  let Φ , A = Φ₁ +ᶜ Φ₂ in
  (Φ ⨟ π +ʷ ρ) , (A ⨟ Evalᵉ.≋-refl)
infix 300 _+ᶜ_


private variable π′ : Usage n

data _*ᶜ_↦_ : (𝜋 : Usageωᴱ n) (Φ₁ Φ : Skel n) → Set where
  ε    : 𝜋 *ᶜ ε ↦ ε
  zero : (Z : Zero Φ) (C : chopᵉ 𝜋 ≡ nothing) → 𝜋 *ᶜ Φ₁ ↦ Φ
  cons : (C : chopᵉ 𝜋 ≡ just 𝜋′) (M : 𝜋′ *ᶜ Φ₁ ↦ Φ) (E : 𝜋′ *ʷ 𝜌 ≋ᵉ 𝜎) →
         𝜋 *ᶜ (Φ₁ ⨟ 𝜌) ↦ (Φ ⨟ 𝜎)
syntax cons C M E = M ⨟[ C ] E
infix 0 _*ᶜ_↦_
infixl 5 cons

_*ᶜ_ : (𝜋 : Usageωᴱ n) (Φ₁ : Skel n) → ∃ (𝜋 *ᶜ Φ₁ ↦_)
𝜋 *ᶜ ε        = ε , ε
𝜋 *ᶜ (Φ₁ ⨟ 𝜌) with chopᵉ 𝜋 | inspect chopᵉ 𝜋
𝜋 *ᶜ (Φ₁ ⨟ 𝜌) | just π′ | [ eq ] =
  let Φ , M = π′ *ᶜ Φ₁ in
  (Φ ⨟ π′ *ʷ 𝜌) , (M ⨟[ eq ] Evalᵉ.≋-refl)
𝜋 *ᶜ (Φ₁ ⨟ 𝜌) | nothing | [ eq ] =
  let Φ , Z = zeroᶜ in Φ , zero Z eq
infix 310 _*ᶜ_


data _≾ᵘ_ : Rel (Usageωᴱ n) lzero where
  refl : 𝜋 ≋ᵉ 𝜌       → 𝜋 ≾ᵘ 𝜌
  -≾ω  : 𝜌 ≋ᵉ ωᵘ ⦂ 𝓤ω → 𝜋 ≾ᵘ 𝜌
infix 4 _≾ᵘ_

≾ᵘ-At : ∀ n → Rel (Usageωᴱ n) _
≾ᵘ-At _ = _≾ᵘ_


data _⊢_-_∋_▷_ : Ctx n → Usageω n → Type n → Term n → Skel n → Set
data _⊢_-_∈_▷_ : Ctx n → Usageω n → Elim n → Type n → Skel n → Set
infix 0 _⊢_-_∋_▷_ _⊢_-_∈_▷_

data _⊢_-_∋_▷_ where
  ty-pre-ty :
    T ⟿ᵗ+ R →
    Γ ⊢ σ - R ∋ t ▷ Φ →
    Γ ⊢ σ - T ∋ t ▷ Φ

  ty-pre-use :
    σ ⟿ᵗ+ σ′ →
    Γ ⊢ σ′ - T ∋ t ▷ Φ →
    Γ ⊢ σ  - T ∋ t ▷ Φ

  ty-pre-ctx :
    Γ ⟿ᶜ+ Γ′ →
    Γ′ ⊢ σ - T ∋ t ▷ Φ →
    Γ  ⊢ σ - T ∋ t ▷ Φ

  ty-pre-skel :
    Φ ⟿ˢ+ Φ′ →
    Γ ⊢ σ - T ∋ t ▷ Φ′ →
    Γ ⊢ σ - T ∋ t ▷ Φ

  ty-⋆ :
    u <ᴺ v →
    Zero Φ →
    Γ ⊢ 0ᵘ - ⋆ v ∋ ⋆ u ▷ Φ

  ty-𝓤 :
    Zero Φ →
    Γ ⊢ 0ᵘ - ⋆ u ∋ 𝓤 ▷ Φ

  ty-𝓤ω :
    Zero Φ →
    Γ ⊢ 0ᵘ - ⋆ u ∋ 𝓤ω ▷ Φ

  ty-𝚷 :
    Zero (Φ ⨟ 𝜁) →
    Γ     ⊢ 0ᵘ - 𝓤   ∋ π            ▷ Φ →
    Γ     ⊢ 0ᵘ - ⋆ u ∋ S            ▷ Φ →
    Γ ⨟ S ⊢ 0ᵘ - ⋆ u ∋ T            ▷ Φ ⨟ 𝜁 →
    Γ     ⊢ 0ᵘ - ⋆ u ∋ 𝚷[ π / S ] T ▷ Φ

  ty-𝛌 :
    𝜌′ ≾ᵘ (σ ⦂ 𝓤ω) *ʷ (π ⦂ 𝓤ω) →
    σ′ ≡ weakᵗ σ →
    Γ ⨟ S ⊢ σ′ -            T ∋   t ▷ Φ ⨟ 𝜌′ →
    Γ     ⊢ σ  - 𝚷[ π / S ] T ∋ 𝛌 t ▷ Φ

  ty-0ᵘ :
    Zero Φ →
    Γ ⊢ σ - 𝓤 ∋ 0ᵘ ▷ Φ

  ty-sucᵘ :
    Γ ⊢ σ - 𝓤 ∋      π ▷ Φ →
    Γ ⊢ σ - 𝓤 ∋ sucᵘ π ▷ Φ

  ty-↑ :
    Γ ⊢ σ - 𝓤 ∋    π ▷ Φ →
    Γ ⊢ σ - 𝓤ω ∋ ↑ π ▷ Φ

  ty-ωᵘ :
    Zero Φ →
    Γ ⊢ σ - 𝓤ω ∋ ωᵘ ▷ Φ

  ty-[] :
    S ⩿ T →
    Γ ⊢ σ - e ∈ S ▷ Φ →
    Γ ⊢ σ - T ∋ [ e ] ▷ Φ

data _⊢_-_∈_▷_ where
  ty-post-ty :
    S ⟿ᵗ+ R →
    Γ ⊢ σ - e ∈ S ▷ Φ →
    Γ ⊢ σ - e ∈ R ▷ Φ

  ty-post-use :
    σ ⟿ᵗ+ σ′ →
    Γ ⊢ σ  - e ∈ S ▷ Φ →
    Γ ⊢ σ′ - e ∈ S ▷ Φ

  ty-post-ctx :
    Γ ⟿ᶜ+ Γ′ →
    Γ  ⊢ σ - e ∈ S ▷ Φ →
    Γ′ ⊢ σ - e ∈ S ▷ Φ

  ty-post-skel :
    Φ ⟿ˢ+ Φ′ →
    Γ ⊢ σ - e ∈ S ▷ Φ →
    Γ ⊢ σ - e ∈ S ▷ Φ′

  ty-` :
    Γ ‼ x ↦ S →
    Only Φ x 𝜎 →
    Γ ⊢ [ 𝜎 ] - ` x ∈ S ▷ Φ

  ty-∙ :
    (π ⦂ 𝓤ω) *ᶜ Φ₂ ↦ Φ₂′ →
    Φ₁ +ᶜ Φ₂′ ↦ Φ →
    T′ ≡ substᵗ T (s ⦂ S) →
    Γ ⊢ σ - f ∈ 𝚷[ π / S ] T ▷ Φ₁ →
    Γ ⊢ σ - S ∋ s ▷ Φ₂ →
    Γ ⊢ σ - f ∙ s ∈ T′ ▷ Φ

  ty-bin-fin :
    Φ₁ +ᶜ Φ₂ ↦ Φ →
    Γ ⊢ σ - 𝜋 ∈ 𝓤               ▷ Φ₁ →
    Γ ⊢ σ - 𝓤 ∋ ρ               ▷ Φ₂ →
    Γ ⊢ σ - bin (fin • 𝜋 ρ) ∈ 𝓤 ▷ Φ

  ty-bin-inf :
    Φ₁ +ᶜ Φ₂ ↦ Φ →
    Γ ⊢ σ - 𝜋 ∈ 𝓤ω               ▷ Φ₁ →
    Γ ⊢ σ - 𝜌 ∈ 𝓤ω               ▷ Φ₂ →
    Γ ⊢ σ - bin (inf • 𝜋 𝜌) ∈ 𝓤ω ▷ Φ

  ty-𝓤-elim :
    Zero (Φ₀ ⨟ 𝜁) →
    Φ₁ +ᶜ Φ₂ ↦ Φ →
    Tᶻ ≡ substᵗ T (0ᵘ ⦂ 𝓤) →
    Tˢ ≡ weakᵗ (substᵗ (weakᵗ′ 1 T) (sucᵘ (‶ 0) ⦂ 𝓤)) →
    Tᵉ ≡ substᵗ T (π ⦂ 𝓤) →
    σ′ ≡ weakᵗ (weakᵗ σ) →
    𝜌′ ≡ (ρ ⦂ 𝓤ω) *ʷ (σ ⦂ 𝓤ω) →
    𝜌ᵀ′ ≡ weakᵉ ((ρᵀ ⦂ 𝓤ω) *ʷ (σ ⦂ 𝓤ω)) →
    Γ ⨟ 𝓤     ⊢ 0ᵘ - ⋆ u ∋ T  ▷ Φ₀ ⨟ 𝜁 →
    Γ         ⊢ 0ᵘ - 𝓤ω  ∋ ρ  ▷ Φ₀ →
    Γ         ⊢ 0ᵘ - 𝓤ω  ∋ ρᵀ ▷ Φ₀ →
    Γ         ⊢ σ  - Tᶻ ∋ z ▷ Φ₁ →
    Γ ⨟ 𝓤 ⨟ T ⊢ σ′ - Tˢ ∋ s ▷ Φ₁ ⨟ 𝜌′ ⨟ 𝜌ᵀ′ →
    Γ         ⊢ σ  - 𝜋  ∈ 𝓤 ▷ Φ₂ →
    Γ         ⊢ σ  - 𝓤-elim T ρ ρᵀ z s 𝜋 ∈ Tᵉ ▷ Φ

  ty-𝓤ω-elim :
    Zero (Φ₀ ⨟ 𝜁) →
    Φ₁ +ᶜ Φ₂ ↦ Φ →
    Tᵈ ≡ substᵗ (weakᵗ′ 1 T) (↑ ‶ 0 ⦂ 𝓤ω) →
    Tʷ ≡ substᵗ T (ωᵘ ⦂ 𝓤ω) →
    Tᵉ ≡ substᵗ T (π  ⦂ 𝓤ω) →
    σ′ ≡ weakᵗ σ →
    𝜌′ ≡ (ρ ⦂ 𝓤ω) *ʷ (σ ⦂ 𝓤ω) →
    Γ ⨟ 𝓤ω ⊢ 0ᵘ - ⋆ u ∋ T ▷ Φ₀ ⨟ 𝜁 →
    Γ      ⊢ 0ᵘ - 𝓤ω ∋ ρ  ▷ Φ₀ →
    Γ ⨟ 𝓤  ⊢ σ′ - Tᵈ ∋ d  ▷ Φ₁ ⨟ 𝜌′ →
    Γ      ⊢ σ  - Tʷ ∋ w  ▷ Φ₁ →
    Γ      ⊢ σ  - 𝜋  ∈ 𝓤ω ▷ Φ₂ →
    Γ      ⊢ σ  - 𝓤ω-elim T ρ d w 𝜋 ∈ Tᵉ ▷ Φ

  ty-⦂ :
    Zero Φ₁ →
    Γ ⊢ 0ᵘ - ⋆ u   ∋ S  ▷ Φ₁ →
    Γ ⊢ σ  - S     ∋ s  ▷ Φ₂ →
    Γ ⊢ σ  - s ⦂ S ∈ S′ ▷ Φ₂


pattern ty-𝛌′ C tT = ty-𝛌 C refl tT

pattern ty-∙′ M P tf ts = ty-∙ M P refl tf ts

pattern ty-𝓤-elim′ Z P tT tz ts tπ =
  ty-𝓤-elim Z P refl refl refl refl refl refl tT tz ts tπ

pattern ty-𝓤ω-elim′ Z P tT td tw tπ =
  ty-𝓤ω-elim Z P refl refl refl refl refl tT td tw tπ
