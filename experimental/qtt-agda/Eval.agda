module Eval where

open import Prelude
open import QTT

open import Relation.Binary.Construct.Closure.ReflexiveTransitive as RT
  using (Star ; ε ; _◅_ ; _◅◅_)
open import Relation.Binary.Construct.Closure.Transitive as T
  using (Plus′ ; [_] ; _∷_)
open import Relation.Binary.Construct.Closure.Symmetric as S
  using (SymClosure ; fwd ; bwd)
open import Relation.Binary.Construct.Union as U

open import Codata.Thunk using (Thunk ; force)
open import Codata.Delay as Delay using (Delay ; now ; later)


private
 variable
  n n′ : ℕ
  s s′ t t′ z z′ d d′ w w′ : Term n
  S S′ T T′ U U′ : Type n
  π π′ ρ ρ′ ρᵀ ρᵀ′ : Usage n
  e e′ f f′ : Elim n
  𝔅 𝔅′ : Binder n
  o : BinOp

data _⟿ᵗ_ : Rel (Term n) lzero
data _⟿ᵉ_ : Rel (Elim n) lzero
data _⟿ᵇ_ : Rel (Binder n) lzero
infix 1 _⟿ᵗ_ _⟿ᵉ_

data _⟿ᵗ_ where
  υ : [ t ⦂ T ] ⟿ᵗ t

  BIND-𝔅 : 𝔅 ⟿ᵇ 𝔅′ → BIND 𝔅 t ⟿ᵗ BIND 𝔅′ t
  BIND-t : t ⟿ᵗ t′ → BIND 𝔅 t ⟿ᵗ BIND 𝔅  t′

  sucᵘ : π ⟿ᵗ π′ → sucᵘ π ⟿ᵗ sucᵘ π′

  ↑- : π ⟿ᵗ π′ → ↑ π ⟿ᵗ ↑ π′

  binˡ : s ⟿ᵗ s′ → s ⟪ o ⟫ t ⟿ᵗ s′ ⟪ o ⟫ t
  binʳ : t ⟿ᵗ t′ → s ⟪ o ⟫ t ⟿ᵗ s  ⟪ o ⟫ t′

  +-0 : 0ᵘ     + ρ ⟿ᵗ ρ
  +-s : sucᵘ π + ρ ⟿ᵗ sucᵘ (π + ρ)

  *-0 : 0ᵘ     * ρ ⟿ᵗ 0ᵘ
  *-s : sucᵘ π * ρ ⟿ᵗ π * ρ + ρ

  +ʷ-↑  : ↑ π +ʷ ↑ ρ ⟿ᵗ ↑ (π + ρ)
  +ʷ-ωˡ : ωᵘ  +ʷ ρ   ⟿ᵗ ωᵘ
  +ʷ-ωʳ : π   +ʷ ωᵘ  ⟿ᵗ ωᵘ

  *ʷ-↑  : ↑ π      *ʷ ↑ ρ      ⟿ᵗ ↑ (π * ρ)
  *ʷ-0ω : ↑ 0ᵘ     *ʷ ωᵘ       ⟿ᵗ ↑ 0ᵘ {n}
  *ʷ-ω0 : ωᵘ       *ʷ ↑ 0ᵘ     ⟿ᵗ ↑ 0ᵘ {n}
  *ʷ-sω : ↑ sucᵘ π *ʷ ωᵘ       ⟿ᵗ ωᵘ
  *ʷ-ωs : ωᵘ       *ʷ ↑ sucᵘ π ⟿ᵗ ωᵘ
  *ʷ-ωω : ωᵘ       *ʷ ωᵘ       ⟿ᵗ ωᵘ {n}

  [_] : e ⟿ᵉ e′ → [ e ] ⟿ᵗ [ e′ ]

data _⟿ᵉ_ where
  β-∙ : (𝛌 t ⦂ 𝚷[ π / S ] T) ∙ s ⟿ᵉ substᵉ (t ⦂ T) (s ⦂ S)
  ∙ˡ : f ⟿ᵉ f′ → f ∙ s ⟿ᵉ f′ ∙ s
  ∙ʳ : s ⟿ᵗ s′ → f ∙ s ⟿ᵉ f ∙ s′

  β-𝓤-0 : 𝓤-elim T ρ ρᵀ z s 0ᵘ ⟿ᵉ z ⦂ substᵗ T (0ᵘ ⦂ 𝓤)
  β-𝓤-s : 𝓤-elim T ρ ρᵀ z s (sucᵘ π) ⟿ᵉ
    let s′ = substᵗ (substᵗ s (weakᵗ π ⦂ 𝓤)) (𝓤-elim T ρ ρᵀ z s π)
        T′ = substᵗ T (sucᵘ π ⦂ 𝓤) in
    s′ ⦂ T′
  𝓤-elim-T  : T  ⟿ᵗ T′  → 𝓤-elim T ρ ρᵀ z s π ⟿ᵉ 𝓤-elim T′ ρ  ρᵀ  z  s  π
  𝓤-elim-ρ  : ρ  ⟿ᵗ ρ′  → 𝓤-elim T ρ ρᵀ z s π ⟿ᵉ 𝓤-elim T  ρ′ ρᵀ  z  s  π
  𝓤-elim-ρᵀ : ρᵀ ⟿ᵗ ρᵀ′ → 𝓤-elim T ρ ρᵀ z s π ⟿ᵉ 𝓤-elim T  ρ  ρᵀ′ z  s  π
  𝓤-elim-z  : z  ⟿ᵗ z′  → 𝓤-elim T ρ ρᵀ z s π ⟿ᵉ 𝓤-elim T  ρ  ρᵀ  z′ s  π
  𝓤-elim-s  : s  ⟿ᵗ s′  → 𝓤-elim T ρ ρᵀ z s π ⟿ᵉ 𝓤-elim T  ρ  ρᵀ  z  s′ π
  𝓤-elim-π  : π  ⟿ᵗ π′  → 𝓤-elim T ρ ρᵀ z s π ⟿ᵉ 𝓤-elim T  ρ  ρᵀ  z  s  π′

  β-𝓤ω-↑ : 𝓤ω-elim T ρ d w (↑ π) ⟿ᵉ substᵗ d (π ⦂ 𝓤) ⦂ substᵗ T (↑ π ⦂ 𝓤ω)
  β-𝓤ω-ω : 𝓤ω-elim T ρ d w ωᵘ    ⟿ᵉ w                ⦂ substᵗ T (ωᵘ  ⦂ 𝓤ω)
  𝓤ω-elim-T : T ⟿ᵗ T′ → 𝓤ω-elim T ρ d w π ⟿ᵉ 𝓤ω-elim T′ ρ  d  w  π
  𝓤ω-elim-ρ : ρ ⟿ᵗ ρ′ → 𝓤ω-elim T ρ d w π ⟿ᵉ 𝓤ω-elim T  ρ′ d  w  π
  𝓤ω-elim-d : d ⟿ᵗ d′ → 𝓤ω-elim T ρ d w π ⟿ᵉ 𝓤ω-elim T  ρ  d′ w  π
  𝓤ω-elim-w : w ⟿ᵗ w′ → 𝓤ω-elim T ρ d w π ⟿ᵉ 𝓤ω-elim T  ρ  d  w′ π
  𝓤ω-elim-π : π ⟿ᵗ π′ → 𝓤ω-elim T ρ d w π ⟿ᵉ 𝓤ω-elim T  ρ  d  w  π′

  ⦂ˡ : s ⟿ᵗ s′ → s ⦂ S ⟿ᵉ s′ ⦂ S
  ⦂ʳ : S ⟿ᵗ S′ → s ⦂ S ⟿ᵉ s  ⦂ S′

data _⟿ᵇ_ where
  `𝚷-π : π ⟿ᵗ π′ → `𝚷[ π / S ] ⟿ᵇ `𝚷[ π′ / S  ]
  `𝚷-S : S ⟿ᵗ S′ → `𝚷[ π / S ] ⟿ᵇ `𝚷[ π  / S′ ]


private
  data Is-0   : Term n → Set where is-0   : Is-0   $ 0ᵘ {n}
  data Is-suc : Term n → Set where is-suc : Is-suc $ sucᵘ π
  data Is-ω   : Term n → Set where is-ω   : Is-ω   $ ωᵘ {n}
  data Is-↑   : Term n → Set where is-↑   : Is-↑   $ ↑ π

  data IsUsage : Term n → Set where
    is-0   : IsUsage $ 0ᵘ {n}
    is-suc : IsUsage $ sucᵘ π

  data IsUsageω : Term n → Set where
    is-↑ : IsUsageω $ ↑ π
    is-ω : IsUsageω $ ωᵘ {n}

  isUsage? : Decidable₁ $ IsUsage {n}
  isUsage? (CORE _)    = no λ()
  isUsage? (BIND _ _)  = no λ()
  isUsage? (_ ⟪ _ ⟫ _) = no λ()
  isUsage? 0ᵘ          = yes is-0
  isUsage? (sucᵘ _)    = yes is-suc
  isUsage? (↑ _)       = no λ()
  isUsage? ωᵘ          = no λ()
  isUsage? [ _ ]       = no λ()

  isUsageω? : Decidable₁ $ IsUsageω {n}
  isUsageω? (CORE _)    = no λ()
  isUsageω? (BIND _ _)  = no λ()
  isUsageω? (_ ⟪ _ ⟫ _) = no λ()
  isUsageω? 0ᵘ          = no λ()
  isUsageω? (sucᵘ _)    = no λ()
  isUsageω? (↑ _)       = yes is-↑
  isUsageω? ωᵘ          = yes is-ω
  isUsageω? [ _ ]       = no λ()

  is-0? : Decidable₁ $ Is-0 {n}
  is-0? s with isUsage? s
  ... | yes is-0   = yes is-0
  ... | yes is-suc = no λ()
  ... | no  ¬u     = no λ{is-0 → ¬u is-0}

  is-suc? : Decidable₁ $ Is-suc {n}
  is-suc? s with isUsage? s
  ... | yes is-0   = no λ()
  ... | yes is-suc = yes is-suc
  ... | no  ¬u     = no λ{is-suc → ¬u is-suc}

  is-ω? : Decidable₁ $ Is-ω {n}
  is-ω? s with isUsageω? s
  ... | yes is-↑ = no λ()
  ... | yes is-ω = yes is-ω
  ... | no  ¬u   = no λ{is-ω → ¬u is-ω}

  is-↑? : Decidable₁ $ Is-↑ {n}
  is-↑? s with isUsageω? s
  ... | yes is-↑ = yes is-↑
  ... | yes is-ω = no λ()
  ... | no  ¬u   = no λ{is-↑ → ¬u is-↑}

  isTypeAnn? : (e : Elim n) → Dec $ ∃[ s ] ∃[ S ] (e ≡ s ⦂ S)
  isTypeAnn? (` _)                = no λ()
  isTypeAnn? (_ ∙ _)              = no λ()
  isTypeAnn? (𝓤-elim _ _ _ _ _ _) = no λ()
  isTypeAnn? (𝓤ω-elim _ _ _ _ _)  = no λ()
  isTypeAnn? (s ⦂ S)              = yes (s , S , refl)

  isTyLam? : (e : Elim n) →
             Dec (∃[ s ] ∃[ π ] ∃[ S ] ∃[ T ] (e ≡ 𝛌 s ⦂ 𝚷[ π / S ] T))
  isTyLam? (` _)                = no λ()
  isTyLam? (_ ∙ _)              = no λ()
  isTyLam? (𝓤-elim _ _ _ _ _ _) = no λ()
  isTyLam? (𝓤ω-elim _ _ _ _ _)  = no λ()
  isTyLam? (CORE _ ⦂ _)         = no λ()
  isTyLam? (𝚷[ _ / _ ] _ ⦂ _)   = no λ()
  isTyLam? (𝛌 _ ⦂ CORE _)       = no λ()
  isTyLam? (𝛌 _ ⦂ _ ⟪ _ ⟫ _)    = no λ()
  isTyLam? (𝛌 s ⦂ 𝚷[ π / S ] T) = yes (s , π , S , T , refl)
  isTyLam? (𝛌 _ ⦂ 𝛌 _)          = no λ()
  isTyLam? (𝛌 _ ⦂ 0ᵘ)           = no λ()
  isTyLam? (𝛌 _ ⦂ sucᵘ _)       = no λ()
  isTyLam? (𝛌 _ ⦂ ↑ _)          = no λ()
  isTyLam? (𝛌 _ ⦂ ωᵘ)           = no λ()
  isTyLam? (𝛌 _ ⦂ [ _ ])        = no λ()
  isTyLam? (_ ⟪ _ ⟫ _ ⦂ _)      = no λ()
  isTyLam? (0ᵘ ⦂ _)             = no λ()
  isTyLam? (sucᵘ _ ⦂ _)         = no λ()
  isTyLam? (↑ _ ⦂ _)            = no λ()
  isTyLam? (ωᵘ ⦂ _)             = no λ()
  isTyLam? ([ _ ] ⦂ _)          = no λ()

  data Are-+ʷ : Usage n → Usage n → Set where
    ↑↑ : Are-+ʷ (↑ π) (↑ ρ)
    ω- : Are-+ʷ ωᵘ    ρ
    -ω : Are-+ʷ π     ωᵘ

  are-+ʷ? : Decidable₂ $ Are-+ʷ {n}
  are-+ʷ? π ρ with isUsageω? π | isUsageω? ρ
  ... | yes is-↑ | yes is-↑ = yes ↑↑
  ... | yes is-↑ | yes is-ω = yes -ω
  ... | yes is-↑ | no ¬uρ   = no λ{↑↑ → ¬uρ is-↑ ; -ω → ¬uρ is-ω}
  ... | yes is-ω | _        = yes ω-
  ... | no ¬uπ   | yes is-↑ = no λ{↑↑ → ¬uπ is-↑ ; ω- → ¬uπ is-ω}
  ... | no _     | yes is-ω = yes -ω
  ... | no ¬uπ   | no ¬uρ   =
    no λ{↑↑ → ¬uρ is-↑ ; ω- → ¬uπ is-ω ; -ω → ¬uρ is-ω}

  data Are-*ʷ : Usage n → Usage n → Set where
    ↑↑ : Are-*ʷ     (↑ π)      (↑ ρ)
    0ω : Are-*ʷ {n} (↑ 0ᵘ)     ωᵘ
    ω0 : Are-*ʷ {n} ωᵘ         (↑ 0ᵘ)
    sω : Are-*ʷ     (↑ sucᵘ π) ωᵘ
    ωs : Are-*ʷ     ωᵘ         (↑ sucᵘ ρ)
    ωω : Are-*ʷ {n} ωᵘ         ωᵘ

  are-*ʷ? : Decidable₂ $ Are-*ʷ {n}
  are-*ʷ? π ρ with isUsageω? π | isUsageω? ρ
  are-*ʷ? _ _ | yes is-↑ | yes is-↑ = yes ↑↑
  are-*ʷ? _ _ | yes (is-↑ {π = π}) | yes is-ω with isUsage? π
  are-*ʷ? _ _ | yes is-↑ | yes is-ω | yes is-0 = yes 0ω
  are-*ʷ? _ _ | yes is-↑ | yes is-ω | yes is-suc = yes sω
  are-*ʷ? _ _ | yes is-↑ | yes is-ω | no ¬uπ = no λ where
    0ω → ¬uπ is-0
    sω → ¬uπ is-suc
  are-*ʷ? _ _ | yes is-ω | yes (is-↑ {π = ρ}) with isUsage? ρ
  are-*ʷ? _ _ | yes is-ω | yes is-↑ | yes is-0 = yes ω0
  are-*ʷ? _ _ | yes is-ω | yes is-↑ | yes is-suc = yes ωs
  are-*ʷ? _ _ | yes is-ω | yes is-↑ | no ¬uρ = no λ where
    ω0 → ¬uρ is-0
    ωs → ¬uρ is-suc
  are-*ʷ? _ _ | yes is-ω | yes is-ω = yes ωω
  are-*ʷ? _ _ | yes is-↑ | no ¬uρ = no λ where
    ↑↑ → ¬uρ is-↑
    0ω → ¬uρ is-ω
    sω → ¬uρ is-ω
  are-*ʷ? _ _ | yes is-ω | no ¬p = no λ where
    ω0 → ¬p is-↑
    ωs → ¬p is-↑
    ωω → ¬p is-ω
  are-*ʷ? _ _ | no ¬p | _ = no λ where
    ↑↑ → ¬p is-↑
    0ω → ¬p is-↑
    ω0 → ¬p is-ω
    sω → ¬p is-↑
    ωs → ¬p is-ω
    ωω → ¬p is-ω

stepᵗ : (t : Term n)   → Dec (∃[ t′ ] (t ⟿ᵗ t′))
stepᵉ : (e : Elim n)   → Dec (∃[ e′ ] (e ⟿ᵉ e′))
stepᵇ : (𝔅 : Binder n) → Dec (∃[ 𝔅′ ] (𝔅 ⟿ᵇ 𝔅′))

stepᵗ (CORE _) = no λ()

stepᵗ (BIND 𝔅 t) with stepᵇ 𝔅
... | yes (_ , R𝔅) = yes (-, BIND-𝔅 R𝔅)
... | no  ¬R𝔅 with stepᵗ t
... | yes (_ , Rt) = yes (-, BIND-t Rt)
... | no  ¬Rt = no λ where
  (_ , BIND-𝔅 R𝔅) → ¬R𝔅 (-, R𝔅)
  (_ , BIND-t Rt) → ¬Rt (-, Rt)

stepᵗ (π + ρ) with isUsage? π
... | yes is-0   = yes (-, +-0)
... | yes is-suc = yes (-, +-s)
... | no  ¬uπ with stepᵗ π
... | yes (_ , Rπ) = yes (-, binˡ Rπ)
... | no  ¬Rπ with stepᵗ ρ
... | yes (_ , Rρ) = yes (-, binʳ Rρ)
... | no  ¬Rρ = no λ where
  (_ , binˡ Rπ) → ¬Rπ (-, Rπ)
  (_ , binʳ Rρ) → ¬Rρ (-, Rρ)
  (_ , +-0)   → ¬uπ is-0
  (_ , +-s)   → ¬uπ is-suc

stepᵗ (π * ρ) with isUsage? π
... | yes is-0   = yes (-, *-0)
... | yes is-suc = yes (-, *-s)
... | no  ¬uπ with stepᵗ π
... | yes (_ , Rπ) = yes (-, binˡ Rπ)
... | no  ¬Rπ with stepᵗ ρ
... | yes (_ , Rρ) = yes (-, binʳ Rρ)
... | no  ¬Rρ = no λ where
  (_ , binˡ R) → ¬Rπ (-, R)
  (_ , binʳ R) → ¬Rρ (-, R)
  (_ , *-0)  → ¬uπ is-0
  (_ , *-s)  → ¬uπ is-suc

stepᵗ (π +ʷ ρ) with are-+ʷ? π ρ
... | yes ↑↑ = yes (-, +ʷ-↑)
... | yes ω- = yes (-, +ʷ-ωˡ)
... | yes -ω = yes (-, +ʷ-ωʳ)
... | no ¬+ with stepᵗ π
... | yes (_ , Rπ) = yes (-, binˡ Rπ)
... | no  ¬Rπ with stepᵗ ρ
... | yes (_ , Rρ) = yes (-, binʳ Rρ)
... | no  ¬Rρ = no λ where
  (_ , +ʷ-↑)  → ¬+  ↑↑
  (_ , +ʷ-ωˡ) → ¬+  ω-
  (_ , +ʷ-ωʳ) → ¬+  -ω
  (_ , binˡ R) → ¬Rπ (-, R)
  (_ , binʳ R) → ¬Rρ (-, R)

stepᵗ (π *ʷ ρ) with are-*ʷ? π ρ
... | yes ↑↑ = yes (-, *ʷ-↑)
... | yes 0ω = yes (-, *ʷ-0ω)
... | yes ω0 = yes (-, *ʷ-ω0)
... | yes sω = yes (-, *ʷ-sω)
... | yes ωs = yes (-, *ʷ-ωs)
... | yes ωω = yes (-, *ʷ-ωω)
... | no ¬p with stepᵗ π
... | yes (_ , Rπ) = yes (-, binˡ Rπ)
... | no  ¬Rπ with stepᵗ ρ
... | yes (_ , Rρ) = yes (-, binʳ Rρ)
... | no  ¬Rρ = no λ where
  (_ , *ʷ-↑)   → ¬p ↑↑
  (_ , *ʷ-0ω)  → ¬p 0ω
  (_ , *ʷ-ω0)  → ¬p ω0
  (_ , *ʷ-sω)  → ¬p sω
  (_ , *ʷ-ωs)  → ¬p ωs
  (_ , *ʷ-ωω)  → ¬p ωω
  (_ , binˡ Rπ) → ¬Rπ (-, Rπ)
  (_ , binʳ Rρ) → ¬Rρ (-, Rρ)

stepᵗ 0ᵘ = no λ()

stepᵗ (sucᵘ π) with stepᵗ π
... | yes (_ , Rπ) = yes (-, sucᵘ Rπ)
... | no  ¬Rπ      = no λ{(_ , sucᵘ Rπ) → ¬Rπ (-, Rπ)}

stepᵗ (↑ π) with stepᵗ π
... | yes (_ , Rπ) = yes (-, ↑- Rπ)
... | no  ¬Rπ      = no λ{(_ , ↑- Rπ) → ¬Rπ (-, Rπ)}

stepᵗ ωᵘ = no λ()

stepᵗ [ e ] with isTypeAnn? e
... | yes (_ , _ , refl) = yes (-, υ)
... | no  ¬⦂ with stepᵉ e
... | yes (_ , Re) = yes (-, [ Re ])
... | no  ¬Re      = no λ where
  (_ , υ)      → ¬⦂  (-, -, refl)
  (_ , [ Re ]) → ¬Re (-, Re)

stepᵉ (` x) = no λ()

stepᵉ (f ∙ s) with isTyLam? f
... | yes (_ , _ , _ , _ , refl) = yes (-, β-∙)
... | no  ¬𝛌 with stepᵉ f
... | yes (_ , Rf) = yes (-, ∙ˡ Rf)
... | no  ¬Rf with stepᵗ s
... | yes (_ , Rs) = yes (-, ∙ʳ Rs)
... | no  ¬Rs      = no λ where
  (_ , β-∙)   → ¬𝛌  (-, -, -, -, refl)
  (_ , ∙ˡ Rf) → ¬Rf (-, Rf)
  (_ , ∙ʳ Rs) → ¬Rs (-, Rs)

stepᵉ (𝓤-elim T ρ ρᵀ z s π) with isUsage? π
... | yes is-0   = yes (-, β-𝓤-0)
... | yes is-suc = yes (-, β-𝓤-s)
... | no ¬uπ with stepᵗ T
... | yes (_ , RT) = yes (-, 𝓤-elim-T RT)
... | no  ¬RT with stepᵗ ρ
... | yes (_ , Rρ) = yes (-, 𝓤-elim-ρ Rρ)
... | no  ¬Rρ with stepᵗ ρᵀ
... | yes (_ , Rρᵀ) = yes (-, 𝓤-elim-ρᵀ Rρᵀ)
... | no  ¬Rρᵀ with stepᵗ z
... | yes (_ , Rz) = yes (-, 𝓤-elim-z Rz)
... | no  ¬Rz with stepᵗ s
... | yes (_ , Rs) = yes (-, 𝓤-elim-s Rs)
... | no  ¬Rs with stepᵗ π
... | yes (_ , Rπ) = yes (-, 𝓤-elim-π Rπ)
... | no  ¬Rπ = no λ where
  (_ , β-𝓤-0)         → ¬uπ  is-0
  (_ , β-𝓤-s)         → ¬uπ  is-suc
  (_ , 𝓤-elim-T  RT)  → ¬RT  (-, RT)
  (_ , 𝓤-elim-ρ  Rρ)  → ¬Rρ  (-, Rρ)
  (_ , 𝓤-elim-ρᵀ Rρᵀ) → ¬Rρᵀ (-, Rρᵀ)
  (_ , 𝓤-elim-z  Rz)  → ¬Rz  (-, Rz)
  (_ , 𝓤-elim-s  Rs)  → ¬Rs  (-, Rs)
  (_ , 𝓤-elim-π  Rπ)  → ¬Rπ  (-, Rπ)

stepᵉ (𝓤ω-elim T ρ d w π) with isUsageω? π
... | yes is-↑ = yes (-, β-𝓤ω-↑)
... | yes is-ω = yes (-, β-𝓤ω-ω)
... | no ¬uπ with stepᵗ T
... | yes (_ , RT) = yes (-, 𝓤ω-elim-T RT)
... | no  ¬RT with stepᵗ ρ
... | yes (_ , Rρ) = yes (-, 𝓤ω-elim-ρ Rρ)
... | no  ¬Rρ with stepᵗ d
... | yes (_ , Rd) = yes (-, 𝓤ω-elim-d Rd)
... | no  ¬Rd with stepᵗ w
... | yes (_ , Rw) = yes (-, 𝓤ω-elim-w Rw)
... | no  ¬Rw with stepᵗ π
... | yes (_ , Rπ) = yes (-, 𝓤ω-elim-π Rπ)
... | no  ¬Rπ = no λ where
  (_ , β-𝓤ω-↑)       → ¬uπ is-↑
  (_ , β-𝓤ω-ω)       → ¬uπ is-ω
  (_ , 𝓤ω-elim-T RT) → ¬RT (-, RT)
  (_ , 𝓤ω-elim-ρ Rρ) → ¬Rρ (-, Rρ)
  (_ , 𝓤ω-elim-d Rd) → ¬Rd (-, Rd)
  (_ , 𝓤ω-elim-w Rw) → ¬Rw (-, Rw)
  (_ , 𝓤ω-elim-π Rπ) → ¬Rπ (-, Rπ)

stepᵉ (s ⦂ S) with stepᵗ s
... | yes (_ , Rs) = yes (-, ⦂ˡ Rs)
... | no  ¬Rs with stepᵗ S
... | yes (_ , RS) = yes (-, ⦂ʳ RS)
... | no  ¬RS      = no λ where
  (_ , ⦂ˡ Rs) → ¬Rs (-, Rs)
  (_ , ⦂ʳ RS) → ¬RS (-, RS)

stepᵇ `𝚷[ π / S ] with stepᵗ π
... | yes (_ , Rπ) = yes (-, `𝚷-π Rπ)
... | no  ¬Rπ with stepᵗ S
... | yes (_ , RS) = yes (-, `𝚷-S RS)
... | no  ¬RS = no λ where
  (_ , `𝚷-π Rπ) → ¬Rπ (-, Rπ)
  (_ , `𝚷-S RS) → ¬RS (-, RS)

stepᵇ `𝛌 = no λ()


module Derived {t ℓ} {F : ℕ → Set t}
               (_⟿_ : ∀ {n} → Rel (F n) ℓ)
               (step  : ∀ {n} (t : F n) → Dec (∃ (t ⟿_)))
 where
  open Relation hiding (_∪_)

  private variable X Y Z : F n

  _⇓ : Pred (F n) _
  X ⇓ = ∄[ Y ] (X ⟿ Y)
  infix 10 _⇓

  _⟿+_ _⟿*_ _⟿!_ : Rel (F n) _
  _⟿+_ = Plus′ _⟿_
  _⟿*_ = Star _⟿_
  X ⟿! Y = (X ⟿* Y) × (Y ⇓)
  infix 1 _⟿*_ _⟿+_ _⟿!_

  ⟿-At ⟿+-At ⟿*-At ⟿!-At : ∀ n → Rel (F n) _
  ⟿-At _ = _⟿_
  ⟿+-At _ = _⟿+_
  ⟿*-At _ = _⟿*_
  ⟿!-At _ = _⟿!_

  ≋-At : ∀ n → Rel (F n) _
  ≋-At _ = Star $ SymClosure _⟿_

  _≋_ : Rel (F n) _
  _≋_ = ≋-At _
  infix 4 _≋_

  ≋-isEquiv : Relation.IsEquivalence $ ≋-At n
  ≋-isEquiv =
    record { refl = ε ; sym = RT.reverse $ S.symmetric _⟿_ ; trans = _◅◅_ }

  ≋-setoid : ℕ → Relation.Setoid _ _
  ≋-setoid n = record { isEquivalence = ≋-isEquiv {n} }

  module _ {n} where
    open Relation.IsEquivalence (≋-isEquiv {n}) public using ()
      renaming (refl to ≋-refl ; sym to ≋-sym ; trans to ≋-trans)

  plus-star : _⟿+_ ⇒₂ ⟿*-At n
  plus-star [ R ]    = R ◅ ε
  plus-star (R ∷ Rs) = R ◅ plus-star Rs

  star-plus : _⟿*_ ⇒₂ (_≡_ ∪ ⟿+-At n)
  star-plus ε        = inj₁ refl
  star-plus (R ◅ Rs) = inj₂ $ R ∷′ star-plus Rs where
    _∷′_ : X ⟿ Y → (Y ≡ Z) ⊎ (Y ⟿+ Z) → X ⟿+ Z
    R ∷′ inj₁ refl = [ R ]
    R ∷′ inj₂ Rs   = R ∷ Rs

  star-≋ : _⟿*_ ⇒₂ ≋-At n
  star-≋ ε        = ε
  star-≋ (R ◅ Rs) = fwd R ◅ star-≋ Rs

  plus-≋ : _⟿+_ ⇒₂ ≋-At n
  plus-≋ = star-≋ ∘ plus-star

  eval : (X : F n) → ∀[ Delay (∃[ Z ] (X ⟿! Z)) ]
  eval X with step X
  ... | no  V       = now (-, ε , V)
  ... | yes (Y , R) = later λ{.force → cons-R $ eval Y}
    where cons-R = Delay.map λ{(Z , Rs , V) → Z , R ◅ Rs , V}


open module Evalᵗ = Derived (λ {n} → _⟿ᵗ_ {n}) stepᵗ public using ()
  renaming (⟿-At to ⟿ᵗ-At ;
            _⟿+_ to _⟿ᵗ+_ ; _⟿*_ to _⟿ᵗ*_ ; _⟿!_ to _⟿ᵗ!_ ;
            ⟿+-At to ⟿ᵗ+-At ; ⟿*-At to ⟿ᵗ*-At ; ⟿!-At to ⟿ᵗ!-At ;
            _⇓ to _⇓ᵗ ; eval to evalᵗ ; _≋_ to _≋ᵗ_ ; ≋-At to ≋ᵗ-At)

open module Evalᵉ = Derived (λ {n} → _⟿ᵉ_ {n}) stepᵉ public using ()
  renaming (⟿-At to ⟿ᵉ-At ;
            _⟿+_ to _⟿ᵉ+_ ; _⟿*_ to _⟿ᵉ*_ ; _⟿!_ to _⟿ᵉ!_ ;
            ⟿+-At to ⟿ᵉ+-At ; ⟿*-At to ⟿ᵉ*-At ; ⟿!-At to ⟿ᵉ!-At ;
            _⇓ to _⇓ᵉ ; eval to evalᵉ ; _≋_ to _≋ᵉ_ ; ≋-At to ≋ᵉ-At)

open module Evalᵇ = Derived (λ {n} → _⟿ᵇ_ {n}) stepᵇ public using ()
  renaming (⟿-At to ⟿ᵇ-At ;
            _⟿+_ to _⟿ᵇ+_ ; _⟿*_ to _⟿ᵇ*_ ; _⟿!_ to _⟿ᵇ!_ ;
            ⟿+-At to ⟿ᵇ+-At ; ⟿*-At to ⟿ᵇ*-At ; ⟿!-At to ⟿ᵇ!-At ;
            _⇓ to _⇓ᵇ ; eval to evalᵇ ; _≋_ to _≋ᵇ_ ; ≋-At to ≋ᵇ-At)


module _ {n} where
  open Relation

  `𝚷-cong : `𝚷[_/_] Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵇ-At n
  `𝚷-cong Rπ RS =
    RT.gmap _ (⊎.map `𝚷-π `𝚷-π) Rπ ◅◅
    RT.gmap _ (⊎.map `𝚷-S `𝚷-S) RS

  BIND-cong : BIND Preserves₂ _≋ᵇ_ ⟶ _≋ᵗ_ ⟶ ≋ᵗ-At n
  BIND-cong R𝔅 Rt =
    RT.gmap _ (⊎.map BIND-𝔅 BIND-𝔅) R𝔅 ◅◅
    RT.gmap _ (⊎.map BIND-t BIND-t) Rt

  𝚷-cong : 𝚷[_/_]_ Preserves₃ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵗ-At n
  𝚷-cong Rπ RS = BIND-cong (`𝚷-cong Rπ RS)

  𝛌-cong : 𝛌_ Preserves _≋ᵗ_ ⟶ ≋ᵗ-At n
  𝛌-cong = BIND-cong Evalᵇ.≋-refl

  sucᵘ-cong : sucᵘ Preserves _≋ᵗ_ ⟶ ≋ᵗ-At n
  sucᵘ-cong = RT.gmap _ (⊎.map sucᵘ sucᵘ)

  ↑-cong : ↑_ Preserves _≋ᵗ_ ⟶ ≋ᵗ-At n
  ↑-cong = RT.gmap _ (⊎.map ↑- ↑-)

  bin-cong : _⟪ o ⟫_ Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵗ-At n
  bin-cong Rπ Rρ =
    RT.gmap _ (⊎.map binˡ binˡ) Rπ ◅◅
    RT.gmap _ (⊎.map binʳ binʳ) Rρ

  +-cong : _+_ Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵗ-At n
  +-cong = bin-cong

  *-cong : _*_ Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵗ-At n
  *-cong = bin-cong

  +ʷ-cong : _+ʷ_ Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵗ-At n
  +ʷ-cong = bin-cong

  *ʷ-cong : _*ʷ_ Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵗ-At n
  *ʷ-cong = bin-cong

  []-cong : [_] Preserves _≋ᵉ_ ⟶ ≋ᵗ-At n
  []-cong = RT.gmap _ (⊎.map [_] [_])

  ∙-cong : _∙_ Preserves₂ _≋ᵉ_ ⟶ _≋ᵗ_ ⟶ ≋ᵉ-At n
  ∙-cong Rf Rs = RT.gmap _ (⊎.map ∙ˡ ∙ˡ) Rf ◅◅ RT.gmap _ (⊎.map ∙ʳ ∙ʳ) Rs

  𝓤-elim-cong : 𝓤-elim Preserves₆
                _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵉ-At n
  𝓤-elim-cong RT Rρ Rρᵀ Rz Rs Rπ =
    RT.gmap _ (⊎.map 𝓤-elim-T  𝓤-elim-T)  RT  ◅◅
    RT.gmap _ (⊎.map 𝓤-elim-ρ  𝓤-elim-ρ)  Rρ  ◅◅
    RT.gmap _ (⊎.map 𝓤-elim-ρᵀ 𝓤-elim-ρᵀ) Rρᵀ ◅◅
    RT.gmap _ (⊎.map 𝓤-elim-z  𝓤-elim-z)  Rz  ◅◅
    RT.gmap _ (⊎.map 𝓤-elim-s  𝓤-elim-s)  Rs  ◅◅
    RT.gmap _ (⊎.map 𝓤-elim-π  𝓤-elim-π)  Rπ

  𝓤ω-elim-cong : 𝓤ω-elim Preserves₅
                 _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵉ-At n
  𝓤ω-elim-cong RT Rρ Rd Rw Rπ =
    RT.gmap _ (⊎.map 𝓤ω-elim-T 𝓤ω-elim-T) RT ◅◅
    RT.gmap _ (⊎.map 𝓤ω-elim-ρ 𝓤ω-elim-ρ) Rρ ◅◅
    RT.gmap _ (⊎.map 𝓤ω-elim-d 𝓤ω-elim-d) Rd ◅◅
    RT.gmap _ (⊎.map 𝓤ω-elim-w 𝓤ω-elim-w) Rw ◅◅
    RT.gmap _ (⊎.map 𝓤ω-elim-π 𝓤ω-elim-π) Rπ

  ⦂-cong : _⦂_ Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵉ-At n
  ⦂-cong Rs RS = RT.gmap _ (⊎.map ⦂ˡ ⦂ˡ) Rs ◅◅ RT.gmap _ (⊎.map ⦂ʳ ⦂ʳ) RS


  open ℕ using () renaming (_+_ to _+ᴺ_ ; _*_ to _*ᴺ_)
  open Evalᵗ

  private
    variable a b c : ℕ

    ⌜_⌝ : ℕ → Term n
    ⌜ a ⌝ = fromNat a

  +-ℕ : a +ᴺ b ≡ c → ⌜ a ⌝ + ⌜ b ⌝ ≋ᵗ ⌜ c ⌝
  +-ℕ {zero}  refl = fwd +-0 ◅ ε
  +-ℕ {suc a} refl = fwd +-s ◅ sucᵘ-cong (+-ℕ refl)

  +-ℕ′ : c ≡ a +ᴺ b → ⌜ c ⌝ ≋ᵗ ⌜ a ⌝ + ⌜ b ⌝
  +-ℕ′ = ≋-sym ∘ +-ℕ ∘ ≡.sym

  *-ℕ : a *ᴺ b ≡ c → ⌜ a ⌝ * ⌜ b ⌝ ≋ᵗ ⌜ c ⌝
  *-ℕ {zero}      refl = inj₁ *-0 ◅ ε
  *-ℕ {suc a} {b} refl rewrite ℕ.+-comm b (a *ᴺ b) =
    fwd *-s ◅ bin-cong (*-ℕ refl) Evalᵗ.≋-refl ◅◅ +-ℕ refl

  *-ℕ′ : c ≡ a *ᴺ b → ⌜ c ⌝ ≋ᵗ ⌜ a ⌝ * ⌜ b ⌝
  *-ℕ′ = ≋-sym ∘ *-ℕ ∘ ≡.sym

  +ʷ-ℕ : a +ᴺ b ≡ c → ↑ ⌜ a ⌝ +ʷ ↑ ⌜ b ⌝ ≋ᵗ ↑ ⌜ c ⌝
  +ʷ-ℕ refl = fwd +ʷ-↑ ◅ ↑-cong (+-ℕ refl)

  +ʷ-ℕ′ : c ≡ a +ᴺ b → ↑ ⌜ c ⌝ ≋ᵗ ↑ ⌜ a ⌝ +ʷ ↑ ⌜ b ⌝
  +ʷ-ℕ′ = ≋-sym ∘ +ʷ-ℕ ∘ ≡.sym

  *ʷ-ℕ : a *ᴺ b ≡ c → ↑ ⌜ a ⌝ *ʷ ↑ ⌜ b ⌝ ≋ᵗ ↑ ⌜ c ⌝
  *ʷ-ℕ refl = fwd *ʷ-↑ ◅ ↑-cong (*-ℕ refl)

  *ʷ-ℕ′ : c ≡ a *ᴺ b → ↑ ⌜ c ⌝ ≋ᵗ ↑ ⌜ a ⌝ *ʷ ↑ ⌜ b ⌝
  *ʷ-ℕ′ = ≋-sym ∘ *ʷ-ℕ ∘ ≡.sym


1-* : 1 * π ≋ᵗ π
1-* = Evalᵗ.star-≋ $ *-s ◅ binˡ *-0 ◅ +-0 ◅ ε

1-*ʷ : ↑ 1 *ʷ ↑ π ≋ᵗ ↑ π
1-*ʷ = fwd *ʷ-↑ ◅ ↑-cong 1-*

0-+ : 0 + π ≋ᵗ π
0-+ = fwd +-0 ◅ ε

0-+ʷ : ↑ 0 +ʷ ↑ π ≋ᵗ ↑ π
0-+ʷ = fwd +ʷ-↑ ◅ ↑-cong 0-+
