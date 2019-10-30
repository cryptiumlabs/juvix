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
  s s′ t t′ z z′ w w′ : Term n
  S S′ T T′ U U′ : Type n
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
  *ᵘ-suc : sucᵘ π *ᵘ ρ      ⟿ᵗ π *ᵘ ρ +ᵘ ρ
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


private
  data Is-0   : Term n → Set where is-0   : Is-0   $ 0ᵘ {n}
  data Is-suc : Term n → Set where is-suc : Is-suc $ sucᵘ π
  data Is-ω   : Term n → Set where is-ω   : Is-ω   $ ωᵘ {n}

  data IsUsageCon : Term n → Set where
    is-0   : IsUsageCon $ 0ᵘ {n}
    is-suc : IsUsageCon $ sucᵘ π
    is-ω   : IsUsageCon $ ωᵘ {n}

  is-0? : Decidable₁ $ Is-0 {n}
  is-0? (⋆ u) = no (λ ())
  is-0? 𝓤 = no (λ ())
  is-0? (𝚷[ π / S ] T) = no (λ ())
  is-0? (𝛌 t) = no (λ ())
  is-0? 0ᵘ = yes is-0
  is-0? ωᵘ = no (λ ())
  is-0? (sucᵘ π) = no (λ ())
  is-0? (π +ᵘ ρ) = no (λ ())
  is-0? (π *ᵘ ρ) = no (λ ())
  is-0? [ e ] = no (λ ())

  is-suc? : Decidable₁ $ Is-suc {n}
  is-suc? (⋆ u) = no (λ ())
  is-suc? 𝓤 = no (λ ())
  is-suc? (𝚷[ π / S ] T) = no (λ ())
  is-suc? (𝛌 t) = no (λ ())
  is-suc? 0ᵘ = no (λ ())
  is-suc? ωᵘ = no (λ ())
  is-suc? (sucᵘ π) = yes is-suc
  is-suc? (π +ᵘ ρ) = no (λ ())
  is-suc? (π *ᵘ ρ) = no (λ ())
  is-suc? [ e ] = no (λ ())

  is-ω? : Decidable₁ $ Is-ω {n}
  is-ω? (⋆ u) = no (λ ())
  is-ω? 𝓤 = no (λ ())
  is-ω? (𝚷[ π / S ] T) = no (λ ())
  is-ω? (𝛌 t) = no (λ ())
  is-ω? 0ᵘ = no (λ ())
  is-ω? ωᵘ = yes is-ω
  is-ω? (sucᵘ π) = no (λ ())
  is-ω? (π +ᵘ ρ) = no (λ ())
  is-ω? (π *ᵘ ρ) = no (λ ())
  is-ω? [ e ] = no (λ ())

  isUsageCon? : Decidable₁ $ IsUsageCon {n}
  isUsageCon? t with is-0? t | is-suc? t  | is-ω? t
  isUsageCon? _ | yes is-0   | s?         | ω?       = yes is-0
  isUsageCon? _ | no ¬0      | yes is-suc | ω?       = yes is-suc
  isUsageCon? _ | no ¬0      | no ¬s      | yes is-ω = yes is-ω
  isUsageCon? _ | no ¬0      | no ¬s      | no ¬ω    = no λ where
    is-0   → ¬0 is-0
    is-suc → ¬s is-suc
    is-ω   → ¬ω is-ω

  isTypeAnn? : (e : Elim n) → Dec $ ∃[ s ] ∃[ S ] (e ≡ s ⦂ S)
  isTypeAnn? (` x)              = no λ()
  isTypeAnn? (e ∙ s)            = no λ()
  isTypeAnn? (𝓤-elim T z s w π) = no λ()
  isTypeAnn? (s ⦂ S)            = yes (-, -, refl)

  isTyLam? : (e : Elim n) →
             Dec (∃[ t ] ∃[ π ] ∃[ S ] ∃[ T ] (e ≡ 𝛌 t ⦂ 𝚷[ π / S ] T))
  isTyLam? (` x)                = no λ()
  isTyLam? (e ∙ s)              = no λ()
  isTyLam? (𝓤-elim T z s w π)   = no λ()
  isTyLam? (⋆ u ⦂ S)            = no λ()
  isTyLam? (𝓤 ⦂ S)              = no λ()
  isTyLam? (𝚷[ π / S₁ ] T ⦂ S)  = no λ()
  isTyLam? (𝛌 s ⦂ ⋆ u)          = no λ()
  isTyLam? (𝛌 s ⦂ 𝓤)            = no λ()
  isTyLam? (𝛌 s ⦂ 𝚷[ π / S ] T) = yes (-, -, -, -, refl)
  isTyLam? (𝛌 s ⦂ 𝛌 S)          = no λ()
  isTyLam? (𝛌 s ⦂ 0ᵘ)           = no λ()
  isTyLam? (𝛌 s ⦂ ωᵘ)           = no λ()
  isTyLam? (𝛌 s ⦂ sucᵘ π)       = no λ()
  isTyLam? (𝛌 s ⦂ (π +ᵘ ρ))     = no λ()
  isTyLam? (𝛌 s ⦂ (π *ᵘ ρ))     = no λ()
  isTyLam? (𝛌 s ⦂ [ e ])        = no λ()
  isTyLam? (0ᵘ ⦂ S)             = no λ()
  isTyLam? (ωᵘ ⦂ S)             = no λ()
  isTyLam? (sucᵘ π ⦂ S)         = no λ()
  isTyLam? ((π +ᵘ ρ) ⦂ S)       = no λ()
  isTyLam? ((π *ᵘ ρ) ⦂ S)       = no λ()
  isTyLam? ([ e ] ⦂ S)          = no λ()


stepᵗ : (t : Term n) → Dec (∃ (t ⟿ᵗ_))
stepᵉ : (e : Elim n) → Dec (∃ (e ⟿ᵉ_))

stepᵗ (⋆ u) = no λ()
stepᵗ 𝓤     = no λ()

stepᵗ (𝚷[ π / S ] T) with stepᵗ π
stepᵗ (𝚷[ π / S ] T) | yes (_ , Rπ)                   = yes (-, 𝚷₁ Rπ)
stepᵗ (𝚷[ π / S ] T) | no ¬Rπ with stepᵗ S
stepᵗ (𝚷[ π / S ] T) | no ¬Rπ | yes (_ , RS)          = yes (-, 𝚷₂ RS)
stepᵗ (𝚷[ π / S ] T) | no ¬Rπ | no ¬RS with stepᵗ T
stepᵗ (𝚷[ π / S ] T) | no ¬Rπ | no ¬RS | yes (_ , RT) = yes (-, 𝚷₃ RT)
stepᵗ (𝚷[ π / S ] T) | no ¬Rπ | no ¬RS | no ¬RT       = no λ where
  (_ , 𝚷₁ Rπ) → ¬Rπ (-, Rπ)
  (_ , 𝚷₂ RS) → ¬RS (-, RS)
  (_ , 𝚷₃ RT) → ¬RT (-, RT)

stepᵗ (𝛌 t) with stepᵗ t
stepᵗ (𝛌 t) | yes (_ , R) = yes (-, 𝛌- R)
stepᵗ (𝛌 t) | no  ¬R      = no λ{(_ , 𝛌- R) → ¬R (-, R)}

stepᵗ 0ᵘ = no λ()
stepᵗ ωᵘ = no λ()

stepᵗ (sucᵘ π) with is-ω? π
stepᵗ (sucᵘ .ωᵘ) | yes is-ω              = yes (-, sucᵘ-ω)
stepᵗ (sucᵘ π)   | no  π≢ω with stepᵗ π
stepᵗ (sucᵘ π)   | no  π≢ω | yes (_ , R) = yes (-, sucᵘ R)
stepᵗ (sucᵘ π)   | no  π≢ω | no  ¬R      = no λ where
  (_ , sucᵘ R) → ¬R (-, R)
  (_ , sucᵘ-ω) → π≢ω is-ω

stepᵗ (π +ᵘ ρ) with isUsageCon? π
stepᵗ (.0ᵘ +ᵘ ρ)       | yes is-0                      = yes (-, +ᵘ-0)
stepᵗ (.(sucᵘ _) +ᵘ ρ) | yes is-suc                    = yes (-, +ᵘ-suc)
stepᵗ (.ωᵘ +ᵘ ρ)       | yes is-ω                      = yes (-, +ᵘ-ω)
stepᵗ (π +ᵘ ρ)         | no ¬u with stepᵗ π
stepᵗ (π +ᵘ ρ)         | no ¬u | yes (_ , Rπ)          = yes (-, +ᵘˡ Rπ)
stepᵗ (π +ᵘ ρ)         | no ¬u | no ¬Rπ with stepᵗ ρ
stepᵗ (π +ᵘ ρ)         | no ¬u | no ¬Rπ | yes (_ , Rρ) = yes (-, +ᵘʳ Rρ)
stepᵗ (π +ᵘ ρ)         | no ¬u | no ¬Rπ | no ¬Rρ       = no λ where
  (_ , +ᵘˡ R)  → ¬Rπ (-, R)
  (_ , +ᵘʳ R)  → ¬Rρ (-, R)
  (_ , +ᵘ-0)   → ¬u is-0
  (_ , +ᵘ-suc) → ¬u is-suc
  (_ , +ᵘ-ω)   → ¬u is-ω

stepᵗ (π *ᵘ ρ) with isUsageCon? π
stepᵗ (.0ᵘ *ᵘ ρ)         | yes is-0                         = yes (-, *ᵘ-0)
stepᵗ (.(sucᵘ _) *ᵘ ρ)   | yes is-suc                       = yes (-, *ᵘ-suc)
stepᵗ (.ωᵘ *ᵘ ρ)         | yes is-ω with isUsageCon? ρ
stepᵗ (.ωᵘ *ᵘ .0ᵘ)       | yes is-ω | yes is-0              = yes (-, *ᵘ-ω0)
stepᵗ (.ωᵘ *ᵘ .(sucᵘ _)) | yes is-ω | yes is-suc            = yes (-, *ᵘ-ωs)
stepᵗ (.ωᵘ *ᵘ .ωᵘ)       | yes is-ω | yes is-ω              = yes (-, *ᵘ-ωω)
stepᵗ (.ωᵘ *ᵘ ρ)         | yes is-ω | no ¬uρ with stepᵗ ρ
stepᵗ (.ωᵘ *ᵘ ρ)         | yes is-ω | no ¬uρ | yes (_ , Rρ) = yes (-, *ᵘʳ Rρ)
stepᵗ (.ωᵘ *ᵘ ρ)         | yes is-ω | no ¬uρ | no ¬Rρ       = no λ where
  (_ , *ᵘʳ Rρ) → ¬Rρ (-, Rρ)
  (_ , *ᵘ-ω0)  → ¬uρ is-0
  (_ , *ᵘ-ωs)  → ¬uρ is-suc
  (_ , *ᵘ-ωω)  → ¬uρ is-ω
stepᵗ (π *ᵘ ρ) | no ¬uπ with stepᵗ π
stepᵗ (π *ᵘ ρ) | no ¬uπ | yes (_ , Rπ)          = yes (-, *ᵘˡ Rπ)
stepᵗ (π *ᵘ ρ) | no ¬uπ | no ¬Rπ with stepᵗ ρ
stepᵗ (π *ᵘ ρ) | no ¬uπ | no ¬Rπ | yes (_ , Rρ) = yes (-, *ᵘʳ Rρ)
stepᵗ (π *ᵘ ρ) | no ¬uπ | no ¬Rπ | no ¬Rρ       = no λ where
  (_ , *ᵘˡ R)  → ¬Rπ (-, R)
  (_ , *ᵘʳ R)  → ¬Rρ (-, R)
  (_ , *ᵘ-0)   → ¬uπ is-0
  (_ , *ᵘ-suc) → ¬uπ is-suc
  (_ , *ᵘ-ω0)  → ¬uπ is-ω
  (_ , *ᵘ-ωs)  → ¬uπ is-ω
  (_ , *ᵘ-ωω)  → ¬uπ is-ω

stepᵗ [ e ] with isTypeAnn? e
stepᵗ [ .(s ⦂ S) ] | yes (s , S , refl)   = yes (-, υ)
stepᵗ [ e ]        | no ¬⦂ with stepᵉ e
stepᵗ [ e ]        | no ¬⦂ | yes (_ , Re) = yes (-, [ Re ])
stepᵗ [ e ]        | no ¬⦂ | no ¬Re       = no λ where
  (_ , υ)      → ¬⦂  (-, -, refl)
  (_ , [ Re ]) → ¬Re (-, Re)

stepᵉ (` x) = no λ()

stepᵉ (e ∙ s) with isTyLam? e
stepᵉ (e ∙ s) | yes (t , π , S , T , refl)    = yes (-, β-∙)
stepᵉ (e ∙ s) | no ¬𝛌 with stepᵉ e
stepᵉ (e ∙ s) | no ¬𝛌 | yes (_ , Re)          = yes (-, ∙ˡ Re)
stepᵉ (e ∙ s) | no ¬𝛌 | no ¬Re with stepᵗ s
stepᵉ (e ∙ s) | no ¬𝛌 | no ¬Re | yes (_ , Rs) = yes (-, ∙ʳ Rs)
stepᵉ (e ∙ s) | no ¬𝛌 | no ¬Re | no ¬Rs       = no λ where
  (_ , β-∙)   → ¬𝛌 (-, -, -, -, refl)
  (_ , ∙ˡ Re) → ¬Re (-, Re)
  (_ , ∙ʳ Rs) → ¬Rs (-, Rs)

stepᵉ (𝓤-elim T z s w π) with isUsageCon? π
stepᵉ (𝓤-elim T z s w .0ᵘ)       | yes is-0   = yes (-, β-𝓤0)
stepᵉ (𝓤-elim T z s w .(sucᵘ _)) | yes is-suc = yes (-, β-𝓤s)
stepᵉ (𝓤-elim T z s w .ωᵘ)       | yes is-ω   = yes (-, β-𝓤ω)
... | no ¬u with stepᵗ T
... | yes (_ , RT) = yes (-, 𝓤-elim₁ RT)
... | no ¬RT with stepᵗ z
... | yes (_ , Rz) = yes (-, 𝓤-elim₂ Rz)
... | no ¬Rz with stepᵗ s
... | yes (_ , Rs) = yes (-, 𝓤-elim₃ Rs)
... | no ¬Rs with stepᵗ w
... | yes (_ , Rw) = yes (-, 𝓤-elim₄ Rw)
... | no ¬Rw with stepᵗ π
... | yes (_ , Rπ) = yes (-, 𝓤-elim₅ Rπ)
... | no ¬Rπ = no λ where
  (_ , β-𝓤0)       → ¬u is-0
  (_ , β-𝓤s)       → ¬u is-suc
  (_ , β-𝓤ω)       → ¬u is-ω
  (_ , 𝓤-elim₁ RT) → ¬RT (-, RT)
  (_ , 𝓤-elim₂ Rz) → ¬Rz (-, Rz)
  (_ , 𝓤-elim₃ Rs) → ¬Rs (-, Rs)
  (_ , 𝓤-elim₄ Rw) → ¬Rw (-, Rw)
  (_ , 𝓤-elim₅ Rπ) → ¬Rπ (-, Rπ)

stepᵉ (s ⦂ S) with stepᵗ s
stepᵉ (s ⦂ S) | yes (_ , Rs)          = yes (-, ⦂ˡ Rs)
stepᵉ (s ⦂ S) | no ¬Rs with stepᵗ S
stepᵉ (s ⦂ S) | no ¬Rs | yes (_ , RS) = yes (-, ⦂ʳ RS)
stepᵉ (s ⦂ S) | no ¬Rs | no  ¬RS      = no λ where
  (_ , ⦂ˡ Rs) → ¬Rs (-, Rs)
  (_ , ⦂ʳ RS) → ¬RS (-, RS)


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
  ... | yes (Y , R) = later λ where .force → cons-R $ eval Y
    where cons-R = Delay.map λ where (Z , Rs , V) → Z , R ◅ Rs , V


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
