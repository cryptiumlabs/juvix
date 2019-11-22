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
         let s′ = substᵗ (substᵗ s (weakᵗ π ⦂ 𝓤)) (𝓤-elim T z s w π)
             T′ = substᵗ T (sucᵘ π ⦂ 𝓤)
         in  s′ ⦂ T′
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

  isUsageCon? : Decidable₁ $ IsUsageCon {n}
  isUsageCon? (⋆ _)          = no λ()
  isUsageCon? 𝓤              = no λ()
  isUsageCon? (𝚷[ _ / _ ] _) = no λ()
  isUsageCon? (𝛌 _)          = no λ()
  isUsageCon? 0ᵘ             = yes is-0
  isUsageCon? ωᵘ             = yes is-ω
  isUsageCon? (sucᵘ _)       = yes is-suc
  isUsageCon? (_ +ᵘ _)       = no λ()
  isUsageCon? (_ *ᵘ _)       = no λ()
  isUsageCon? [ _ ]          = no λ()

  is-0? : Decidable₁ $ Is-0 {n}
  is-0? s with isUsageCon? s
  ... | yes is-0   = yes is-0
  ... | yes is-suc = no λ()
  ... | yes is-ω   = no λ()
  ... | no  ¬u     = no λ{is-0 → ¬u is-0}

  is-suc? : Decidable₁ $ Is-suc {n}
  is-suc? s with isUsageCon? s
  ... | yes is-0   = no λ()
  ... | yes is-suc = yes is-suc
  ... | yes is-ω   = no λ()
  ... | no  ¬u     = no λ{is-suc → ¬u is-suc}

  is-ω? : Decidable₁ $ Is-ω {n}
  is-ω? s with isUsageCon? s
  ... | yes is-0   = no λ()
  ... | yes is-suc = no λ()
  ... | yes is-ω   = yes is-ω
  ... | no  ¬u     = no λ{is-ω → ¬u is-ω}

  isTypeAnn? : (e : Elim n) → Dec $ ∃[ s ] ∃[ S ] (e ≡ s ⦂ S)
  isTypeAnn? (` x)              = no λ()
  isTypeAnn? (e ∙ s)            = no λ()
  isTypeAnn? (𝓤-elim T z s w π) = no λ()
  isTypeAnn? (s ⦂ S)            = yes (s , S , refl)

  isTyLam? : (e : Elim n) →
             Dec (∃[ s ] ∃[ π ] ∃[ S ] ∃[ T ] (e ≡ 𝛌 s ⦂ 𝚷[ π / S ] T))
  isTyLam? (` x)                = no λ()
  isTyLam? (e ∙ s)              = no λ()
  isTyLam? (𝓤-elim T z s w π)   = no λ()
  isTyLam? (⋆ u ⦂ S)            = no λ()
  isTyLam? (𝓤 ⦂ S)              = no λ()
  isTyLam? (𝚷[ π / S₁ ] T ⦂ S)  = no λ()
  isTyLam? (𝛌 s ⦂ ⋆ u)          = no λ()
  isTyLam? (𝛌 s ⦂ 𝓤)            = no λ()
  isTyLam? (𝛌 s ⦂ 𝚷[ π / S ] T) = yes (s , π , S , T , refl)
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
... | yes (_ , Rπ) = yes (-, 𝚷₁ Rπ)
... | no ¬Rπ with stepᵗ S
... | yes (_ , RS) = yes (-, 𝚷₂ RS)
... | no ¬RS with stepᵗ T
... | yes (_ , RT) = yes (-, 𝚷₃ RT)
... | no ¬RT       = no λ where
  (_ , 𝚷₁ Rπ) → ¬Rπ (-, Rπ)
  (_ , 𝚷₂ RS) → ¬RS (-, RS)
  (_ , 𝚷₃ RT) → ¬RT (-, RT)

stepᵗ (𝛌 t) with stepᵗ t
... | yes (_ , R) = yes (-, 𝛌- R)
... | no  ¬R      = no λ{(_ , 𝛌- R) → ¬R (-, R)}

stepᵗ 0ᵘ = no λ()
stepᵗ ωᵘ = no λ()

stepᵗ (sucᵘ π) with is-ω? π
... | yes is-ω = yes (-, sucᵘ-ω)
... | no  π≢ω with stepᵗ π
... | yes (_ , R) = yes (-, sucᵘ R)
... | no  ¬R      = no λ where
  (_ , sucᵘ R) → ¬R (-, R)
  (_ , sucᵘ-ω) → π≢ω is-ω

stepᵗ (π +ᵘ ρ) with isUsageCon? π
... | yes is-0   = yes (-, +ᵘ-0)
... | yes is-suc = yes (-, +ᵘ-suc)
... | yes is-ω   = yes (-, +ᵘ-ω)
... | no ¬u with stepᵗ π
... | yes (_ , Rπ) = yes (-, +ᵘˡ Rπ)
... | no ¬Rπ with stepᵗ ρ
... | yes (_ , Rρ) = yes (-, +ᵘʳ Rρ)
... | no ¬Rρ       = no λ where
  (_ , +ᵘˡ R)  → ¬Rπ (-, R)
  (_ , +ᵘʳ R)  → ¬Rρ (-, R)
  (_ , +ᵘ-0)   → ¬u is-0
  (_ , +ᵘ-suc) → ¬u is-suc
  (_ , +ᵘ-ω)   → ¬u is-ω

stepᵗ (π *ᵘ ρ) with isUsageCon? π
... | yes is-0   = yes (-, *ᵘ-0)
... | yes is-suc = yes (-, *ᵘ-suc)
... | yes is-ω with isUsageCon? ρ
... | yes is-0   = yes (-, *ᵘ-ω0)
... | yes is-suc = yes (-, *ᵘ-ωs)
... | yes is-ω   = yes (-, *ᵘ-ωω)
... | no ¬uρ with stepᵗ ρ
... | yes (_ , Rρ) = yes (-, *ᵘʳ Rρ)
... | no ¬Rρ       = no λ where
  (_ , *ᵘʳ Rρ) → ¬Rρ (-, Rρ)
  (_ , *ᵘ-ω0)  → ¬uρ is-0
  (_ , *ᵘ-ωs)  → ¬uρ is-suc
  (_ , *ᵘ-ωω)  → ¬uρ is-ω
stepᵗ (π *ᵘ ρ) | no ¬uπ with stepᵗ π
... | yes (_ , Rπ) = yes (-, *ᵘˡ Rπ)
... | no ¬Rπ with stepᵗ ρ
... | yes (_ , Rρ) = yes (-, *ᵘʳ Rρ)
... | no ¬Rρ       = no λ where
  (_ , *ᵘˡ R)  → ¬Rπ (-, R)
  (_ , *ᵘʳ R)  → ¬Rρ (-, R)
  (_ , *ᵘ-0)   → ¬uπ is-0
  (_ , *ᵘ-suc) → ¬uπ is-suc
  (_ , *ᵘ-ω0)  → ¬uπ is-ω
  (_ , *ᵘ-ωs)  → ¬uπ is-ω
  (_ , *ᵘ-ωω)  → ¬uπ is-ω

stepᵗ [ e ] with isTypeAnn? e
... | yes (_ , _ , refl) = yes (-, υ)
... | no ¬⦂ with stepᵉ e
... | yes (_ , Re) = yes (-, [ Re ])
... | no ¬Re       = no λ where
  (_ , υ)      → ¬⦂  (-, -, refl)
  (_ , [ Re ]) → ¬Re (-, Re)

stepᵉ (` x) = no λ()

stepᵉ (e ∙ s) with isTyLam? e
... | yes (t , π , S , T , refl) = yes (-, β-∙)
... | no ¬𝛌 with stepᵉ e
... | yes (_ , Re) = yes (-, ∙ˡ Re)
... | no ¬Re with stepᵗ s
... | yes (_ , Rs) = yes (-, ∙ʳ Rs)
... | no ¬Rs       = no λ where
  (_ , β-∙)   → ¬𝛌 (-, -, -, -, refl)
  (_ , ∙ˡ Re) → ¬Re (-, Re)
  (_ , ∙ʳ Rs) → ¬Rs (-, Rs)

stepᵉ (𝓤-elim T z s w π) with isUsageCon? π
... | yes is-0   = yes (-, β-𝓤0)
... | yes is-suc = yes (-, β-𝓤s)
... | yes is-ω   = yes (-, β-𝓤ω)
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
... | yes (_ , Rs) = yes (-, ⦂ˡ Rs)
... | no ¬Rs with stepᵗ S
... | yes (_ , RS) = yes (-, ⦂ʳ RS)
... | no  ¬RS      = no λ where
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


module _ {n} where
  open Relation

  𝚷-cong : 𝚷[_/_]_ Preserves₃ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵗ-At n
  𝚷-cong Rπ RS RT =
    RT.gmap _ (⊎.map 𝚷₁ 𝚷₁) Rπ ◅◅
    RT.gmap _ (⊎.map 𝚷₂ 𝚷₂) RS ◅◅
    RT.gmap _ (⊎.map 𝚷₃ 𝚷₃) RT

  𝛌-cong : 𝛌_ Preserves _≋ᵗ_ ⟶ ≋ᵗ-At n
  𝛌-cong = RT.gmap _ (⊎.map 𝛌- 𝛌-)

  sucᵘ-cong : sucᵘ Preserves _≋ᵗ_ ⟶ ≋ᵗ-At n
  sucᵘ-cong = RT.gmap _ (⊎.map sucᵘ sucᵘ)

  +ᵘ-cong : _+ᵘ_ Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵗ-At n
  +ᵘ-cong Rπ Rρ = RT.gmap _ (⊎.map +ᵘˡ +ᵘˡ) Rπ ◅◅ RT.gmap _ (⊎.map +ᵘʳ +ᵘʳ) Rρ

  *ᵘ-cong : _*ᵘ_ Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵗ-At n
  *ᵘ-cong Rπ Rρ = RT.gmap _ (⊎.map *ᵘˡ *ᵘˡ) Rπ ◅◅ RT.gmap _ (⊎.map *ᵘʳ *ᵘʳ) Rρ

  []-cong : [_] Preserves _≋ᵉ_ ⟶ ≋ᵗ-At n
  []-cong = RT.gmap _ (⊎.map [_] [_])

  ∙-cong : _∙_ Preserves₂ _≋ᵉ_ ⟶ _≋ᵗ_ ⟶ ≋ᵉ-At n
  ∙-cong Rf Rs = RT.gmap _ (⊎.map ∙ˡ ∙ˡ) Rf ◅◅ RT.gmap _ (⊎.map ∙ʳ ∙ʳ) Rs

  𝓤-elim-cong : 𝓤-elim Preserves₅
                _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵉ-At n
  𝓤-elim-cong RT Rz Rs Rw Rπ =
    RT.gmap _ (⊎.map 𝓤-elim₁ 𝓤-elim₁) RT ◅◅
    RT.gmap _ (⊎.map 𝓤-elim₂ 𝓤-elim₂) Rz ◅◅
    RT.gmap _ (⊎.map 𝓤-elim₃ 𝓤-elim₃) Rs ◅◅
    RT.gmap _ (⊎.map 𝓤-elim₄ 𝓤-elim₄) Rw ◅◅
    RT.gmap _ (⊎.map 𝓤-elim₅ 𝓤-elim₅) Rπ

  ⦂-cong : _⦂_ Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵉ-At n
  ⦂-cong Rs RS = RT.gmap _ (⊎.map ⦂ˡ ⦂ˡ) Rs ◅◅ RT.gmap _ (⊎.map ⦂ʳ ⦂ʳ) RS


  open ℕ using (_+_ ; _*_)
  open Evalᵗ

  private
    variable a b c : ℕ

    ⌜_⌝ : ℕ → Term n
    ⌜ a ⌝ = fromNat a

  +ᵘ-ℕ : a + b ≡ c → ⌜ a ⌝ +ᵘ ⌜ b ⌝ ≋ᵗ ⌜ c ⌝
  +ᵘ-ℕ {zero}  refl = fwd +ᵘ-0   ◅ ε
  +ᵘ-ℕ {suc a} refl = fwd +ᵘ-suc ◅ sucᵘ-cong (+ᵘ-ℕ refl)

  +ᵘ-ℕ′ : c ≡ a + b → ⌜ c ⌝ ≋ᵗ ⌜ a ⌝ +ᵘ ⌜ b ⌝
  +ᵘ-ℕ′ = ≋-sym ∘ +ᵘ-ℕ ∘ ≡.sym

  *ᵘ-ℕ : a * b ≡ c → ⌜ a ⌝ *ᵘ ⌜ b ⌝ ≋ᵗ ⌜ c ⌝
  *ᵘ-ℕ {zero}      refl = inj₁ *ᵘ-0 ◅ ε
  *ᵘ-ℕ {suc a} {b} refl rewrite ℕ.+-comm b (a * b) =
    fwd *ᵘ-suc ◅ +ᵘ-cong (*ᵘ-ℕ refl) Evalᵗ.≋-refl ◅◅ +ᵘ-ℕ refl

  *ᵘ-ℕ′ : c ≡ a * b → ⌜ c ⌝ ≋ᵗ ⌜ a ⌝ *ᵘ ⌜ b ⌝
  *ᵘ-ℕ′ = ≋-sym ∘ *ᵘ-ℕ ∘ ≡.sym

1-*ᵘ : 1 *ᵘ π ≋ᵗ π
1-*ᵘ = Evalᵗ.star-≋ $ *ᵘ-suc ◅ +ᵘˡ *ᵘ-0 ◅ +ᵘ-0 ◅ ε
