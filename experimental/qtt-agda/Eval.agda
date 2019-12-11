module Eval where

open import Prelude
open import QTT
open import Hole

open import Relation.Binary.Construct.Closure.ReflexiveTransitive as RT
  using (Star ; ε ; _◅_ ; _◅◅_)
open import Relation.Binary.Construct.Closure.Transitive as T
  using (Plus′ ; [_] ; _∷_)
open import Relation.Binary.Construct.Closure.Symmetric as S
  using (SymClosure ; fwd ; bwd)
open import Relation.Binary.Construct.Union as U using (_∪_)

open import Codata.Thunk using (Thunk ; force)
open import Codata.Delay as Delay using (Delay ; now ; later)


private
 variable
  n n′ h : ℕ
  ℋ : SynKind
  s s′ t t′ z z′ d d′ w w′ : Term n
  S S′ T T′ U U′ : Type n
  π π′ ρ ρ′ ρᵀ ρᵀ′ : Usage n
  e e′ f f′ : Elim n
  B B′ C : Binder n
  o : BinOp


module Derived {𝒯 : ℕ → Set} (⟿-At : ∀ n → Rel (𝒯 n) lzero) where
  open Relation hiding (_∪_)

  private variable X Y Z : 𝒯 n

  -- single step as an infix operator
  _⟿_ : Rel (𝒯 n) _
  _⟿_ {n} = ⟿-At n
  infix 1 _⟿_

  -- X ⇓ means X doesn't reduce
  -- (reduction is untyped so it includes ill-typed stuck terms, but
  -- for now let's call them "values" anyway)
  _⇓ : Pred (𝒯 n) _
  X ⇓ = ∄[ Y ] (X ⟿ Y)
  infix 10 _⇓

  -- * 1-n steps
  -- * 0-n steps
  -- * 0-n steps & results in a value
  _⟿+_ _⟿*_ _⟿!_ : Rel (𝒯 n) _
  _⟿+_ = Plus′ _⟿_
  _⟿*_ = Star _⟿_
  X ⟿! Y = (X ⟿* Y) × (Y ⇓)
  infix 1 _⟿*_ _⟿+_ _⟿!_

  -- nonfix versions with explicit n
  ⟿+-At ⟿*-At ⟿!-At : ∀ n → Rel (𝒯 n) _
  ⟿+-At _ = _⟿+_
  ⟿*-At _ = _⟿*_
  ⟿!-At _ = _⟿!_

  -- equality: two terms S, T are equal if there is a third term U
  -- which S and T both reduce to
  record ≋-At n (S T : 𝒯 n) : Set where
    constructor make-≋
    field
      {reduct} : 𝒯 n
      left     : S ⟿* reduct
      right    : T ⟿* reduct
  open ≋-At public
  
  _≋_ : Rel (𝒯 n) _
  _≋_ = ≋-At _
  infix 4 _≋_

  ≋-refl : Reflexive $ ≋-At n
  ≋-refl = make-≋ ε ε

  ≋-sym : Symmetric $ ≋-At n
  ≋-sym (make-≋ L R) = make-≋ R L

  -- transitivity of ≋ needs strong church-rosser ☹
  -- so it is elsewhere

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
  star-≋ Rs = make-≋ Rs ε

  plus-≋ : _⟿+_ ⇒₂ ≋-At n
  plus-≋ = star-≋ ∘ plus-star

  module Eval (step : ∀ {n} (t : 𝒯 n) → Dec (∃ (t ⟿_))) where
    eval : (X : 𝒯 n) → ∀[ Delay (∃[ Z ] (X ⟿! Z)) ]
    eval X with step X
    ... | no  V       = now (-, ε , V)
    ... | yes (Y , R) = later λ{.force → cons-R $ eval Y}
      where cons-R = Delay.map λ{(Z , Rs , V) → Z , R ◅ Rs , V}

open Derived public using (make-≋ ; reduct ; left ; right)


data ⟿ᵗ-At′ n : Rel (Term n) lzero
data ⟿ᵉ-At′ n : Rel (Elim n) lzero

data ⟿ᵗ-At′ n where
  υ : [ t ⦂ T ] ⟨ ⟿ᵗ-At′ _ ⟩ t

  +-0 : 0ᵘ     + ρ ⟨ ⟿ᵗ-At′ _ ⟩ ρ
  +-s : sucᵘ π + ρ ⟨ ⟿ᵗ-At′ _ ⟩ sucᵘ (π + ρ)

  *-0 : 0ᵘ     * ρ ⟨ ⟿ᵗ-At′ _ ⟩ 0ᵘ
  *-s : sucᵘ π * ρ ⟨ ⟿ᵗ-At′ _ ⟩ π * ρ + ρ

  +ʷ-↑  : ↑ π +ʷ ↑ ρ ⟨ ⟿ᵗ-At′ _ ⟩ ↑ (π + ρ)
  +ʷ-ωˡ : ωᵘ  +ʷ ρ   ⟨ ⟿ᵗ-At′ _ ⟩ ωᵘ
  +ʷ-ωʳ : π   +ʷ ωᵘ  ⟨ ⟿ᵗ-At′ _ ⟩ ωᵘ

  *ʷ-↑  : ↑ π      *ʷ ↑ ρ      ⟨ ⟿ᵗ-At′ _ ⟩ ↑ (π * ρ)
  *ʷ-0ω : ↑ 0ᵘ     *ʷ ωᵘ       ⟨ ⟿ᵗ-At′ _ ⟩ ↑ 0ᵘ
  *ʷ-ω0 : ωᵘ       *ʷ ↑ 0ᵘ     ⟨ ⟿ᵗ-At′ _ ⟩ ↑ 0ᵘ
  *ʷ-sω : ↑ sucᵘ π *ʷ ωᵘ       ⟨ ⟿ᵗ-At′ _ ⟩ ωᵘ
  *ʷ-ωs : ωᵘ       *ʷ ↑ sucᵘ π ⟨ ⟿ᵗ-At′ _ ⟩ ωᵘ
  *ʷ-ωω : ωᵘ       *ʷ ωᵘ       ⟨ ⟿ᵗ-At′ _ ⟩ ωᵘ

data ⟿ᵉ-At′ n where
  β-∙ : (𝛌 t ⦂ 𝚷[ π / S ] T) ∙ s ⟨ ⟿ᵉ-At′ _ ⟩ substᵉ (t ⦂ T) (s ⦂ S)

  β-𝓤-0 : 𝓤-elim T ρ ρᵀ z s 0ᵘ ⟨ ⟿ᵉ-At′ _ ⟩ z ⦂ substᵗ T (0ᵘ ⦂ 𝓤)
  β-𝓤-s : 𝓤-elim T ρ ρᵀ z s (sucᵘ π) ⟨ ⟿ᵉ-At′ _ ⟩
    let s′ = substᵗ (substᵗ s (weakᵗ π ⦂ 𝓤)) (𝓤-elim T ρ ρᵀ z s π)
        T′ = substᵗ T (sucᵘ π ⦂ 𝓤) in
    s′ ⦂ T′

  β-𝓤ω-↑ : 𝓤ω-elim T ρ d w (↑ π) ⟨ ⟿ᵉ-At′ _ ⟩
             substᵗ d (π ⦂ 𝓤) ⦂ substᵗ T (↑ π ⦂ 𝓤ω)
  β-𝓤ω-ω : 𝓤ω-elim T ρ d w ωᵘ    ⟨ ⟿ᵉ-At′ _ ⟩
             w ⦂ substᵗ T (ωᵘ ⦂ 𝓤ω)


StepOfKind : (𝒯 : SynKind) → Rel (⌈ 𝒯 ⌉ n) lzero
StepOfKind `Term   = ⟿ᵗ-At′ _
StepOfKind `Elim   = ⟿ᵉ-At′ _
StepOfKind `Binder = λ _ _ → ⊥


record CongStep (𝒯 : SynKind) (n : ℕ) (X Y : ⌈ 𝒯 ⌉ n) : Set where
  constructor make-cs
  field
    {holeScope}     : ℕ
    {holeKind}      : SynKind
    {context}       : ■⌈ 𝒯 ⌉ n holeScope holeKind
    {source target} : ⌈ holeKind ⌉ holeScope
    congSource      : context ⟦ source ⟧^ 𝒯 ↦ X
    congTarget      : context ⟦ target ⟧^ 𝒯 ↦ Y
    step            : StepOfKind holeKind source target
open CongStep

⟿ᵗ-At : ∀ n → Rel (Term n) _
⟿ᵗ-At = CongStep `Term

⟿ᵉ-At : ∀ n → Rel (Elim n) _
⟿ᵉ-At = CongStep `Elim

⟿ᵇ-At : ∀ n → Rel (Binder n) _
⟿ᵇ-At = CongStep `Binder


congWrapᵗ : {X Y : ⌈ ℋ ⌉ h} (T′ : ■Term n h ℋ) →
            CongStep ℋ h X Y →
            CongStep `Term n (T′ ⟦ X ⟧ᵗ′) (T′ ⟦ Y ⟧ᵗ′)
congWrapᵗ {X = X} {Y = Y} T′ (make-cs cs ct s) =
  make-cs ((T′ ⟦ X ⟧ᵗ) .proj₂ ⊡ᵗ cs) ((T′ ⟦ Y ⟧ᵗ) .proj₂ ⊡ᵗ ct) s

congWrapᵉ : {X Y : ⌈ ℋ ⌉ h} (e′ : ■Elim n h ℋ) →
            CongStep ℋ h X Y →
            CongStep `Elim n (e′ ⟦ X ⟧ᵉ′) (e′ ⟦ Y ⟧ᵉ′)
congWrapᵉ {X = X} {Y = Y} T′ (make-cs cs ct s) =
  make-cs ((T′ ⟦ X ⟧ᵉ) .proj₂ ⊡ᵉ cs) ((T′ ⟦ Y ⟧ᵉ) .proj₂ ⊡ᵉ ct) s

congWrapᵇ : {X Y : ⌈ ℋ ⌉ h} (B′ : ■Binder n h ℋ) →
            CongStep ℋ h X Y →
            CongStep `Binder n (B′ ⟦ X ⟧ᵇ′) (B′ ⟦ Y ⟧ᵇ′)
congWrapᵇ {X = X} {Y = Y} B′ (make-cs cs ct s) =
  make-cs ((B′ ⟦ X ⟧ᵇ) .proj₂ ⊡ᵇ cs) ((B′ ⟦ Y ⟧ᵇ) .proj₂ ⊡ᵇ ct) s

stepHereᵗ : s ⟨ ⟿ᵗ-At′ _ ⟩ t → CongStep _ _ s t
stepHereᵗ = make-cs ■ ■

stepHereᵉ : e ⟨ ⟿ᵉ-At′ _ ⟩ f → CongStep _ _ e f
stepHereᵉ = make-cs ■ ■



open module Evalᵗ = Derived ⟿ᵗ-At public using ()
  renaming (_⟿_ to _⟿ᵗ_ ;
            _⟿+_ to _⟿ᵗ+_ ; _⟿*_ to _⟿ᵗ*_ ; _⟿!_ to _⟿ᵗ!_ ;
            ⟿+-At to ⟿ᵗ+-At ; ⟿*-At to ⟿ᵗ*-At ; ⟿!-At to ⟿ᵗ!-At ;
            _⇓ to _⇓ᵗ ; _≋_ to _≋ᵗ_ ; ≋-At to ≋ᵗ-At)

open module Evalᵉ = Derived ⟿ᵉ-At public using ()
  renaming (_⟿_ to _⟿ᵉ_ ;
            _⟿+_ to _⟿ᵉ+_ ; _⟿*_ to _⟿ᵉ*_ ; _⟿!_ to _⟿ᵉ!_ ;
            ⟿+-At to ⟿ᵉ+-At ; ⟿*-At to ⟿ᵉ*-At ; ⟿!-At to ⟿ᵉ!-At ;
            _⇓ to _⇓ᵉ ; _≋_ to _≋ᵉ_ ; ≋-At to ≋ᵉ-At)

open module Evalᵇ = Derived ⟿ᵇ-At public using ()
  renaming (_⟿_ to _⟿ᵇ_ ;
            _⟿+_ to _⟿ᵇ+_ ; _⟿*_ to _⟿ᵇ*_ ; _⟿!_ to _⟿ᵇ!_ ;
            ⟿+-At to ⟿ᵇ+-At ; ⟿*-At to ⟿ᵇ*-At ; ⟿!-At to ⟿ᵇ!-At ;
            _⇓ to _⇓ᵇ ; _≋_ to _≋ᵇ_ ; ≋-At to ≋ᵇ-At)


congWrap*ᵗ : {X Y : ⌈ ℋ ⌉ h} (T′ : ■Term n h ℋ) →
            Star (CongStep ℋ h) X Y →
            Star (CongStep `Term n) (T′ ⟦ X ⟧ᵗ′) (T′ ⟦ Y ⟧ᵗ′)
congWrap*ᵗ T′ = RT.gmap _ (congWrapᵗ T′)

congWrap*ᵉ : {X Y : ⌈ ℋ ⌉ h} (e′ : ■Elim n h ℋ) →
            Star (CongStep ℋ h) X Y →
            Star (CongStep `Elim n) (e′ ⟦ X ⟧ᵉ′) (e′ ⟦ Y ⟧ᵉ′)
congWrap*ᵉ e′ = RT.gmap _ (congWrapᵉ e′)

congWrap*ᵇ : {X Y : ⌈ ℋ ⌉ h} (B′ : ■Binder n h ℋ) →
            Star (CongStep ℋ h) X Y →
            Star (CongStep `Binder n) (B′ ⟦ X ⟧ᵇ′) (B′ ⟦ Y ⟧ᵇ′)
congWrap*ᵇ B′ = RT.gmap _ (congWrapᵇ B′)


-- the point of these is to factor out some complex pattern matches
-- that stepˣ would otherwise have to repeat for yes and no cases
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
stepᵇ : (B : Binder n) → Dec (∃[ B′ ] (B ⟿ᵇ B′))

stepᵗ (CORE _) = no λ{(_ , make-cs ■ ■ ())}

stepᵗ (BIND B t) with stepᵇ B
... | yes (_ , RB) = yes $ -, congWrapᵗ (BIND-B ■ t) RB
... | no  ¬RB with stepᵗ t
... | yes (_ , Rt) = yes $ -, congWrapᵗ (BIND-t B ■) Rt
... | no  ¬Rt = no nope where
  nope : ∄ (BIND B t ⟿ᵗ_)
  nope (_ , make-cs (BIND-B cs) (BIND-B ct) s) = ¬RB $ -, make-cs cs ct s
  nope (_ , make-cs (BIND-t cs) (BIND-t ct) s) = ¬Rt $ -, make-cs cs ct s

stepᵗ (s + t) with isUsage? s
... | yes is-0 = yes $ -, stepHereᵗ +-0
... | yes is-suc = yes $ -, stepHereᵗ +-s
... | no ¬us with stepᵗ s
... | yes (_ , Rs) = yes $ -, congWrapᵗ (■ +ˡ t) Rs
... | no ¬Rs with stepᵗ t
... | yes (_ , Rt) = yes $ -, congWrapᵗ (s +ʳ ■) Rt
... | no ¬Rt = no nope where
  nope : ∄ (s + t ⟿ᵗ_)
  nope (_ , make-cs ■ ■ +-0) = ¬us is-0
  nope (_ , make-cs ■ ■ +-s) = ¬us is-suc
  nope (_ , make-cs (binˡ cs) (binˡ ct) s) = ¬Rs $ -, make-cs cs ct s
  nope (_ , make-cs (binʳ cs) (binʳ ct) s) = ¬Rt $ -, make-cs cs ct s

stepᵗ (s * t) with isUsage? s
... | yes is-0 = yes $ -, stepHereᵗ *-0
... | yes is-suc = yes $ -, stepHereᵗ *-s
... | no ¬us with stepᵗ s
... | yes (_ , Rs) = yes $ -, congWrapᵗ (■ *ˡ t) Rs
... | no ¬Rs with stepᵗ t
... | yes (_ , Rt) = yes $ -, congWrapᵗ (s *ʳ ■) Rt
... | no ¬Rt = no nope where
  nope : ∄ (s * t ⟿ᵗ_)
  nope (_ , make-cs ■ ■ *-0) = ¬us is-0
  nope (_ , make-cs ■ ■ *-s) = ¬us is-suc
  nope (_ , make-cs (binˡ cs) (binˡ ct) s) = ¬Rs $ -, make-cs cs ct s
  nope (_ , make-cs (binʳ cs) (binʳ ct) s) = ¬Rt $ -, make-cs cs ct s

stepᵗ (s +ʷ t) with are-+ʷ? s t
... | yes ↑↑ = yes $ -, stepHereᵗ +ʷ-↑
... | yes ω- = yes $ -, stepHereᵗ +ʷ-ωˡ
... | yes -ω = yes $ -, stepHereᵗ +ʷ-ωʳ
... | no ¬+ʷ with stepᵗ s
... | yes (_ , Rs) = yes $ -, congWrapᵗ (■ +ʷˡ t) Rs
... | no ¬Rs with stepᵗ t
... | yes (_ , Rt) = yes $ -, congWrapᵗ (s +ʷʳ ■) Rt
... | no ¬Rt = no nope where
  nope : ∄ (s +ʷ t ⟿ᵗ_)
  nope (_ , make-cs ■ ■ +ʷ-↑) = ¬+ʷ ↑↑
  nope (_ , make-cs ■ ■ +ʷ-ωˡ) = ¬+ʷ ω-
  nope (_ , make-cs ■ ■ +ʷ-ωʳ) = ¬+ʷ -ω
  nope (_ , make-cs (binˡ cs) (binˡ ct) s) = ¬Rs $ -, make-cs cs ct s
  nope (_ , make-cs (binʳ cs) (binʳ ct) s) = ¬Rt $ -, make-cs cs ct s

stepᵗ (s *ʷ t) with are-*ʷ? s t
... | yes ↑↑ = yes $ -, stepHereᵗ *ʷ-↑
... | yes 0ω = yes $ -, stepHereᵗ *ʷ-0ω
... | yes ω0 = yes $ -, stepHereᵗ *ʷ-ω0
... | yes sω = yes $ -, stepHereᵗ *ʷ-sω
... | yes ωs = yes $ -, stepHereᵗ *ʷ-ωs
... | yes ωω = yes $ -, stepHereᵗ *ʷ-ωω
... | no ¬*ʷ with stepᵗ s
... | yes (_ , Rs) = yes $ -, congWrapᵗ (■ *ʷˡ t) Rs
... | no ¬Rs with stepᵗ t
... | yes (_ , Rt) = yes $ -, congWrapᵗ (s *ʷʳ ■) Rt
... | no ¬Rt = no nope where
  nope : ∄ (s *ʷ t ⟿ᵗ_)
  nope (_ , make-cs ■ ■ *ʷ-↑)  = ¬*ʷ ↑↑
  nope (_ , make-cs ■ ■ *ʷ-0ω) = ¬*ʷ 0ω
  nope (_ , make-cs ■ ■ *ʷ-ω0) = ¬*ʷ ω0
  nope (_ , make-cs ■ ■ *ʷ-sω) = ¬*ʷ sω
  nope (_ , make-cs ■ ■ *ʷ-ωs) = ¬*ʷ ωs
  nope (_ , make-cs ■ ■ *ʷ-ωω) = ¬*ʷ ωω
  nope (_ , make-cs (binˡ cs) (binˡ ct) s) = ¬Rs $ -, make-cs cs ct s
  nope (_ , make-cs (binʳ cs) (binʳ ct) s) = ¬Rt $ -, make-cs cs ct s


stepᵗ 0ᵘ = no λ{(_ , make-cs ■ ■ ())}

stepᵗ (sucᵘ π) with stepᵗ π
... | yes (_ , Rπ) = yes $ -, congWrapᵗ (sucᵘ ■) Rπ
... | no ¬Rπ = no nope where
  nope : ∄ (sucᵘ π ⟿ᵗ_)
  nope (_ , make-cs (sucᵘ cs) (sucᵘ ct) s) = ¬Rπ $ -, make-cs cs ct s

stepᵗ (↑ π) with stepᵗ π
... | yes (_ , Rπ) = yes $ -, congWrapᵗ (↑ ■) Rπ
... | no ¬Rπ = no nope where
  nope : ∄ (↑ π ⟿ᵗ_)
  nope (_ , make-cs (↑ cs) (↑ ct) s) = ¬Rπ $ -, make-cs cs ct s

stepᵗ ωᵘ = no λ{(_ , make-cs ■ ■ ())}

stepᵗ [ e ] with isTypeAnn? e
... | yes (_ , _ , refl) = yes $ -, stepHereᵗ υ
... | no ¬⦂ with stepᵉ e
... | yes (_ , Re) = yes $ -, congWrapᵗ [ ■ ] Re
... | no ¬Re = no nope where
  nope : ∄ ([ e ] ⟿ᵗ_)
  nope (_ , make-cs ■ ■ υ) = ¬⦂ $ -, -, refl
  nope (_ , make-cs [ cs ] [ ct ] s) = ¬Re $ -, make-cs cs ct s

stepᵉ (` x) = no λ{(_ , make-cs ■ ■ ())}

stepᵉ (f ∙ s) with isTyLam? f
... | yes (_ , _ , _ , _ , refl) = yes $ -, stepHereᵉ β-∙
... | no ¬λ with stepᵉ f
... | yes (_ , Rf) = yes $ -, congWrapᵉ (■ ∙ˡ s) Rf
... | no ¬Rf with stepᵗ s
... | yes (_ , Rs) = yes $ -, congWrapᵉ (f ∙ʳ ■) Rs
... | no ¬Rs = no nope where
  nope : ∄ (f ∙ s ⟿ᵉ_)
  nope (_ , make-cs ■ ■ β-∙) = ¬λ $ -, -, -, -, refl
  nope (_ , make-cs ([∙ˡ] cs) ([∙ˡ] ct) s) = ¬Rf $ -, make-cs cs ct s
  nope (_ , make-cs ([∙ʳ] cs) ([∙ʳ] ct) s) = ¬Rs $ -, make-cs cs ct s

stepᵉ (𝓤-elim T ρ ρᵀ z s π) with isUsage? π
... | yes is-0 = yes $ -, stepHereᵉ β-𝓤-0
... | yes is-suc = yes $ -, stepHereᵉ β-𝓤-s
... | no ¬uπ with stepᵗ T
... | yes (_ , RT) = yes $ -, congWrapᵉ (𝓤-elim-T ■ ρ ρᵀ z s π) RT
... | no ¬RT with stepᵗ ρ
... | yes (_ , Rρ) = yes $ -, congWrapᵉ (𝓤-elim-ρ T ■ ρᵀ z s π) Rρ
... | no ¬Rρ with stepᵗ ρᵀ
... | yes (_ , Rρᵀ) = yes $ -, congWrapᵉ (𝓤-elim-ρᵀ T ρ ■ z s π) Rρᵀ
... | no ¬Rρᵀ with stepᵗ z
... | yes (_ , Rz) = yes $ -, congWrapᵉ (𝓤-elim-z T ρ ρᵀ ■ s π) Rz
... | no ¬Rz with stepᵗ s
... | yes (_ , Rs) = yes $ -, congWrapᵉ (𝓤-elim-s T ρ ρᵀ z ■ π) Rs
... | no ¬Rs with stepᵗ π
... | yes (_ , Rπ) = yes $ -, congWrapᵉ (𝓤-elim-π T ρ ρᵀ z s ■) Rπ
... | no ¬Rπ = no nope where
  nope : ∄ (𝓤-elim T ρ ρᵀ z s π ⟿ᵉ_)
  nope (_ , make-cs ■ ■ β-𝓤-0) = ¬uπ is-0
  nope (_ , make-cs ■ ■ β-𝓤-s) = ¬uπ is-suc
  nope (_ , make-cs (𝓤-elim-T  cs) (𝓤-elim-T  ct) s) = ¬RT  $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤-elim-ρ  cs) (𝓤-elim-ρ  ct) s) = ¬Rρ  $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤-elim-ρᵀ cs) (𝓤-elim-ρᵀ ct) s) = ¬Rρᵀ $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤-elim-z  cs) (𝓤-elim-z  ct) s) = ¬Rz  $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤-elim-s  cs) (𝓤-elim-s  ct) s) = ¬Rs  $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤-elim-π  cs) (𝓤-elim-π  ct) s) = ¬Rπ  $ -, make-cs cs ct s

stepᵉ (𝓤ω-elim T ρ d w π) with isUsageω? π
... | yes is-↑ = yes $ -, stepHereᵉ β-𝓤ω-↑
... | yes is-ω = yes $ -, stepHereᵉ β-𝓤ω-ω
... | no ¬uπ with stepᵗ T
... | yes (_ , RT) = yes $ -, congWrapᵉ (𝓤ω-elim-T ■ ρ d w π) RT
... | no ¬RT with stepᵗ ρ
... | yes (_ , Rρ) = yes $ -, congWrapᵉ (𝓤ω-elim-ρ T ■ d w π) Rρ
... | no ¬Rρ with stepᵗ d
... | yes (_ , Rd) = yes $ -, congWrapᵉ (𝓤ω-elim-d T ρ ■ w π) Rd
... | no ¬Rd with stepᵗ w
... | yes (_ , Rw) = yes $ -, congWrapᵉ (𝓤ω-elim-w T ρ d ■ π) Rw
... | no ¬Rw with stepᵗ π
... | yes (_ , Rπ) = yes $ -, congWrapᵉ (𝓤ω-elim-π T ρ d w ■) Rπ
... | no ¬Rπ = no nope where
  nope : ∄ (𝓤ω-elim T ρ d w π ⟿ᵉ_)
  nope (_ , make-cs ■ ■ β-𝓤ω-↑) = ¬uπ is-↑
  nope (_ , make-cs ■ ■ β-𝓤ω-ω) = ¬uπ is-ω
  nope (_ , make-cs (𝓤ω-elim-T cs) (𝓤ω-elim-T ct) s) = ¬RT $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤ω-elim-ρ cs) (𝓤ω-elim-ρ ct) s) = ¬Rρ $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤ω-elim-d cs) (𝓤ω-elim-d ct) s) = ¬Rd $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤ω-elim-w cs) (𝓤ω-elim-w ct) s) = ¬Rw $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤ω-elim-π cs) (𝓤ω-elim-π ct) s) = ¬Rπ $ -, make-cs cs ct s

stepᵉ (s ⦂ S) with stepᵗ s
... | yes (_ , Rs) = yes $ -, congWrapᵉ (■ ⦂ˡ S) Rs
... | no ¬Rs with stepᵗ S
... | yes (_ , RS) = yes $ -, congWrapᵉ (s ⦂ʳ ■) RS
... | no ¬RS = no nope where
  nope : ∄ (s ⦂ S ⟿ᵉ_)
  nope (_ , make-cs ([⦂ˡ] cs) ([⦂ˡ] ct) s) = ¬Rs $ -, make-cs cs ct s
  nope (_ , make-cs ([⦂ʳ] cs) ([⦂ʳ] ct) s) = ¬RS $ -, make-cs cs ct s

stepᵇ `𝚷[ π / S ] with stepᵗ π
... | yes (_ , Rπ) = yes $ -, congWrapᵇ (`𝚷-π ■ S) Rπ
... | no ¬Rπ with stepᵗ S
... | yes (_ , RS) = yes $ -, congWrapᵇ (`𝚷-S π ■) RS
... | no ¬RS = no nope where
  nope : ∄ (`𝚷[ π / S ] ⟿ᵇ_)
  nope (_ , make-cs (`𝚷-π cs) (`𝚷-π ct) s) = ¬Rπ $ -, make-cs cs ct s
  nope (_ , make-cs (`𝚷-S cs) (`𝚷-S ct) s) = ¬RS $ -, make-cs cs ct s

stepᵇ `𝛌 = no λ{(_ , make-cs ■ ■ ())}

open Evalᵗ.Eval stepᵗ public renaming (eval to evalᵗ)
open Evalᵉ.Eval stepᵉ public renaming (eval to evalᵉ)
open Evalᵇ.Eval stepᵇ public renaming (eval to evalᵇ)


module _ where
  open Relation

  module _ {n} where
    `𝚷-cong : `𝚷[_/_] Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵇ-At n
    `𝚷-cong (make-≋ Rπ₁ Rπ₂) (make-≋ RS₁ RS₂) = make-≋
      (congWrap*ᵇ (`𝚷-π ■ _) Rπ₁ ◅◅ congWrap*ᵇ (`𝚷-S _ ■) RS₁)
      (congWrap*ᵇ (`𝚷-π ■ _) Rπ₂ ◅◅ congWrap*ᵇ (`𝚷-S _ ■) RS₂)

    BIND-cong : BIND Preserves₂ _≋ᵇ_ ⟶ _≋ᵗ_ ⟶ ≋ᵗ-At n
    BIND-cong (make-≋ RB₁ RB₂) (make-≋ RT₁ RT₂) = make-≋
      (congWrap*ᵗ (BIND-B ■ _) RB₁ ◅◅ congWrap*ᵗ (BIND-t _ ■) RT₁)
      (congWrap*ᵗ (BIND-B ■ _) RB₂ ◅◅ congWrap*ᵗ (BIND-t _ ■) RT₂)

    𝚷-cong : 𝚷[_/_]_ Preserves₃ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵗ-At n
    𝚷-cong Rπ RS = BIND-cong $ `𝚷-cong Rπ RS

    𝛌-cong : 𝛌_ Preserves _≋ᵗ_ ⟶ ≋ᵗ-At n
    𝛌-cong = BIND-cong Evalᵇ.≋-refl

    sucᵘ-cong : sucᵘ Preserves _≋ᵗ_ ⟶ ≋ᵗ-At n
    sucᵘ-cong (make-≋ Rπ₁ Rπ₂) = make-≋
      (congWrap*ᵗ (sucᵘ ■) Rπ₁)
      (congWrap*ᵗ (sucᵘ ■) Rπ₂)

    ↑-cong : ↑_ Preserves _≋ᵗ_ ⟶ ≋ᵗ-At n
    ↑-cong (make-≋ Rπ₁ Rπ₂) = make-≋
      (congWrap*ᵗ (↑ ■) Rπ₁)
      (congWrap*ᵗ (↑ ■) Rπ₂)

    bin-cong : _⟪ o ⟫_ Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵗ-At n
    bin-cong {o = o} (make-≋ Rπ₁ Rπ₂) (make-≋ Rρ₁ Rρ₂) = make-≋
      (congWrap*ᵗ (■ ⟪ o ⟫ˡ _) Rπ₁ ◅◅ congWrap*ᵗ (_ ⟪ o ⟫ʳ ■) Rρ₁)
      (congWrap*ᵗ (■ ⟪ o ⟫ˡ _) Rπ₂ ◅◅ congWrap*ᵗ (_ ⟪ o ⟫ʳ ■) Rρ₂)

    +-cong : _+_ Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵗ-At n
    +-cong = bin-cong

    *-cong : _*_ Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵗ-At n
    *-cong = bin-cong

    +ʷ-cong : _+ʷ_ Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵗ-At n
    +ʷ-cong = bin-cong

    *ʷ-cong : _*ʷ_ Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵗ-At n
    *ʷ-cong = bin-cong

    []-cong : [_] Preserves _≋ᵉ_ ⟶ ≋ᵗ-At n
    []-cong (make-≋ Re₁ Re₂) =
      make-≋ (congWrap*ᵗ [ ■ ] Re₁) (congWrap*ᵗ [ ■ ] Re₂)

    ∙-cong : _∙_ Preserves₂ _≋ᵉ_ ⟶ _≋ᵗ_ ⟶ ≋ᵉ-At n
    ∙-cong (make-≋ Rf₁ Rf₂) (make-≋ Rs₁ Rs₂) = make-≋
      (congWrap*ᵉ (■ ∙ˡ _) Rf₁ ◅◅ congWrap*ᵉ (_ ∙ʳ ■) Rs₁)
      (congWrap*ᵉ (■ ∙ˡ _) Rf₂ ◅◅ congWrap*ᵉ (_ ∙ʳ ■) Rs₂)

    𝓤-elim-cong : 𝓤-elim Preserves₆
                  _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵉ-At n
    𝓤-elim-cong (make-≋ RT₁ RT₂) (make-≋ Rρ₁ Rρ₂) (make-≋ Rρᵀ₁ Rρᵀ₂)
                (make-≋ Rz₁ Rz₂) (make-≋ Rs₁ Rs₂) (make-≋ Rπ₁  Rπ₂) =
      make-≋
        (congWrap*ᵉ (𝓤-elim-T ■ _ _ _ _ _) RT₁
          ◅◅ congWrap*ᵉ (𝓤-elim-ρ _ ■ _ _ _ _) Rρ₁
          ◅◅ congWrap*ᵉ (𝓤-elim-ρᵀ _ _ ■ _ _ _) Rρᵀ₁
          ◅◅ congWrap*ᵉ (𝓤-elim-z _ _ _ ■ _ _) Rz₁
          ◅◅ congWrap*ᵉ (𝓤-elim-s _ _ _ _ ■ _) Rs₁
          ◅◅ congWrap*ᵉ (𝓤-elim-π _ _ _ _ _ ■) Rπ₁)
        (congWrap*ᵉ (𝓤-elim-T ■ _ _ _ _ _) RT₂
          ◅◅ congWrap*ᵉ (𝓤-elim-ρ _ ■ _ _ _ _) Rρ₂
          ◅◅ congWrap*ᵉ (𝓤-elim-ρᵀ _ _ ■ _ _ _) Rρᵀ₂
          ◅◅ congWrap*ᵉ (𝓤-elim-z _ _ _ ■ _ _) Rz₂
          ◅◅ congWrap*ᵉ (𝓤-elim-s _ _ _ _ ■ _) Rs₂
          ◅◅ congWrap*ᵉ (𝓤-elim-π _ _ _ _ _ ■) Rπ₂)

    𝓤ω-elim-cong : 𝓤ω-elim Preserves₅
                   _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵉ-At n
    𝓤ω-elim-cong (make-≋ RT₁ RT₂) (make-≋ Rρ₁ Rρ₂)
                 (make-≋ Rd₁ Rd₂) (make-≋ Rw₁ Rw₂) (make-≋ Rπ₁ Rπ₂) =
      make-≋
        (congWrap*ᵉ (𝓤ω-elim-T ■ _ _ _ _) RT₁
          ◅◅ congWrap*ᵉ (𝓤ω-elim-ρ _ ■ _ _ _) Rρ₁
          ◅◅ congWrap*ᵉ (𝓤ω-elim-d _ _ ■ _ _) Rd₁
          ◅◅ congWrap*ᵉ (𝓤ω-elim-w _ _ _ ■ _) Rw₁
          ◅◅ congWrap*ᵉ (𝓤ω-elim-π _ _ _ _ ■) Rπ₁)
        (congWrap*ᵉ (𝓤ω-elim-T ■ _ _ _ _) RT₂
          ◅◅ congWrap*ᵉ (𝓤ω-elim-ρ _ ■ _ _ _) Rρ₂
          ◅◅ congWrap*ᵉ (𝓤ω-elim-d _ _ ■ _ _) Rd₂
          ◅◅ congWrap*ᵉ (𝓤ω-elim-w _ _ _ ■ _) Rw₂
          ◅◅ congWrap*ᵉ (𝓤ω-elim-π _ _ _ _ ■) Rπ₂)

    ⦂-cong : _⦂_ Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵉ-At n
    ⦂-cong (make-≋ Rs₁ Rs₂) (make-≋ RS₁ RS₂) = make-≋
      (congWrap*ᵉ (■ ⦂ˡ _) Rs₁ ◅◅ congWrap*ᵉ (_ ⦂ʳ ■) RS₁)
      (congWrap*ᵉ (■ ⦂ˡ _) Rs₂ ◅◅ congWrap*ᵉ (_ ⦂ʳ ■) RS₂)


  open ℕ using () renaming (_+_ to _+ᴺ_ ; _*_ to _*ᴺ_)
  open Evalᵗ

  private
    variable a b c : ℕ

    ⌜_⌝ : ℕ → Term n
    ⌜ a ⌝ = fromNat a

  +-ℕ-⟿ : a +ᴺ b ≡ c → ⌜ a ⌝ + ⌜ b ⌝ ⟨ ⟿ᵗ*-At n ⟩ ⌜ c ⌝
  +-ℕ-⟿ {zero}  refl = stepHereᵗ +-0 ◅ ε
  +-ℕ-⟿ {suc a} refl = stepHereᵗ +-s ◅ congWrap*ᵗ (sucᵘ ■) (+-ℕ-⟿ refl)

  +-ℕ : a +ᴺ b ≡ c → ⌜ a ⌝ + ⌜ b ⌝ ⟨ ≋ᵗ-At n ⟩ ⌜ c ⌝
  +-ℕ = star-≋ ∘ +-ℕ-⟿

  +-ℕ′ : c ≡ a +ᴺ b → ⌜ c ⌝ ⟨ ≋ᵗ-At n ⟩ ⌜ a ⌝ + ⌜ b ⌝
  +-ℕ′ = ≋-sym ∘ +-ℕ ∘ ≡.sym

  *-ℕ-⟿ : a *ᴺ b ≡ c → ⌜ a ⌝ * ⌜ b ⌝ ⟨ ⟿ᵗ*-At n ⟩ ⌜ c ⌝
  *-ℕ-⟿ {zero}      refl = stepHereᵗ *-0 ◅ ε
  *-ℕ-⟿ {suc a} {b} refl rewrite ℕ.+-comm b (a *ᴺ b) =
    stepHereᵗ *-s ◅ congWrap*ᵗ (■ +ˡ ⌜ b ⌝) (*-ℕ-⟿ refl) ◅◅ +-ℕ-⟿ refl

  *-ℕ : a *ᴺ b ≡ c → ⌜ a ⌝ * ⌜ b ⌝ ⟨ ≋ᵗ-At n ⟩ ⌜ c ⌝
  *-ℕ = star-≋ ∘ *-ℕ-⟿

  *-ℕ′ : c ≡ a *ᴺ b → ⌜ c ⌝ ⟨ ≋ᵗ-At n ⟩ ⌜ a ⌝ * ⌜ b ⌝
  *-ℕ′ = ≋-sym ∘ *-ℕ ∘ ≡.sym

  +ʷ-ℕ-⟿ : a +ᴺ b ≡ c → ↑ ⌜ a ⌝ +ʷ ↑ ⌜ b ⌝ ⟨ ⟿ᵗ*-At n ⟩ ↑ ⌜ c ⌝
  +ʷ-ℕ-⟿ E = stepHereᵗ +ʷ-↑ ◅ congWrap*ᵗ (↑ ■) (+-ℕ-⟿ E)

  +ʷ-ℕ : a +ᴺ b ≡ c → ↑ ⌜ a ⌝ +ʷ ↑ ⌜ b ⌝ ⟨ ≋ᵗ-At n ⟩ ↑ ⌜ c ⌝
  +ʷ-ℕ = star-≋ ∘ +ʷ-ℕ-⟿

  +ʷ-ℕ′ : c ≡ a +ᴺ b → ↑ ⌜ c ⌝ ⟨ ≋ᵗ-At n ⟩ ↑ ⌜ a ⌝ +ʷ ↑ ⌜ b ⌝
  +ʷ-ℕ′ = ≋-sym ∘ +ʷ-ℕ ∘ ≡.sym

  *ʷ-ℕ-⟿ : a *ᴺ b ≡ c → ↑ ⌜ a ⌝ *ʷ ↑ ⌜ b ⌝ ⟨ ⟿ᵗ*-At n ⟩ ↑ ⌜ c ⌝
  *ʷ-ℕ-⟿ E = stepHereᵗ *ʷ-↑ ◅ congWrap*ᵗ (↑ ■) (*-ℕ-⟿ E)

  *ʷ-ℕ : a *ᴺ b ≡ c → ↑ ⌜ a ⌝ *ʷ ↑ ⌜ b ⌝ ⟨ ≋ᵗ-At n ⟩ ↑ ⌜ c ⌝
  *ʷ-ℕ = star-≋ ∘ *ʷ-ℕ-⟿

  *ʷ-ℕ′ : c ≡ a *ᴺ b → ↑ ⌜ c ⌝ ⟨ ≋ᵗ-At n ⟩ ↑ ⌜ a ⌝ *ʷ ↑ ⌜ b ⌝
  *ʷ-ℕ′ = ≋-sym ∘ *ʷ-ℕ ∘ ≡.sym


  1-*-⟿ : 1 * π ⟿ᵗ* π
  1-*-⟿ {π = π} =
    stepHereᵗ *-s ◅ congWrapᵗ (■ +ˡ π) (stepHereᵗ *-0) ◅ stepHereᵗ +-0 ◅ ε

  1-* : 1 * π ≋ᵗ π
  1-* = star-≋ 1-*-⟿

  1-*ʷ-⟿ : ↑ 1 *ʷ ↑ π ⟿ᵗ* ↑ π
  1-*ʷ-⟿ = stepHereᵗ *ʷ-↑ ◅ congWrap*ᵗ (↑ ■) 1-*-⟿

  1-*ʷ : ↑ 1 *ʷ ↑ π ≋ᵗ ↑ π
  1-*ʷ = star-≋ 1-*ʷ-⟿

  0-+-⟿ : 0 + π ⟿ᵗ* π
  0-+-⟿ = stepHereᵗ +-0 ◅ ε

  0-+ : 0 + π ≋ᵗ π
  0-+ = star-≋ 0-+-⟿

  0-+ʷ-⟿ : ↑ 0 +ʷ ↑ π ⟿ᵗ* ↑ π
  0-+ʷ-⟿ = stepHereᵗ +ʷ-↑ ◅ congWrap*ᵗ (↑ ■) 0-+-⟿

  0-+ʷ : ↑ 0 +ʷ ↑ π ≋ᵗ ↑ π
  0-+ʷ = star-≋ 0-+ʷ-⟿

