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
  e e′ f f′ 𝜋 𝜋′ 𝜌 𝜌′ : Elim n
  B B′ C : Binder n
  o o′ : BinOp n
  • : BinOpKind


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

  step-≋ : _⟿_ ⇒₂ ≋-At n
  step-≋ = star-≋ ∘ (_◅ ε)

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

data ⟿ᵉ-At′ n where
  β-∙ : (𝛌 t ⦂ 𝚷[ π / S ] T) ∙ s ⟨ ⟿ᵉ-At′ _ ⟩ substᵉ (t ⦂ T) (s ⦂ S)

  +-0 : (0ᵘ     ⦂ 𝓤) + ρ ⟨ ⟿ᵉ-At′ _ ⟩ ρ ⦂ 𝓤
  +-s : (sucᵘ π ⦂ 𝓤) + ρ ⟨ ⟿ᵉ-At′ _ ⟩ sucᵘ [ (π ⦂ 𝓤) + ρ ] ⦂ 𝓤

  *-0 : (0ᵘ     ⦂ 𝓤) * ρ ⟨ ⟿ᵉ-At′ _ ⟩ 0ᵘ ⦂ 𝓤
  *-s : (sucᵘ π ⦂ 𝓤) * ρ ⟨ ⟿ᵉ-At′ _ ⟩ (π ⦂ 𝓤) * ρ + ρ

  +ʷ-↑  : (↑ π ⦂ 𝓤ω) +ʷ (↑ ρ ⦂ 𝓤ω) ⟨ ⟿ᵉ-At′ _ ⟩ ↑ [ (π ⦂ 𝓤) + ρ ] ⦂ 𝓤ω
  +ʷ-ωˡ : (ωᵘ  ⦂ 𝓤ω) +ʷ 𝜌          ⟨ ⟿ᵉ-At′ _ ⟩ ωᵘ ⦂ 𝓤ω
  +ʷ-ωʳ : 𝜋          +ʷ (ωᵘ ⦂ 𝓤ω)  ⟨ ⟿ᵉ-At′ _ ⟩ ωᵘ ⦂ 𝓤ω

  *ʷ-↑  : (↑ π  ⦂ 𝓤ω)     *ʷ (↑ ρ  ⦂ 𝓤ω)     ⟨ ⟿ᵉ-At′ _ ⟩ ↑ [ (π ⦂ 𝓤) * ρ ] ⦂ 𝓤ω
  *ʷ-0ω : (↑ 0ᵘ ⦂ 𝓤ω)     *ʷ (ωᵘ   ⦂ 𝓤ω)     ⟨ ⟿ᵉ-At′ _ ⟩ ↑ 0ᵘ ⦂ 𝓤ω
  *ʷ-ω0 : (ωᵘ   ⦂ 𝓤ω)     *ʷ (↑ 0ᵘ ⦂ 𝓤ω)     ⟨ ⟿ᵉ-At′ _ ⟩ ↑ 0ᵘ ⦂ 𝓤ω
  *ʷ-sω : (↑ sucᵘ π ⦂ 𝓤ω) *ʷ (ωᵘ ⦂ 𝓤ω)       ⟨ ⟿ᵉ-At′ _ ⟩ ωᵘ ⦂ 𝓤ω
  *ʷ-ωs : (ωᵘ ⦂ 𝓤ω)       *ʷ (↑ sucᵘ π ⦂ 𝓤ω) ⟨ ⟿ᵉ-At′ _ ⟩ ωᵘ ⦂ 𝓤ω
  *ʷ-ωω : (ωᵘ ⦂ 𝓤ω)       *ʷ (ωᵘ ⦂ 𝓤ω)       ⟨ ⟿ᵉ-At′ _ ⟩ ωᵘ ⦂ 𝓤ω

  β-𝓤-0 : 𝓤-elim T ρ ρᵀ z s (0ᵘ ⦂ 𝓤) ⟨ ⟿ᵉ-At′ _ ⟩ z ⦂ substᵗ T (0ᵘ ⦂ 𝓤)
  β-𝓤-s : 𝓤-elim T ρ ρᵀ z s (sucᵘ π ⦂ 𝓤) ⟨ ⟿ᵉ-At′ _ ⟩
    let s′ = substᵗ (substᵗ s (weakᵗ π ⦂ 𝓤)) (𝓤-elim T ρ ρᵀ z s (π ⦂ 𝓤))
        T′ = substᵗ T (sucᵘ π ⦂ 𝓤) in
    s′ ⦂ T′

  β-𝓤ω-↑ : 𝓤ω-elim T ρ d w (↑ π ⦂ 𝓤ω) ⟨ ⟿ᵉ-At′ _ ⟩
             substᵗ d (π ⦂ 𝓤) ⦂ substᵗ T (↑ π ⦂ 𝓤ω)
  β-𝓤ω-ω : 𝓤ω-elim T ρ d w (ωᵘ ⦂ 𝓤ω) ⟨ ⟿ᵉ-At′ _ ⟩
             w ⦂ substᵗ T (ωᵘ ⦂ 𝓤ω)


StepOfKind : (𝒯 : SynKind) → Rel (⌈ 𝒯 ⌉ n) lzero
StepOfKind `Term   = ⟿ᵗ-At′ _
StepOfKind `Elim   = ⟿ᵉ-At′ _
StepOfKind `Binder = λ _ _ → ⊥
StepOfKind `BinOp  = λ _ _ → ⊥


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

⟿ᵒ-At : ∀ n → Rel (BinOp n) _
⟿ᵒ-At = CongStep `BinOp


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

congWrapᵒ : {X Y : ⌈ ℋ ⌉ h} (o′ : ■BinOp n h ℋ) →
            CongStep ℋ h X Y →
            CongStep `BinOp n (o′ ⟦ X ⟧ᵒ′) (o′ ⟦ Y ⟧ᵒ′)
congWrapᵒ {X = X} {Y = Y} B′ (make-cs cs ct s) =
  make-cs ((B′ ⟦ X ⟧ᵒ) .proj₂ ⊡ᵒ cs) ((B′ ⟦ Y ⟧ᵒ) .proj₂ ⊡ᵒ ct) s

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

open module Evalᵒ = Derived ⟿ᵒ-At public using ()
  renaming (_⟿_ to _⟿ᵒ_ ;
            _⟿+_ to _⟿ᵒ+_ ; _⟿*_ to _⟿ᵒ*_ ; _⟿!_ to _⟿ᵒ!_ ;
            ⟿+-At to ⟿ᵒ+-At ; ⟿*-At to ⟿ᵒ*-At ; ⟿!-At to ⟿ᵒ!-At ;
            _⇓ to _⇓ᵒ ; _≋_ to _≋ᵒ_ ; ≋-At to ≋ᵒ-At)


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

congWrap*ᵒ : {X Y : ⌈ ℋ ⌉ h} (B′ : ■BinOp n h ℋ) →
            Star (CongStep ℋ h) X Y →
            Star (CongStep `BinOp n) (B′ ⟦ X ⟧ᵒ′) (B′ ⟦ Y ⟧ᵒ′)
congWrap*ᵒ B′ = RT.gmap _ (congWrapᵒ B′)


-- the point of these is to factor out some complex pattern matches
-- that stepˣ would otherwise have to repeat for yes and no cases
private
  data Is-0   {n} : Term n → Set where is-0   : Is-0     0ᵘ
  data Is-suc {n} : Term n → Set where is-suc : Is-suc $ sucᵘ π
  data Is-ω   {n} : Term n → Set where is-ω   : Is-ω     ωᵘ
  data Is-↑   {n} : Term n → Set where is-↑   : Is-↑   $ ↑ π

  data IsUsage {n} : Term n → Set where
    is-0   : IsUsage   0ᵘ
    is-suc : IsUsage $ sucᵘ π

  data IsUsageω {n} : Term n → Set where
    is-↑ : IsUsageω $ ↑ π
    is-ω : IsUsageω   ωᵘ

  isUsage? : Decidable₁ $ IsUsage {n}
  isUsage? (CORE _)   = no λ()
  isUsage? (BIND _ _) = no λ()
  isUsage? 0ᵘ         = yes is-0
  isUsage? (sucᵘ _)   = yes is-suc
  isUsage? (↑ _)      = no λ()
  isUsage? ωᵘ         = no λ()
  isUsage? [ _ ]      = no λ()

  isUsageω? : Decidable₁ $ IsUsageω {n}
  isUsageω? (CORE _)   = no λ()
  isUsageω? (BIND _ _) = no λ()
  isUsageω? 0ᵘ         = no λ()
  isUsageω? (sucᵘ _)   = no λ()
  isUsageω? (↑ _)      = yes is-↑
  isUsageω? ωᵘ         = yes is-ω
  isUsageω? [ _ ]      = no λ()

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
  isTypeAnn? (bin _)              = no λ()
  isTypeAnn? (𝓤-elim _ _ _ _ _ _) = no λ()
  isTypeAnn? (𝓤ω-elim _ _ _ _ _)  = no λ()
  isTypeAnn? (s ⦂ S)              = yes (s , S , refl)

  isCore? : (s : Term n) → Dec $ ∃[ K ] (s ≡ CORE K)
  isCore? (CORE K)   = yes $ K , refl
  isCore? (BIND _ _) = no λ()
  isCore? 0ᵘ         = no λ()
  isCore? (sucᵘ _)   = no λ()
  isCore? (↑ _)      = no λ()
  isCore? ωᵘ         = no λ()
  isCore? [ _ ]      = no λ()

  isAnnUsage? : (e : Elim n) → Dec $ ∃[ π ] (e ≡ π ⦂ 𝓤 × IsUsage π)
  isAnnUsage? e with isTypeAnn? e
  ... | no ¬p = no λ{(π , refl , U) → ¬p $ -, -, refl}
  ... | yes (π , S , refl) with isUsage? π | isCore? S
  ... | no ¬U | _ = no (λ{(_ , refl , U) → ¬U U})
  ... | yes U | no ¬C = no λ{(π , refl , U) → ¬C (-, refl)}
  ... | yes U | yes (`⋆ u , refl) = no λ()
  ... | yes U | yes (`𝓤ω , refl) = no λ()
  ... | yes U | yes (`𝓤 , refl) = yes (π , refl , U)

  isAnnUsageω? : (e : Elim n) → Dec $ ∃[ π ] (e ≡ π ⦂ 𝓤ω × IsUsageω π)
  isAnnUsageω? e with isTypeAnn? e
  ... | no ¬p = no λ{(π , refl , U) → ¬p $ -, -, refl}
  ... | yes (π , S , refl) with isUsageω? π | isCore? S
  ... | no ¬U | _ = no (λ{(_ , refl , U) → ¬U U})
  ... | yes U | no ¬C = no λ{(π , refl , U) → ¬C (-, refl)}
  ... | yes U | yes (`⋆ u , refl) = no λ()
  ... | yes U | yes (`𝓤 , refl) = no λ()
  ... | yes U | yes (`𝓤ω , refl) = yes (π , refl , U)

  isBind? : (s : Term n) → Dec (∃[ B ] ∃[ t ] (s ≡ BIND B t))
  isBind? (CORE _)   = no λ()
  isBind? (BIND B s) = yes $ B , s , refl
  isBind? 0ᵘ         = no λ()
  isBind? (sucᵘ _)   = no λ()
  isBind? (↑ _)      = no λ()
  isBind? ωᵘ         = no λ()
  isBind? [ _ ]      = no λ()

  isTyLam? : (e : Elim n) →
             Dec (∃[ s ] ∃[ π ] ∃[ S ] ∃[ T ] (e ≡ 𝛌 s ⦂ 𝚷[ π / S ] T))
  isTyLam? e with isTypeAnn? e
  isTyLam? e | no ¬p = no λ{(_ , _ , _ , _ , refl) → ¬p $ -, -, refl}
  isTyLam? _ | yes (s₁ , s₂ , refl) with isBind? s₁ | isBind? s₂
  ... | no ¬p | _     = no λ{(_ , _ , _ , _ , refl) → ¬p $ -, -, refl}
  ... | yes p | no ¬q = no λ{(_ , _ , _ , _ , refl) → ¬q $ -, -, refl}
  ... | yes (`𝚷[ _ / _ ] , _ , refl) | yes (_ , _ , refl) = no λ()
  ... | yes (`𝛌 , t , refl) | yes (`𝚷[ π / S ] , T , refl) =
    yes (t , π , S , T , refl)
  ... | yes (`𝛌 , _ , refl) | yes (`𝛌 , _ , refl) = no λ()

  data Are-+ʷ {n} : Rel (Usageᴱ n) lzero where
    ↑↑ : Are-+ʷ (↑ π ⦂ 𝓤ω) (↑ ρ ⦂ 𝓤ω)
    ω- : Are-+ʷ (ωᵘ  ⦂ 𝓤ω) 𝜌
    -ω : Are-+ʷ 𝜋          (ωᵘ  ⦂ 𝓤ω)

  are-+ʷ? : Decidable₂ $ Are-+ʷ {n}
  are-+ʷ? π ρ with isAnnUsageω? π | isAnnUsageω? ρ
  ... | yes (_ , refl , is-↑) | yes (_ , refl , is-↑) = yes ↑↑
  ... | yes (_ , refl , is-↑) | yes (_ , refl , is-ω) = yes -ω
  ... | yes (_ , refl , is-↑) | no ¬uρ   = no λ where
    ↑↑ → ¬uρ $ -, refl , is-↑
    -ω → ¬uρ $ -, refl , is-ω
  ... | yes (_ , refl , is-ω) | _        = yes ω-
  ... | no ¬uπ   | yes (_ , refl , is-↑) = no λ where
    ↑↑ → ¬uπ $ -, refl , is-↑
    ω- → ¬uπ $ -, refl , is-ω
  ... | no _     | yes (_ , refl , is-ω) = yes -ω
  ... | no ¬uπ   | no ¬uρ   = no λ where
    ↑↑ → ¬uρ $ -, refl , is-↑
    ω- → ¬uπ $ -, refl , is-ω
    -ω → ¬uρ $ -, refl , is-ω

  data Are-*ʷ {n} : Rel (Usageᴱ n) lzero where
    ↑↑ : Are-*ʷ (↑ π      ⦂ 𝓤ω) (↑ ρ      ⦂ 𝓤ω)
    0ω : Are-*ʷ (↑ 0ᵘ     ⦂ 𝓤ω) (ωᵘ       ⦂ 𝓤ω)
    ω0 : Are-*ʷ (ωᵘ       ⦂ 𝓤ω) (↑ 0ᵘ     ⦂ 𝓤ω)
    sω : Are-*ʷ (↑ sucᵘ π ⦂ 𝓤ω) (ωᵘ       ⦂ 𝓤ω)
    ωs : Are-*ʷ (ωᵘ       ⦂ 𝓤ω) (↑ sucᵘ ρ ⦂ 𝓤ω)
    ωω : Are-*ʷ (ωᵘ       ⦂ 𝓤ω) (ωᵘ       ⦂ 𝓤ω)

  are-*ʷ? : Decidable₂ $ Are-*ʷ {n}
  are-*ʷ? π ρ with isAnnUsageω? π | isAnnUsageω? ρ
  are-*ʷ? _ _ | yes (_ , refl , is-↑) | yes (_ , refl , is-↑) = yes ↑↑
  are-*ʷ? _ _ | yes (_ , refl , is-↑ {π = π}) | yes (_ , refl , is-ω)
    with isUsage? π
  ... | yes is-0 = yes 0ω
  ... | yes is-suc = yes sω
  ... | no ¬uπ = no λ where
    0ω → ¬uπ is-0
    sω → ¬uπ is-suc
  are-*ʷ? _ _ | yes (_ , refl , is-ω) | yes (_ , refl , is-↑ {π = ρ})
    with isUsage? ρ
  ... | yes is-0   = yes ω0
  ... | yes is-suc = yes ωs
  ... | no ¬uρ = no λ where
    ω0 → ¬uρ $ is-0
    ωs → ¬uρ $ is-suc
  are-*ʷ? _ _ | yes (_ , refl , is-ω) | yes (_ , refl , is-ω) = yes ωω
  are-*ʷ? _ _ | yes (_ , refl , is-↑) | no ¬uρ = no λ where
    ↑↑ → ¬uρ $ -, refl , is-↑
    0ω → ¬uρ $ -, refl , is-ω
    sω → ¬uρ $ -, refl , is-ω
  are-*ʷ? _ _ | yes (_ , refl , is-ω) | no ¬p = no λ where
    ω0 → ¬p $ -, refl , is-↑
    ωs → ¬p $ -, refl , is-↑
    ωω → ¬p $ -, refl , is-ω
  are-*ʷ? _ _ | no ¬p | _ = no λ where
    ↑↑ → ¬p $ -, refl , is-↑
    0ω → ¬p $ -, refl , is-↑
    ω0 → ¬p $ -, refl , is-ω
    sω → ¬p $ -, refl , is-↑
    ωs → ¬p $ -, refl , is-ω
    ωω → ¬p $ -, refl , is-ω

  stepᵒ′ : (o : BinOp n) → Dec (∃[ t′ ] (bin o ⟨ ⟿ᵉ-At′ _ ⟩ t′))
  stepᵒ′ (fin `+ 𝜋 ρ) with isAnnUsage? 𝜋
  ... | yes (_ , refl , is-0)   = yes $ -, +-0
  ... | yes (_ , refl , is-suc) = yes $ -, +-s
  ... | no  ¬u𝜋                 = no λ where
    (_ , +-0) → ¬u𝜋 $ -, refl , is-0
    (_ , +-s) → ¬u𝜋 $ -, refl , is-suc
  stepᵒ′ (fin `* 𝜋 ρ) with isAnnUsage? 𝜋
  ... | yes (_ , refl , is-0)   = yes $ -, *-0
  ... | yes (_ , refl , is-suc) = yes $ -, *-s
  ... | no  ¬u𝜋                 = no λ where
    (_ , *-0) → ¬u𝜋 $ -, refl , is-0
    (_ , *-s) → ¬u𝜋 $ -, refl , is-suc
  stepᵒ′ (inf `+ 𝜋 𝜌) with are-+ʷ? 𝜋 𝜌
  ... | yes ↑↑ = yes $ -, +ʷ-↑
  ... | yes ω- = yes $ -, +ʷ-ωˡ
  ... | yes -ω = yes $ -, +ʷ-ωʳ
  ... | no  ¬+ = no λ where
    (_ , +ʷ-↑)  → ¬+ ↑↑
    (_ , +ʷ-ωˡ) → ¬+ ω-
    (_ , +ʷ-ωʳ) → ¬+ -ω
  stepᵒ′ (inf `* 𝜋 𝜌) with are-*ʷ? 𝜋 𝜌
  ... | yes ↑↑ = yes $ -, *ʷ-↑
  ... | yes 0ω = yes $ -, *ʷ-0ω
  ... | yes ω0 = yes $ -, *ʷ-ω0
  ... | yes sω = yes $ -, *ʷ-sω
  ... | yes ωs = yes $ -, *ʷ-ωs
  ... | yes ωω = yes $ -, *ʷ-ωω
  ... | no  ¬* = no λ where
    (_ , *ʷ-↑)  → ¬* ↑↑
    (_ , *ʷ-0ω) → ¬* 0ω
    (_ , *ʷ-ω0) → ¬* ω0
    (_ , *ʷ-sω) → ¬* sω
    (_ , *ʷ-ωs) → ¬* ωs
    (_ , *ʷ-ωω) → ¬* ωω

stepᵗ : (t : Term   n) → Dec (∃[ t′ ] (t ⟿ᵗ t′))
stepᵉ : (e : Elim   n) → Dec (∃[ e′ ] (e ⟿ᵉ e′))
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

-- FIXME factor out the cong bits for fin/inf better
-- (fin has an elim and a term but inf has two elims)
stepᵉ (bin o) with stepᵒ′ o
stepᵉ (bin o) | yes (_ , Ro) = yes $ -, stepHereᵉ Ro
stepᵉ (bin (fin • 𝜋 ρ)) | no ¬Ro with stepᵉ 𝜋
... | yes (_ , R𝜋) = yes $ -, congWrapᵉ (bin (finˡ • ■ ρ)) R𝜋
... | no ¬R𝜋 with stepᵗ ρ
... | yes (_ , Rρ) = yes $ -, congWrapᵉ (bin (finʳ • 𝜋 ■)) Rρ
... | no ¬Rρ = no λ where
  (_ , make-cs ■ ■ s) → ¬Ro $ -, s
  (_ , make-cs (bin (finˡ cs)) (bin (finˡ ct)) s) → ¬R𝜋 $ -, make-cs cs ct s
  (_ , make-cs (bin (finʳ cs)) (bin (finʳ ct)) s) → ¬Rρ $ -, make-cs cs ct s
stepᵉ (bin (inf • 𝜋 𝜌)) | no ¬Ro with stepᵉ 𝜋
... | yes (_ , R𝜋) = yes $ -, congWrapᵉ (bin (infˡ • ■ 𝜌)) R𝜋
... | no ¬R𝜋 with stepᵉ 𝜌
... | yes (_ , R𝜌) = yes $ -, congWrapᵉ (bin (infʳ • 𝜋 ■)) R𝜌
... | no ¬R𝜌 = no λ where
  (_ , make-cs ■ ■ s) → ¬Ro $ -, s
  (_ , make-cs (bin (infˡ cs)) (bin (infˡ ct)) s) → ¬R𝜋 $ -, make-cs cs ct s
  (_ , make-cs (bin (infʳ cs)) (bin (infʳ ct)) s) → ¬R𝜌 $ -, make-cs cs ct s

stepᵉ (𝓤-elim T ρ ρᵀ z s 𝜋) with isAnnUsage? 𝜋
... | yes (_ , refl , is-0)   = yes $ -, stepHereᵉ β-𝓤-0
... | yes (_ , refl , is-suc) = yes $ -, stepHereᵉ β-𝓤-s
... | no ¬u𝜋 with stepᵗ T
... | yes (_ , RT) = yes $ -, congWrapᵉ (𝓤-elim-T ■ ρ ρᵀ z s 𝜋) RT
... | no ¬RT with stepᵗ ρ
... | yes (_ , Rρ) = yes $ -, congWrapᵉ (𝓤-elim-ρ T ■ ρᵀ z s 𝜋) Rρ
... | no ¬Rρ with stepᵗ ρᵀ
... | yes (_ , Rρᵀ) = yes $ -, congWrapᵉ (𝓤-elim-ρᵀ T ρ ■ z s 𝜋) Rρᵀ
... | no ¬Rρᵀ with stepᵗ z
... | yes (_ , Rz) = yes $ -, congWrapᵉ (𝓤-elim-z T ρ ρᵀ ■ s 𝜋) Rz
... | no ¬Rz with stepᵗ s
... | yes (_ , Rs) = yes $ -, congWrapᵉ (𝓤-elim-s T ρ ρᵀ z ■ 𝜋) Rs
... | no ¬Rs with stepᵉ 𝜋
... | yes (_ , R𝜋) = yes $ -, congWrapᵉ (𝓤-elim-𝜋 T ρ ρᵀ z s ■) R𝜋
... | no ¬R𝜋 = no nope where
  nope : ∄ (𝓤-elim T ρ ρᵀ z s 𝜋 ⟿ᵉ_)
  nope (_ , make-cs ■ ■ β-𝓤-0) = ¬u𝜋 $ -, refl , is-0
  nope (_ , make-cs ■ ■ β-𝓤-s) = ¬u𝜋 $ -, refl , is-suc
  nope (_ , make-cs (𝓤-elim-T  cs) (𝓤-elim-T  ct) s) = ¬RT  $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤-elim-ρ  cs) (𝓤-elim-ρ  ct) s) = ¬Rρ  $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤-elim-ρᵀ cs) (𝓤-elim-ρᵀ ct) s) = ¬Rρᵀ $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤-elim-z  cs) (𝓤-elim-z  ct) s) = ¬Rz  $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤-elim-s  cs) (𝓤-elim-s  ct) s) = ¬Rs  $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤-elim-𝜋  cs) (𝓤-elim-𝜋  ct) s) = ¬R𝜋  $ -, make-cs cs ct s

stepᵉ (𝓤ω-elim T ρ d w 𝜋) with isAnnUsageω? 𝜋
... | yes (_ , refl , is-↑) = yes $ -, stepHereᵉ β-𝓤ω-↑
... | yes (_ , refl , is-ω) = yes $ -, stepHereᵉ β-𝓤ω-ω
... | no ¬u𝜋 with stepᵗ T
... | yes (_ , RT) = yes $ -, congWrapᵉ (𝓤ω-elim-T ■ ρ d w 𝜋) RT
... | no ¬RT with stepᵗ ρ
... | yes (_ , Rρ) = yes $ -, congWrapᵉ (𝓤ω-elim-ρ T ■ d w 𝜋) Rρ
... | no ¬Rρ with stepᵗ d
... | yes (_ , Rd) = yes $ -, congWrapᵉ (𝓤ω-elim-d T ρ ■ w 𝜋) Rd
... | no ¬Rd with stepᵗ w
... | yes (_ , Rw) = yes $ -, congWrapᵉ (𝓤ω-elim-w T ρ d ■ 𝜋) Rw
... | no ¬Rw with stepᵉ 𝜋
... | yes (_ , R𝜋) = yes $ -, congWrapᵉ (𝓤ω-elim-𝜋 T ρ d w ■) R𝜋
... | no ¬R𝜋 = no nope where
  nope : ∄ (𝓤ω-elim T ρ d w 𝜋 ⟿ᵉ_)
  nope (_ , make-cs ■ ■ β-𝓤ω-↑) = ¬u𝜋 $ -, refl , is-↑
  nope (_ , make-cs ■ ■ β-𝓤ω-ω) = ¬u𝜋 $ -, refl , is-ω
  nope (_ , make-cs (𝓤ω-elim-T cs) (𝓤ω-elim-T ct) s) = ¬RT $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤ω-elim-ρ cs) (𝓤ω-elim-ρ ct) s) = ¬Rρ $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤ω-elim-d cs) (𝓤ω-elim-d ct) s) = ¬Rd $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤ω-elim-w cs) (𝓤ω-elim-w ct) s) = ¬Rw $ -, make-cs cs ct s
  nope (_ , make-cs (𝓤ω-elim-𝜋 cs) (𝓤ω-elim-𝜋 ct) s) = ¬R𝜋 $ -, make-cs cs ct s

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

    []-cong : [_] Preserves _≋ᵉ_ ⟶ ≋ᵗ-At n
    []-cong (make-≋ Re₁ Re₂) =
      make-≋ (congWrap*ᵗ [ ■ ] Re₁) (congWrap*ᵗ [ ■ ] Re₂)

    ∙-cong : _∙_ Preserves₂ _≋ᵉ_ ⟶ _≋ᵗ_ ⟶ ≋ᵉ-At n
    ∙-cong (make-≋ Rf₁ Rf₂) (make-≋ Rs₁ Rs₂) = make-≋
      (congWrap*ᵉ (■ ∙ˡ _) Rf₁ ◅◅ congWrap*ᵉ (_ ∙ʳ ■) Rs₁)
      (congWrap*ᵉ (■ ∙ˡ _) Rf₂ ◅◅ congWrap*ᵉ (_ ∙ʳ ■) Rs₂)

    bin-cong : bin Preserves _≋ᵒ_ ⟶ ≋ᵉ-At n
    bin-cong (make-≋ Ro₁ Ro₂) =
      make-≋ (congWrap*ᵉ (bin ■) Ro₁) (congWrap*ᵉ (bin ■) Ro₂)

    fin-cong : fin • Preserves₂ _≋ᵉ_ ⟶ _≋ᵗ_ ⟶ ≋ᵒ-At n
    fin-cong (make-≋ R𝜋₁ R𝜋₂) (make-≋ Rρ₁ Rρ₂) = make-≋
      (congWrap*ᵒ (finˡ _ ■ _) R𝜋₁ ◅◅ congWrap*ᵒ (finʳ _ _ ■) Rρ₁)
      (congWrap*ᵒ (finˡ _ ■ _) R𝜋₂ ◅◅ congWrap*ᵒ (finʳ _ _ ■) Rρ₂)

    inf-cong : inf • Preserves₂ _≋ᵉ_ ⟶ _≋ᵉ_ ⟶ ≋ᵒ-At n
    inf-cong (make-≋ R𝜋₁ R𝜋₂) (make-≋ R𝜌₁ R𝜌₂) = make-≋
      (congWrap*ᵒ (infˡ _ ■ _) R𝜋₁ ◅◅ congWrap*ᵒ (infʳ _ _ ■) R𝜌₁)
      (congWrap*ᵒ (infˡ _ ■ _) R𝜋₂ ◅◅ congWrap*ᵒ (infʳ _ _ ■) R𝜌₂)

    +-cong : _+_ Preserves₂ _≋ᵉ_ ⟶ _≋ᵗ_ ⟶ ≋ᵉ-At n
    +-cong E𝜋 Eρ = bin-cong (fin-cong E𝜋 Eρ)

    *-cong : _*_ Preserves₂ _≋ᵉ_ ⟶ _≋ᵗ_ ⟶ ≋ᵉ-At n
    *-cong E𝜋 Eρ = bin-cong (fin-cong E𝜋 Eρ)

    +ʷ-cong : _+ʷ_ Preserves₂ _≋ᵉ_ ⟶ _≋ᵉ_ ⟶ ≋ᵉ-At n
    +ʷ-cong E𝜋 E𝜌 = bin-cong (inf-cong E𝜋 E𝜌)

    *ʷ-cong : _*ʷ_ Preserves₂ _≋ᵉ_ ⟶ _≋ᵉ_ ⟶ ≋ᵉ-At n
    *ʷ-cong E𝜋 E𝜌 = bin-cong (inf-cong E𝜋 E𝜌)

    𝓤-elim-cong : 𝓤-elim Preserves₆
                  _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵉ_ ⟶ ≋ᵉ-At n
    𝓤-elim-cong (make-≋ RT₁ RT₂) (make-≋ Rρ₁ Rρ₂) (make-≋ Rρᵀ₁ Rρᵀ₂)
                (make-≋ Rz₁ Rz₂) (make-≋ Rs₁ Rs₂) (make-≋ Rπ₁  Rπ₂) =
      make-≋
        (congWrap*ᵉ (𝓤-elim-T ■ _ _ _ _ _) RT₁
          ◅◅ congWrap*ᵉ (𝓤-elim-ρ _ ■ _ _ _ _) Rρ₁
          ◅◅ congWrap*ᵉ (𝓤-elim-ρᵀ _ _ ■ _ _ _) Rρᵀ₁
          ◅◅ congWrap*ᵉ (𝓤-elim-z _ _ _ ■ _ _) Rz₁
          ◅◅ congWrap*ᵉ (𝓤-elim-s _ _ _ _ ■ _) Rs₁
          ◅◅ congWrap*ᵉ (𝓤-elim-𝜋 _ _ _ _ _ ■) Rπ₁)
        (congWrap*ᵉ (𝓤-elim-T ■ _ _ _ _ _) RT₂
          ◅◅ congWrap*ᵉ (𝓤-elim-ρ _ ■ _ _ _ _) Rρ₂
          ◅◅ congWrap*ᵉ (𝓤-elim-ρᵀ _ _ ■ _ _ _) Rρᵀ₂
          ◅◅ congWrap*ᵉ (𝓤-elim-z _ _ _ ■ _ _) Rz₂
          ◅◅ congWrap*ᵉ (𝓤-elim-s _ _ _ _ ■ _) Rs₂
          ◅◅ congWrap*ᵉ (𝓤-elim-𝜋 _ _ _ _ _ ■) Rπ₂)

    𝓤ω-elim-cong : 𝓤ω-elim Preserves₅
                   _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ _≋ᵉ_ ⟶ ≋ᵉ-At n
    𝓤ω-elim-cong (make-≋ RT₁ RT₂) (make-≋ Rρ₁ Rρ₂)
                 (make-≋ Rd₁ Rd₂) (make-≋ Rw₁ Rw₂) (make-≋ Rπ₁ Rπ₂) =
      make-≋
        (congWrap*ᵉ (𝓤ω-elim-T ■ _ _ _ _) RT₁
          ◅◅ congWrap*ᵉ (𝓤ω-elim-ρ _ ■ _ _ _) Rρ₁
          ◅◅ congWrap*ᵉ (𝓤ω-elim-d _ _ ■ _ _) Rd₁
          ◅◅ congWrap*ᵉ (𝓤ω-elim-w _ _ _ ■ _) Rw₁
          ◅◅ congWrap*ᵉ (𝓤ω-elim-𝜋 _ _ _ _ ■) Rπ₁)
        (congWrap*ᵉ (𝓤ω-elim-T ■ _ _ _ _) RT₂
          ◅◅ congWrap*ᵉ (𝓤ω-elim-ρ _ ■ _ _ _) Rρ₂
          ◅◅ congWrap*ᵉ (𝓤ω-elim-d _ _ ■ _ _) Rd₂
          ◅◅ congWrap*ᵉ (𝓤ω-elim-w _ _ _ ■ _) Rw₂
          ◅◅ congWrap*ᵉ (𝓤ω-elim-𝜋 _ _ _ _ ■) Rπ₂)

    ⦂-cong : _⦂_ Preserves₂ _≋ᵗ_ ⟶ _≋ᵗ_ ⟶ ≋ᵉ-At n
    ⦂-cong (make-≋ Rs₁ Rs₂) (make-≋ RS₁ RS₂) = make-≋
      (congWrap*ᵉ (■ ⦂ˡ _) Rs₁ ◅◅ congWrap*ᵉ (_ ⦂ʳ ■) RS₁)
      (congWrap*ᵉ (■ ⦂ˡ _) Rs₂ ◅◅ congWrap*ᵉ (_ ⦂ʳ ■) RS₂)


  open ℕ using () renaming (_+_ to _+ᴺ_ ; _*_ to _*ᴺ_)

  private
    variable a b c : ℕ

    ⌜_⌝ : ℕ → Term n
    ⌜ a ⌝ = fromNat a

    ⌜_⌝′ : ℕ → Elim n
    ⌜ a ⌝′ = ⌜ a ⌝ ⦂ 𝓤

  +-ℕ-⟿ : a +ᴺ b ≡ c → ⌜ a ⌝′ + ⌜ b ⌝ ⟨ ⟿ᵉ*-At n ⟩ ⌜ c ⌝′
  +-ℕ-⟿ {zero}  refl = stepHereᵉ +-0 ◅ ε
  +-ℕ-⟿ {suc a} refl =
    stepHereᵉ +-s ◅
    congWrap*ᵉ (sucᵘ ■ ⦂ˡ 𝓤)
      (congWrap*ᵗ [ ■ ] (+-ℕ-⟿ refl) ◅◅
       stepHereᵗ υ ◅ ε)

  +-ℕ : a +ᴺ b ≡ c → ⌜ a ⌝′ + ⌜ b ⌝ ⟨ ≋ᵉ-At n ⟩ ⌜ c ⌝′
  +-ℕ = Evalᵉ.star-≋ ∘ +-ℕ-⟿

  +-ℕ′ : c ≡ a +ᴺ b → ⌜ c ⌝′ ⟨ ≋ᵉ-At n ⟩ ⌜ a ⌝′ + ⌜ b ⌝
  +-ℕ′ = Evalᵉ.≋-sym ∘ +-ℕ ∘ ≡.sym

  *-ℕ-⟿ : a *ᴺ b ≡ c → ⌜ a ⌝′ * ⌜ b ⌝ ⟨ ⟿ᵉ*-At n ⟩ ⌜ c ⌝′
  *-ℕ-⟿ {zero} refl = stepHereᵉ *-0 ◅ ε
  *-ℕ-⟿ {suc a} {b} refl rewrite ℕ.+-comm b (a *ᴺ b) =
    stepHereᵉ *-s ◅
    congWrap*ᵉ (■ +ˡ ⌜ b ⌝) (*-ℕ-⟿ refl) ◅◅
    +-ℕ-⟿ refl

  *-ℕ : a *ᴺ b ≡ c → ⌜ a ⌝′ * ⌜ b ⌝ ⟨ ≋ᵉ-At n ⟩ ⌜ c ⌝′
  *-ℕ = Evalᵉ.star-≋ ∘ *-ℕ-⟿

  *-ℕ′ : c ≡ a *ᴺ b → ⌜ c ⌝′ ⟨ ≋ᵉ-At n ⟩ ⌜ a ⌝′ * ⌜ b ⌝
  *-ℕ′ = Evalᵉ.≋-sym ∘ *-ℕ ∘ ≡.sym

  private
    ↑⌜_⌝′ : ℕ → Elim n
    ↑⌜ a ⌝′ = ↑ ⌜ a ⌝ ⦂ 𝓤ω

  +ʷ-ℕ-⟿ : a +ᴺ b ≡ c → ↑⌜ a ⌝′ +ʷ ↑⌜ b ⌝′ ⟨ ⟿ᵉ*-At n ⟩ ↑⌜ c ⌝′
  +ʷ-ℕ-⟿ E =
    stepHereᵉ +ʷ-↑ ◅
    congWrap*ᵉ (↑ ■ ⦂ˡ 𝓤ω)
      (congWrap*ᵗ [ ■ ] (+-ℕ-⟿ E) ◅◅
       stepHereᵗ υ ◅ ε)

  +ʷ-ℕ : a +ᴺ b ≡ c → ↑⌜ a ⌝′ +ʷ ↑⌜ b ⌝′ ⟨ ≋ᵉ-At n ⟩ ↑⌜ c ⌝′
  +ʷ-ℕ = Evalᵉ.star-≋ ∘ +ʷ-ℕ-⟿

  +ʷ-ℕ′ : c ≡ a +ᴺ b → ↑⌜ c ⌝′ ⟨ ≋ᵉ-At n ⟩ ↑⌜ a ⌝′ +ʷ ↑⌜ b ⌝′
  +ʷ-ℕ′ = Evalᵉ.≋-sym ∘ +ʷ-ℕ ∘ ≡.sym

  *ʷ-ℕ-⟿ : a *ᴺ b ≡ c → ↑⌜ a ⌝′ *ʷ ↑⌜ b ⌝′ ⟨ ⟿ᵉ*-At n ⟩ ↑⌜ c ⌝′
  *ʷ-ℕ-⟿ E =
    stepHereᵉ *ʷ-↑ ◅
    congWrap*ᵉ (↑ ■ ⦂ˡ 𝓤ω)
      (congWrap*ᵗ [ ■ ] (*-ℕ-⟿ E) ◅◅
       stepHereᵗ υ ◅ ε)

  *ʷ-ℕ : a *ᴺ b ≡ c → ↑⌜ a ⌝′ *ʷ ↑⌜ b ⌝′ ⟨ ≋ᵉ-At n ⟩ ↑⌜ c ⌝′
  *ʷ-ℕ = Evalᵉ.star-≋ ∘ *ʷ-ℕ-⟿

  *ʷ-ℕ′ : c ≡ a *ᴺ b → ↑⌜ c ⌝′ ⟨ ≋ᵉ-At n ⟩ ↑⌜ a ⌝′ *ʷ ↑⌜ b ⌝′
  *ʷ-ℕ′ = Evalᵉ.≋-sym ∘ *ʷ-ℕ ∘ ≡.sym


  1-*-⟿ : (1 ⦂ 𝓤) * π ⟿ᵉ* π ⦂ 𝓤
  1-*-⟿ {π = π} =
    stepHereᵉ *-s ◅ congWrapᵉ (■ +ˡ π) (stepHereᵉ *-0) ◅ stepHereᵉ +-0 ◅ ε

  1-* : (1 ⦂ 𝓤) * π ≋ᵉ π ⦂ 𝓤
  1-* = Evalᵉ.star-≋ 1-*-⟿

  1-*ʷ-⟿ : (↑ 1 ⦂ 𝓤ω) *ʷ (↑ π ⦂ 𝓤ω) ⟿ᵉ* ↑ π ⦂ 𝓤ω
  1-*ʷ-⟿ =
    stepHereᵉ *ʷ-↑ ◅
    congWrap*ᵉ (↑ ■ ⦂ˡ 𝓤ω) (congWrap*ᵗ [ ■ ] 1-*-⟿ ◅◅ stepHereᵗ υ ◅ ε)

  1-*ʷ : (↑ 1 ⦂ 𝓤ω) *ʷ (↑ π ⦂ 𝓤ω) ≋ᵉ (↑ π ⦂ 𝓤ω)
  1-*ʷ = Evalᵉ.star-≋ 1-*ʷ-⟿

  0-+-⟿ : (0 ⦂ 𝓤) + π ⟿ᵉ* π ⦂ 𝓤
  0-+-⟿ = stepHereᵉ +-0 ◅ ε

  0-+ : (0 ⦂ 𝓤) + π ≋ᵉ π ⦂ 𝓤
  0-+ = Evalᵉ.star-≋ 0-+-⟿

  0-+ʷ-⟿ : (↑ 0 ⦂ 𝓤ω) +ʷ (↑ π ⦂ 𝓤ω) ⟿ᵉ* ↑ π ⦂ 𝓤ω
  0-+ʷ-⟿ =
    stepHereᵉ +ʷ-↑ ◅
    congWrap*ᵉ (↑ ■ ⦂ˡ 𝓤ω)
      (congWrap*ᵗ [ ■ ] 0-+-⟿ ◅◅ stepHereᵗ υ ◅ ε)

  0-+ʷ : (↑ 0 ⦂ 𝓤ω) +ʷ (↑ π ⦂ 𝓤ω) ≋ᵉ ↑ π ⦂ 𝓤ω
  0-+ʷ = Evalᵉ.star-≋ 0-+ʷ-⟿
