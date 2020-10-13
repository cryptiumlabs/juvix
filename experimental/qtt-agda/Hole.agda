module Hole where

open import Prelude

open import QTT

private
 variable
  n h h₁ h₂ : ℕ
  s t S T π ρ ρᵀ z d w : Term n
  e f : Elim n
  𝜋 𝜌 : Usageᴱ n
  B : Binder n
  o : BinOp n
  • : BinOpKind

data SynKind : Set where `Term `Elim `Binder `BinOp : SynKind
private variable 𝒯 ℋ ℋ₁ ℋ₂ : SynKind

-- Terms (etc) containing a single hole, indicated by ■.
--
-- * n is the size of the scope of the outer term
-- * h is the size of the hole's scope (it might be under some binders)
-- * ℋ is what the hole is in place of
data ■Term   n : (h : ℕ) (ℋ : SynKind) → Set
data ■Elim   n : (h : ℕ) (ℋ : SynKind) → Set
data ■Binder n : (h : ℕ) (ℋ : SynKind) → Set
data ■BinOp  n : (h : ℕ) (ℋ : SynKind) → Set
■Type   = ■Term
■Usage  = ■Term
■Usageω = ■Term
■Typeᴱ   = ■Elim
■Usageᴱ  = ■Elim
■Usageωᴱ = ■Elim


data ■Term n where
  ■       : ■Term n n `Term

  BIND-B : (B′ : ■Binder n h ℋ) (t  :  Term (suc n))     → ■Term n h ℋ
  BIND-t : (B  :  Binder n)     (t′ : ■Term (suc n) h ℋ) → ■Term n h ℋ

  sucᵘ : (π′ : ■Usage n h ℋ) → ■Usage  n h ℋ
  ↑_   : (π′ : ■Usage n h ℋ) → ■Usageω n h ℋ

  [_] : (e′ : ■Elim n h ℋ) → ■Term n h ℋ

infix 1000 ↑_


data ■Elim n where
  ■ : ■Elim n n `Elim

  _∙ˡ_ : (f′ : ■Elim n h ℋ) (s  :  Term n)     → ■Elim n h ℋ
  _∙ʳ_ : (f  :  Elim n)     (s′ : ■Term n h ℋ) → ■Elim n h ℋ

  bin : (o′ : ■BinOp n h ℋ) → ■Elim n h ℋ

  𝓤-elim-T :  (T′ : ■Type (suc n) h ℋ) (ρ ρᵀ : Usageω n) →
              (z : Term n) (s : Term (suc (suc n))) →
              (𝜋 : Elim n) → ■Elim n h ℋ
  𝓤-elim-ρ :  (T : Type (suc n)) (ρ′ : ■Usageω n h ℋ) (ρᵀ : Usageω n) →
              (z : Term n) (s : Term (suc (suc n))) →
              (𝜋 : Elim n) → ■Elim n h ℋ
  𝓤-elim-ρᵀ : (T : Type (suc n)) (ρ : Usageω n) (ρᵀ′ : ■Usageω n h ℋ) →
              (z : Term n) (s : Term (suc (suc n))) →
              (𝜋 : Elim n) → ■Elim n h ℋ
  𝓤-elim-z :  (T : Type (suc n)) (ρ ρᵀ : Usageω n) →
              (z′ : ■Term n h ℋ) (s : Term (suc (suc n))) →
              (𝜋 : Elim n) → ■Elim n h ℋ
  𝓤-elim-s :  (T : Type (suc n)) (ρ ρᵀ : Usageω n) →
              (z : Term n) (s′ : ■Term (suc (suc n)) h ℋ) →
              (𝜋 : Elim n) → ■Elim n h ℋ
  𝓤-elim-𝜋 :  (T : Type (suc n)) (ρ ρᵀ : Usageω n) →
              (z : Term n) (s : Term (suc (suc n))) →
              (𝜋′ : ■Elim n h ℋ) → ■Elim n h ℋ

  𝓤ω-elim-T : (T′ : ■Type (suc n) h ℋ) (ρ : Usageω n) →
              (d : Term (suc n)) (w : Term n) →
              (𝜋 : Elim n) → ■Elim n h ℋ
  𝓤ω-elim-ρ : (T : Type (suc n)) (ρ′ : ■Usageω n h ℋ) →
              (d : Term (suc n)) (w : Term n) →
              (𝜋 : Elim n) → ■Elim n h ℋ
  𝓤ω-elim-d : (T : Type (suc n)) (ρ : Usageω n) →
              (d′ : ■Term (suc n) h ℋ) (w : Term n) →
              (𝜋 : Elim n) → ■Elim n h ℋ
  𝓤ω-elim-w : (T : Type (suc n)) (ρ : Usageω n) →
              (d : Term (suc n)) (w′ : ■Term n h ℋ) →
              (𝜋 : Elim n) → ■Elim n h ℋ
  𝓤ω-elim-𝜋 : (T : Type (suc n)) (ρ : Usageω n) →
              (d : Term (suc n)) (w : Term n) →
              (𝜋′ : ■Elim n h ℋ) → ■Elim n h ℋ

  _⦂ˡ_ : (s′ : ■Term n h ℋ) (S  :  Type n)     → ■Elim n h ℋ
  _⦂ʳ_ : (s  :  Term n)     (S′ : ■Type n h ℋ) → ■Elim n h ℋ

infixl 400 _∙ˡ_ _∙ʳ_
infix  100 _⦂ˡ_ _⦂ʳ_

data ■BinOp n where
  ■ : ■BinOp n n `BinOp

  finˡ : (• : BinOpKind) (𝜋′ : ■Elim n h ℋ) (ρ  :  Term n)     → ■BinOp n h ℋ
  finʳ : (• : BinOpKind) (𝜋  :  Elim n)     (ρ′ : ■Term n h ℋ) → ■BinOp n h ℋ

  infˡ : (• : BinOpKind) (𝜋′ : ■Elim n h ℋ) (𝜌  :  Elim n)     → ■BinOp n h ℋ
  infʳ : (• : BinOpKind) (𝜋  :  Elim n)     (𝜌′ : ■Elim n h ℋ) → ■BinOp n h ℋ


pattern _+ˡ_  s′ t  = bin (finˡ `+ s′ t)
pattern _+ʳ_  s  t′ = bin (finʳ `+ s  t′)
pattern _+ʷˡ_ s′ t  = bin (infˡ `+ s′ t)
pattern _+ʷʳ_ s  t′ = bin (infʳ `+ s  t′)
pattern _*ˡ_  s′ t  = bin (finˡ `* s′ t)
pattern _*ʳ_  s  t′ = bin (finʳ `* s  t′)
pattern _*ʷˡ_ s′ t  = bin (infˡ `* s′ t)
pattern _*ʷʳ_ s  t′ = bin (infʳ `* s  t′)
infixl 300 _+ˡ_ _+ʳ_ _+ʷˡ_ _+ʷʳ_
infixl 310 _*ˡ_ _*ʳ_ _*ʷˡ_ _*ʷʳ_


data ■Binder n where
  ■ : ■Binder n n `Binder

  `𝚷-π : (π′ : ■Term n h ℋ) (S  :  Term n)     → ■Binder n h ℋ
  `𝚷-S : (π  :  Term n)     (S′ : ■Term n h ℋ) → ■Binder n h ℋ

private
 variable
  s′ t′ π′ S′ T′ ρ′ ρᵀ′ z′ d′ w′ : ■Term n h ℋ
  e′ f′ 𝜋′ 𝜌′ : ■Elim n h ℋ
  B′ : ■Binder n h ℋ
  o′ : ■BinOp n h ℋ


⌈_⌉ : SynKind → ℕ → Set
⌈ `Term   ⌉ = Term
⌈ `Elim   ⌉ = Elim
⌈ `Binder ⌉ = Binder
⌈ `BinOp  ⌉ = BinOp

■⌈_⌉ : SynKind → ℕ → ℕ → SynKind → Set
■⌈ `Term   ⌉ = ■Term
■⌈ `Elim   ⌉ = ■Elim
■⌈ `Binder ⌉ = ■Binder
■⌈ `BinOp  ⌉ = ■BinOp

-- Compose two objects with holes—fill a hole with an object which
-- itself has a hole somewhere
_⊙ᵗ_ : ■Term   n h₁ ℋ₁ → ■⌈ ℋ₁ ⌉ h₁ h₂ ℋ₂ → ■Term   n h₂ ℋ₂
_⊙ᵉ_ : ■Elim   n h₁ ℋ₁ → ■⌈ ℋ₁ ⌉ h₁ h₂ ℋ₂ → ■Elim   n h₂ ℋ₂
_⊙ᵇ_ : ■Binder n h₁ ℋ₁ → ■⌈ ℋ₁ ⌉ h₁ h₂ ℋ₂ → ■Binder n h₂ ℋ₂
_⊙ᵒ_ : ■BinOp  n h₁ ℋ₁ → ■⌈ ℋ₁ ⌉ h₁ h₂ ℋ₂ → ■BinOp  n h₂ ℋ₂
infixl 10 _⊙ᵗ_ _⊙ᵉ_ _⊙ᵇ_
■                       ⊙ᵗ X′ = X′
BIND-B B′ t             ⊙ᵗ X′ = BIND-B (B′ ⊙ᵇ X′) t
BIND-t B  t′            ⊙ᵗ X′ = BIND-t B (t′ ⊙ᵗ X′)
sucᵘ π′                 ⊙ᵗ X′ = sucᵘ (π′ ⊙ᵗ X′)
↑ π′                    ⊙ᵗ X′ = ↑ (π′ ⊙ᵗ X′)
[ e′ ]                  ⊙ᵗ X′ = [ e′ ⊙ᵉ X′ ]
■                       ⊙ᵉ X′ = X′
f′ ∙ˡ s                 ⊙ᵉ X′ = (f′ ⊙ᵉ X′) ∙ˡ s
f  ∙ʳ s′                ⊙ᵉ X′ = f ∙ʳ (s′ ⊙ᵗ X′)
bin o′                  ⊙ᵉ X′ = bin (o′ ⊙ᵒ X′)
𝓤-elim-T T′ ρ ρᵀ z s 𝜋  ⊙ᵉ X′ = 𝓤-elim-T (T′ ⊙ᵗ X′) ρ ρᵀ z s 𝜋
𝓤-elim-ρ T ρ′ ρᵀ z s 𝜋  ⊙ᵉ X′ = 𝓤-elim-ρ T (ρ′ ⊙ᵗ X′) ρᵀ z s 𝜋
𝓤-elim-ρᵀ T ρ ρᵀ′ z s 𝜋 ⊙ᵉ X′ = 𝓤-elim-ρᵀ T ρ (ρᵀ′ ⊙ᵗ X′) z s 𝜋
𝓤-elim-z T ρ ρᵀ z′ s 𝜋  ⊙ᵉ X′ = 𝓤-elim-z T ρ ρᵀ (z′ ⊙ᵗ X′) s 𝜋
𝓤-elim-s T ρ ρᵀ z s′ 𝜋  ⊙ᵉ X′ = 𝓤-elim-s T ρ ρᵀ z (s′ ⊙ᵗ X′) 𝜋
𝓤-elim-𝜋 T ρ ρᵀ z s 𝜋′  ⊙ᵉ X′ = 𝓤-elim-𝜋 T ρ ρᵀ z s (𝜋′ ⊙ᵉ X′)
𝓤ω-elim-T T′ ρ d w 𝜋    ⊙ᵉ X′ = 𝓤ω-elim-T (T′ ⊙ᵗ X′) ρ d w 𝜋
𝓤ω-elim-ρ T ρ′ d w 𝜋    ⊙ᵉ X′ = 𝓤ω-elim-ρ T (ρ′ ⊙ᵗ X′) d w 𝜋
𝓤ω-elim-d T ρ d′ w 𝜋    ⊙ᵉ X′ = 𝓤ω-elim-d T ρ (d′ ⊙ᵗ X′) w 𝜋
𝓤ω-elim-w T ρ d w′ 𝜋    ⊙ᵉ X′ = 𝓤ω-elim-w T ρ d (w′ ⊙ᵗ X′) 𝜋
𝓤ω-elim-𝜋 T ρ d w 𝜋′    ⊙ᵉ X′ = 𝓤ω-elim-𝜋 T ρ d w (𝜋′ ⊙ᵉ X′)
s′ ⦂ˡ S                 ⊙ᵉ X′ = (s′ ⊙ᵗ X′) ⦂ˡ S
s ⦂ʳ S′                 ⊙ᵉ X′ = s ⦂ʳ (S′ ⊙ᵗ X′)
■                       ⊙ᵇ X′ = X′
`𝚷-π π′ S               ⊙ᵇ X′ = `𝚷-π (π′ ⊙ᵗ X′) S
`𝚷-S π S′               ⊙ᵇ X′ = `𝚷-S π (S′ ⊙ᵗ X′)
■                       ⊙ᵒ X′ = X′
finˡ • 𝜋′ ρ             ⊙ᵒ X′ = finˡ • (𝜋′ ⊙ᵉ X′) ρ
finʳ • 𝜋 ρ′             ⊙ᵒ X′ = finʳ • 𝜋 (ρ′ ⊙ᵗ X′)
infˡ • 𝜋′ 𝜌             ⊙ᵒ X′ = infˡ • (𝜋′ ⊙ᵉ X′) 𝜌
infʳ • 𝜋 𝜌′             ⊙ᵒ X′ = infʳ • 𝜋 (𝜌′ ⊙ᵉ X′)

-- The relation 'T′ ⟦ X ⟧ˣ↦ T' means that 'T' is the result of filling
-- in the hole in 'T′' with 'X'. They're written as relations instead
-- of functions so that later proofs can match on the cases (like
-- Coq's 'functional induction', but by hand ☹).
--
-- (the repeated 'X' binders can't be put into a variable declaration
-- because doing so ends up with about forty(‼) unsolved metas per
-- constructor)
data _⟦_⟧ᵗ↦_ : ■Term   n h ℋ → ⌈ ℋ ⌉ h → Term   n → Set
data _⟦_⟧ᵉ↦_ : ■Elim   n h ℋ → ⌈ ℋ ⌉ h → Elim   n → Set
data _⟦_⟧ᵇ↦_ : ■Binder n h ℋ → ⌈ ℋ ⌉ h → Binder n → Set
data _⟦_⟧ᵒ↦_ : ■BinOp  n h ℋ → ⌈ ℋ ⌉ h → BinOp  n → Set
infix 10 _⟦_⟧ᵗ↦_ _⟦_⟧ᵉ↦_ _⟦_⟧ᵇ↦_ _⟦_⟧ᵒ↦_

data _⟦_⟧ᵗ↦_ where
  ■ : ■ ⟦ t ⟧ᵗ↦ t

  BIND-B : {X : ⌈ ℋ ⌉ h} →
           B′ ⟦ X ⟧ᵇ↦ B →
           BIND-B B′ t  ⟦ X ⟧ᵗ↦ BIND B t
  BIND-t : {X : ⌈ ℋ ⌉ h} →
           t′ ⟦ X ⟧ᵗ↦ t →
           BIND-t B t′ ⟦ X ⟧ᵗ↦ BIND B t

  sucᵘ : {X : ⌈ ℋ ⌉ h} →
         π′ ⟦ X ⟧ᵗ↦ π →
         sucᵘ π′ ⟦ X ⟧ᵗ↦ sucᵘ π

  ↑_ : {X : ⌈ ℋ ⌉ h} →
       π′ ⟦ X ⟧ᵗ↦ π →
       ↑ π′ ⟦ X ⟧ᵗ↦ ↑ π

  [_] : {X : ⌈ ℋ ⌉ h} →
        e′ ⟦ X ⟧ᵉ↦ e →
        [ e′ ] ⟦ X ⟧ᵗ↦ [ e ]

data _⟦_⟧ᵉ↦_ where
  ■ : ■ ⟦ e ⟧ᵉ↦ e

  [∙ˡ] : {X : ⌈ ℋ ⌉ h} →
         f′ ⟦ X ⟧ᵉ↦ f →
         f′ ∙ˡ s ⟦ X ⟧ᵉ↦ f ∙ s
  [∙ʳ] : {X : ⌈ ℋ ⌉ h} →
         s′ ⟦ X ⟧ᵗ↦ s →
         f ∙ʳ s′ ⟦ X ⟧ᵉ↦ f ∙ s

  bin : {X : ⌈ ℋ ⌉ h} →
        o′ ⟦ X ⟧ᵒ↦ o →
        bin o′ ⟦ X ⟧ᵉ↦ bin o

  𝓤-elim-T : {X : ⌈ ℋ ⌉ h} →
             T′ ⟦ X ⟧ᵗ↦ T →
             𝓤-elim-T T′ ρ ρᵀ z s 𝜋 ⟦ X ⟧ᵉ↦ 𝓤-elim T ρ ρᵀ z s 𝜋
  𝓤-elim-ρ : {X : ⌈ ℋ ⌉ h} →
             ρ′ ⟦ X ⟧ᵗ↦ ρ →
             𝓤-elim-ρ T ρ′ ρᵀ z s 𝜋 ⟦ X ⟧ᵉ↦ 𝓤-elim T ρ ρᵀ z s 𝜋
  𝓤-elim-ρᵀ : {X : ⌈ ℋ ⌉ h} →
              ρᵀ′ ⟦ X ⟧ᵗ↦ ρᵀ →
              𝓤-elim-ρᵀ T ρ ρᵀ′ z s 𝜋 ⟦ X ⟧ᵉ↦ 𝓤-elim T ρ ρᵀ z s 𝜋
  𝓤-elim-z : {X : ⌈ ℋ ⌉ h} →
             z′ ⟦ X ⟧ᵗ↦ z →
             𝓤-elim-z T ρ ρᵀ z′ s 𝜋 ⟦ X ⟧ᵉ↦ 𝓤-elim T ρ ρᵀ z s 𝜋
  𝓤-elim-s : {X : ⌈ ℋ ⌉ h} →
             s′ ⟦ X ⟧ᵗ↦ s →
             𝓤-elim-s T ρ ρᵀ z s′ 𝜋 ⟦ X ⟧ᵉ↦ 𝓤-elim T ρ ρᵀ z s 𝜋
  𝓤-elim-𝜋 : {X : ⌈ ℋ ⌉ h} →
             𝜋′ ⟦ X ⟧ᵉ↦ 𝜋 →
             𝓤-elim-𝜋 T ρ ρᵀ z s 𝜋′ ⟦ X ⟧ᵉ↦ 𝓤-elim T ρ ρᵀ z s 𝜋

  𝓤ω-elim-T : {X : ⌈ ℋ ⌉ h} →
              T′ ⟦ X ⟧ᵗ↦ T →
              𝓤ω-elim-T T′ ρ d w 𝜋 ⟦ X ⟧ᵉ↦ 𝓤ω-elim T ρ d w 𝜋
  𝓤ω-elim-ρ : {X : ⌈ ℋ ⌉ h} →
              ρ′ ⟦ X ⟧ᵗ↦ ρ →
              𝓤ω-elim-ρ T ρ′ d w 𝜋 ⟦ X ⟧ᵉ↦ 𝓤ω-elim T ρ d w 𝜋
  𝓤ω-elim-d : {X : ⌈ ℋ ⌉ h} →
              d′ ⟦ X ⟧ᵗ↦ d →
              𝓤ω-elim-d T ρ d′ w 𝜋 ⟦ X ⟧ᵉ↦ 𝓤ω-elim T ρ d w 𝜋
  𝓤ω-elim-w : {X : ⌈ ℋ ⌉ h} →
              w′ ⟦ X ⟧ᵗ↦ w →
              𝓤ω-elim-w T ρ d w′ 𝜋 ⟦ X ⟧ᵉ↦ 𝓤ω-elim T ρ d w 𝜋
  𝓤ω-elim-𝜋 : {X : ⌈ ℋ ⌉ h} →
              𝜋′ ⟦ X ⟧ᵉ↦ 𝜋 →
              𝓤ω-elim-𝜋 T ρ d w 𝜋′ ⟦ X ⟧ᵉ↦ 𝓤ω-elim T ρ d w 𝜋

  [⦂ˡ] : {X : ⌈ ℋ ⌉ h} →
         s′ ⟦ X ⟧ᵗ↦ s →
         s′ ⦂ˡ S ⟦ X ⟧ᵉ↦ s ⦂ S
  [⦂ʳ] : {X : ⌈ ℋ ⌉ h} →
         S′ ⟦ X ⟧ᵗ↦ S →
         s ⦂ʳ S′ ⟦ X ⟧ᵉ↦ s ⦂ S

data _⟦_⟧ᵇ↦_ where
  ■ : ■ ⟦ B ⟧ᵇ↦ B

  `𝚷-π : {X : ⌈ ℋ ⌉ h} →
         π′ ⟦ X ⟧ᵗ↦ π →
         `𝚷-π π′ S ⟦ X ⟧ᵇ↦ `𝚷[ π / S ]
  `𝚷-S : {X : ⌈ ℋ ⌉ h} →
         S′ ⟦ X ⟧ᵗ↦ S →
         `𝚷-S π S′ ⟦ X ⟧ᵇ↦ `𝚷[ π / S ]

data _⟦_⟧ᵒ↦_ where
  ■ : ■ ⟦ o ⟧ᵒ↦ o

  finˡ : {X : ⌈ ℋ ⌉ h} →
         𝜋′ ⟦ X ⟧ᵉ↦ 𝜋 →
         finˡ • 𝜋′ ρ ⟦ X ⟧ᵒ↦ fin • 𝜋 ρ
  finʳ : {X : ⌈ ℋ ⌉ h} →
         ρ′ ⟦ X ⟧ᵗ↦ ρ →
         finʳ • 𝜋 ρ′ ⟦ X ⟧ᵒ↦ fin • 𝜋 ρ

  infˡ : {X : ⌈ ℋ ⌉ h} →
         𝜋′ ⟦ X ⟧ᵉ↦ 𝜋 →
         infˡ • 𝜋′ 𝜌 ⟦ X ⟧ᵒ↦ inf • 𝜋 𝜌
  infʳ : {X : ⌈ ℋ ⌉ h} →
         𝜌′ ⟦ X ⟧ᵉ↦ 𝜌 →
         infʳ • 𝜋 𝜌′ ⟦ X ⟧ᵒ↦ inf • 𝜋 𝜌


-- These are the actual functions that compute the filled-in forms,
-- along with proofs of the above relations.
_⟦_⟧ᵗ : (t′ : ■Term   n h ℋ) (X : ⌈ ℋ ⌉ h) → ∃[ t ] (t′ ⟦ X ⟧ᵗ↦ t)
_⟦_⟧ᵉ : (e′ : ■Elim   n h ℋ) (X : ⌈ ℋ ⌉ h) → ∃[ e ] (e′ ⟦ X ⟧ᵉ↦ e)
_⟦_⟧ᵇ : (B′ : ■Binder n h ℋ) (X : ⌈ ℋ ⌉ h) → ∃[ B ] (B′ ⟦ X ⟧ᵇ↦ B)
_⟦_⟧ᵒ : (o′ : ■BinOp  n h ℋ) (X : ⌈ ℋ ⌉ h) → ∃[ o ] (o′ ⟦ X ⟧ᵒ↦ o)
■                       ⟦ t ⟧ᵗ = t , ■
BIND-B B′ t             ⟦ X ⟧ᵗ = -, BIND-B ((B′ ⟦ X ⟧ᵇ) .proj₂)
BIND-t B t′             ⟦ X ⟧ᵗ = -, BIND-t ((t′ ⟦ X ⟧ᵗ) .proj₂)
sucᵘ π′                 ⟦ X ⟧ᵗ = -, sucᵘ ((π′ ⟦ X ⟧ᵗ) .proj₂)
↑ π′                    ⟦ X ⟧ᵗ = -, ↑ ((π′ ⟦ X ⟧ᵗ) .proj₂)
[ e′ ]                  ⟦ X ⟧ᵗ = -, [ (e′ ⟦ X ⟧ᵉ) .proj₂ ]
■                       ⟦ e ⟧ᵉ = e , ■
f′ ∙ˡ s                 ⟦ X ⟧ᵉ = -, [∙ˡ] ((f′ ⟦ X ⟧ᵉ) .proj₂)
f  ∙ʳ s′                ⟦ X ⟧ᵉ = -, [∙ʳ] ((s′ ⟦ X ⟧ᵗ) .proj₂)
bin o′                  ⟦ X ⟧ᵉ = -, bin ((o′ ⟦ X ⟧ᵒ) .proj₂)
𝓤-elim-T T′ ρ ρᵀ z s 𝜋  ⟦ X ⟧ᵉ = -, 𝓤-elim-T ((T′ ⟦ X ⟧ᵗ) .proj₂)
𝓤-elim-ρ T ρ′ ρᵀ z s 𝜋  ⟦ X ⟧ᵉ = -, 𝓤-elim-ρ ((ρ′ ⟦ X ⟧ᵗ) .proj₂)
𝓤-elim-ρᵀ T ρ ρᵀ′ z s 𝜋 ⟦ X ⟧ᵉ = -, 𝓤-elim-ρᵀ ((ρᵀ′ ⟦ X ⟧ᵗ) .proj₂)
𝓤-elim-z T ρ ρᵀ z′ s 𝜋  ⟦ X ⟧ᵉ = -, 𝓤-elim-z ((z′ ⟦ X ⟧ᵗ) .proj₂)
𝓤-elim-s T ρ ρᵀ z s′ 𝜋  ⟦ X ⟧ᵉ = -, 𝓤-elim-s ((s′ ⟦ X ⟧ᵗ) .proj₂)
𝓤-elim-𝜋 T ρ ρᵀ z s 𝜋′  ⟦ X ⟧ᵉ = -, 𝓤-elim-𝜋 ((𝜋′ ⟦ X ⟧ᵉ) .proj₂)
𝓤ω-elim-T T′ ρ d w 𝜋    ⟦ X ⟧ᵉ = -, 𝓤ω-elim-T ((T′ ⟦ X ⟧ᵗ) .proj₂)
𝓤ω-elim-ρ T ρ′ d w 𝜋    ⟦ X ⟧ᵉ = -, 𝓤ω-elim-ρ ((ρ′ ⟦ X ⟧ᵗ) .proj₂)
𝓤ω-elim-d T ρ d′ w 𝜋    ⟦ X ⟧ᵉ = -, 𝓤ω-elim-d ((d′ ⟦ X ⟧ᵗ) .proj₂)
𝓤ω-elim-w T ρ d w′ 𝜋    ⟦ X ⟧ᵉ = -, 𝓤ω-elim-w ((w′ ⟦ X ⟧ᵗ) .proj₂)
𝓤ω-elim-𝜋 T ρ d w 𝜋′    ⟦ X ⟧ᵉ = -, 𝓤ω-elim-𝜋 ((𝜋′ ⟦ X ⟧ᵉ) .proj₂)
s′ ⦂ˡ S                 ⟦ X ⟧ᵉ = -, [⦂ˡ] ((s′ ⟦ X ⟧ᵗ) .proj₂)
s  ⦂ʳ S′                ⟦ X ⟧ᵉ = -, [⦂ʳ] ((S′ ⟦ X ⟧ᵗ) .proj₂)
■                       ⟦ B ⟧ᵇ = B , ■
`𝚷-π π′ S               ⟦ X ⟧ᵇ = -, `𝚷-π ((π′ ⟦ X ⟧ᵗ) .proj₂)
`𝚷-S π S′               ⟦ X ⟧ᵇ = -, `𝚷-S ((S′ ⟦ X ⟧ᵗ) .proj₂)
■                       ⟦ o ⟧ᵒ = o , ■
finˡ • 𝜋′ ρ             ⟦ X ⟧ᵒ = -, finˡ ((𝜋′ ⟦ X ⟧ᵉ) .proj₂)
finʳ • 𝜋 ρ′             ⟦ X ⟧ᵒ = -, finʳ ((ρ′ ⟦ X ⟧ᵗ) .proj₂)
infˡ • 𝜋′ 𝜌             ⟦ X ⟧ᵒ = -, infˡ ((𝜋′ ⟦ X ⟧ᵉ) .proj₂)
infʳ • 𝜋 𝜌′             ⟦ X ⟧ᵒ = -, infʳ ((𝜌′ ⟦ X ⟧ᵉ) .proj₂)
infix 10 _⟦_⟧ᵗ _⟦_⟧ᵉ _⟦_⟧ᵇ _⟦_⟧ᵒ


-- Sometimes you only care about the answer and not the proof.
_⟦_⟧ᵗ′ : ■Term n h ℋ → ⌈ ℋ ⌉ h → Term n
T′ ⟦ X ⟧ᵗ′ = (T′ ⟦ X ⟧ᵗ) .proj₁

_⟦_⟧ᵉ′ : ■Elim n h ℋ → ⌈ ℋ ⌉ h → Elim n
e′ ⟦ X ⟧ᵉ′ = (e′ ⟦ X ⟧ᵉ) .proj₁

_⟦_⟧ᵇ′ : ■Binder n h ℋ → ⌈ ℋ ⌉ h → Binder n
B′ ⟦ X ⟧ᵇ′ = (B′ ⟦ X ⟧ᵇ) .proj₁

_⟦_⟧ᵒ′ : ■BinOp n h ℋ → ⌈ ℋ ⌉ h → BinOp n
B′ ⟦ X ⟧ᵒ′ = (B′ ⟦ X ⟧ᵒ) .proj₁


-- This takes a 'SynKind' and returns the predicate from above for
-- that kind.
syntax FillOfKindᴾ 𝒯 T′ X T = T′ ⟦ X ⟧^ 𝒯 ↦ T
FillOfKindᴾ : ∀ 𝒯 → ■⌈ 𝒯 ⌉ n h ℋ → ⌈ ℋ ⌉ h → ⌈ 𝒯 ⌉ n → Set
T′ ⟦ X ⟧^ `Term   ↦ T = T′ ⟦ X ⟧ᵗ↦ T
T′ ⟦ X ⟧^ `Elim   ↦ T = T′ ⟦ X ⟧ᵉ↦ T
T′ ⟦ X ⟧^ `Binder ↦ T = T′ ⟦ X ⟧ᵇ↦ T
T′ ⟦ X ⟧^ `BinOp  ↦ T = T′ ⟦ X ⟧ᵒ↦ T

-- Ditto for the function.
syntax FillOfKindᴰ 𝒯 T′ X = T′ ⟦ X ⟧^ 𝒯
FillOfKindᴰ : ∀ 𝒯 → (X′ : ■⌈ 𝒯 ⌉ n h ℋ) (Y : ⌈ ℋ ⌉ h) →
              ∃[ X ] (X′ ⟦ Y ⟧^ 𝒯 ↦ X)
T′ ⟦ X ⟧^ `Term   = T′ ⟦ X ⟧ᵗ
T′ ⟦ X ⟧^ `Elim   = T′ ⟦ X ⟧ᵉ
T′ ⟦ X ⟧^ `Binder = T′ ⟦ X ⟧ᵇ
T′ ⟦ X ⟧^ `BinOp  = T′ ⟦ X ⟧ᵒ

-- And for the answer-only function.
syntax FillOfKindᴰ′ 𝒯 T′ X = T′ ⟦ X ⟧^ 𝒯 ′
FillOfKindᴰ′ : ∀ 𝒯 → ■⌈ 𝒯 ⌉ n h ℋ → ⌈ ℋ ⌉ h → ⌈ 𝒯 ⌉ n
T′ ⟦ X ⟧^ ℋ ′ = (T′ ⟦ X ⟧^ ℋ) .proj₁


-- Glue two filling proofs together.
-- In other words, filling and composition commute with each other: if
-- * you get 'X' from filling the hole in 'X′' with Y, and
-- * you get 'T' from filling the hole in 'T′' with 'X',
-- then you obtain 'T' from filling in the hole in 'T′ ⊙ˣ X′' with 'Y'.
--
-- If T′ = ░␣␣␣░, X′ = ▒␣▒, and Y = ▓, then
--
--            ▒␣▒ ⟦▓⟧            ░␣␣␣░ ⊙ ▒␣▒
--               ↧                     ∥
--       ░␣␣␣░ ⟦▒▓▒⟧  ↦  ░▒▓▒░  ↤  ░▒␣▒░ ⟦▓⟧.
module _ {X′ : ■⌈ 𝒯 ⌉ h₁ h₂ ℋ} {X : ⌈ 𝒯 ⌉ h₁} {Y : ⌈ ℋ ⌉ h₂} where
  _⊡ᵗ_ : T′ ⟦ X ⟧ᵗ↦ T → X′ ⟦ Y ⟧^ 𝒯 ↦ X → (T′ ⊙ᵗ X′) ⟦ Y ⟧ᵗ↦ T
  _⊡ᵉ_ : e′ ⟦ X ⟧ᵉ↦ e → X′ ⟦ Y ⟧^ 𝒯 ↦ X → (e′ ⊙ᵉ X′) ⟦ Y ⟧ᵉ↦ e
  _⊡ᵇ_ : B′ ⟦ X ⟧ᵇ↦ B → X′ ⟦ Y ⟧^ 𝒯 ↦ X → (B′ ⊙ᵇ X′) ⟦ Y ⟧ᵇ↦ B
  _⊡ᵒ_ : o′ ⟦ X ⟧ᵒ↦ o → X′ ⟦ Y ⟧^ 𝒯 ↦ X → (o′ ⊙ᵒ X′) ⟦ Y ⟧ᵒ↦ o
  ■            ⊡ᵗ X′ = X′
  BIND-B B′    ⊡ᵗ X′ = BIND-B (B′ ⊡ᵇ X′)
  BIND-t T′    ⊡ᵗ X′ = BIND-t (T′ ⊡ᵗ X′)
  sucᵘ T′      ⊡ᵗ X′ = sucᵘ (T′ ⊡ᵗ X′)
  ↑ T′         ⊡ᵗ X′ = ↑ (T′ ⊡ᵗ X′)
  [ e′ ]       ⊡ᵗ X′ = [ e′ ⊡ᵉ X′ ]
  ■            ⊡ᵉ X′ = X′
  [∙ˡ] e′      ⊡ᵉ X′ = [∙ˡ] (e′ ⊡ᵉ X′)
  [∙ʳ] t′      ⊡ᵉ X′ = [∙ʳ] (t′ ⊡ᵗ X′)
  bin o′       ⊡ᵉ X′ = bin (o′ ⊡ᵒ X′)
  𝓤-elim-T t′  ⊡ᵉ X′ = 𝓤-elim-T (t′ ⊡ᵗ X′)
  𝓤-elim-ρ t′  ⊡ᵉ X′ = 𝓤-elim-ρ (t′ ⊡ᵗ X′)
  𝓤-elim-ρᵀ t′ ⊡ᵉ X′ = 𝓤-elim-ρᵀ (t′ ⊡ᵗ X′)
  𝓤-elim-z t′  ⊡ᵉ X′ = 𝓤-elim-z (t′ ⊡ᵗ X′)
  𝓤-elim-s t′  ⊡ᵉ X′ = 𝓤-elim-s (t′ ⊡ᵗ X′)
  𝓤-elim-𝜋 t′  ⊡ᵉ X′ = 𝓤-elim-𝜋 (t′ ⊡ᵉ X′)
  𝓤ω-elim-T t′ ⊡ᵉ X′ = 𝓤ω-elim-T (t′ ⊡ᵗ X′)
  𝓤ω-elim-ρ t′ ⊡ᵉ X′ = 𝓤ω-elim-ρ (t′ ⊡ᵗ X′)
  𝓤ω-elim-d t′ ⊡ᵉ X′ = 𝓤ω-elim-d (t′ ⊡ᵗ X′)
  𝓤ω-elim-w t′ ⊡ᵉ X′ = 𝓤ω-elim-w (t′ ⊡ᵗ X′)
  𝓤ω-elim-𝜋 t′ ⊡ᵉ X′ = 𝓤ω-elim-𝜋 (t′ ⊡ᵉ X′)
  [⦂ˡ] s′      ⊡ᵉ X′ = [⦂ˡ] (s′ ⊡ᵗ X′)
  [⦂ʳ] S′      ⊡ᵉ X′ = [⦂ʳ] (S′ ⊡ᵗ X′)
  ■            ⊡ᵇ X′ = X′
  `𝚷-π π′      ⊡ᵇ X′ = `𝚷-π (π′ ⊡ᵗ X′)
  `𝚷-S S′      ⊡ᵇ X′ = `𝚷-S (S′ ⊡ᵗ X′)
  ■            ⊡ᵒ X′ = X′
  finˡ 𝜋′      ⊡ᵒ X′ = finˡ (𝜋′ ⊡ᵉ X′)
  finʳ ρ′      ⊡ᵒ X′ = finʳ (ρ′ ⊡ᵗ X′)
  infˡ 𝜋′      ⊡ᵒ X′ = infˡ (𝜋′ ⊡ᵉ X′)
  infʳ 𝜌′      ⊡ᵒ X′ = infʳ (𝜌′ ⊡ᵉ X′)
