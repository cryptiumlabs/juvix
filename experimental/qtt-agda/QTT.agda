module QTT where

open import Prelude

open import Category.Applicative
import Function.Identity.Categorical as IdC
import Data.Maybe.Categorical as MaybeC

private variable m n : ℕ

Var = Fin
private variable x y : Var n

Universe = ℕ
private variable u v : Universe

data Term n : Set
data Elim n : Set
Type   = Term
Usage  = Term
Usageω = Term


data CoreType : Set where
  `⋆     : (u : Universe) → CoreType
  `𝓤 `𝓤ω : CoreType

data Binder n : Set where
  `𝚷[_/_] : (π : Usageω n) (S : Type n) → Binder n
  `𝛌      : Binder n

data BinOp : Set where
  `+  `*  : BinOp -- finite
  `+ʷ `*ʷ : BinOp -- infinite


-- maybe the usage stuff should be elims since their types can always
-- be inferred as 𝓤
-- (and then the bits be renamed to ChkTerm and InfTerm or something i
-- suppose)

data Term n where
  CORE      : (𝔗 : CoreType) → Type n
  BIND      : (𝔅 : Binder n) → Term (suc n) → Term n
  _⟪_⟫_     : (s : Term n) (o : BinOp) (t : Term n) → Term n
  0ᵘ        : Usage n
  sucᵘ      : (π : Usage n) → Usage n
  ↑_        : (π : Usage n) → Usageω n
  ωᵘ        : Usageω n
  [_]       : (e : Elim n) → Term n
private variable s t : Term n ; S S′ T T′ : Type n ; π ρ ρᵀ : Usage n

infix 1000 ↑_
infix 250 _⟪_⟫_

pattern ⋆ i = CORE (`⋆ i)
pattern 𝓤   = CORE `𝓤
pattern 𝓤ω  = CORE `𝓤ω

pattern _+_  π ρ = π ⟪ `+  ⟫ ρ
pattern _*_  π ρ = π ⟪ `*  ⟫ ρ
pattern _+ʷ_ π ρ = π ⟪ `+ʷ ⟫ ρ
pattern _*ʷ_ π ρ = π ⟪ `*ʷ ⟫ ρ
infixl 300 _+_ _+ʷ_ ; infixl 310 _*_ _*ʷ_

pattern 𝚷[_/_]_ π S T = BIND `𝚷[ π / S ] T
pattern 𝛌_          t = BIND `𝛌          t
infixr 150 𝚷[_/_]_ 𝛌_

data Elim n where
  `_      : (x : Var n) → Elim n
  _∙_     : (f : Elim n) (s : Term n) → Elim n
  𝓤-elim  : (T : Type (suc n)) (ρ ρᵀ : Usageω n) →
            (z : Term n) (s : Term (suc (suc n))) →
            (π : Usage n) → Elim n
  𝓤ω-elim : (T : Type (suc n)) (ρ : Usageω n) →
            (d : Term (suc n)) (w : Term n) →
            (π : Usage n) → Elim n
  _⦂_     : (s : Term n) (S : Type n) → Elim n
infix 1000 `_ ; infixl 400 _∙_ ; infix 100 _⦂_
private variable e f : Elim n

pattern ‶_ x = [ ` x ]
infix 1000 ‶_


data _⩿_ : Rel (Type n) lzero

⩿-At : ∀ n → Rel (Type n) _
⩿-At _ = _⩿_

data _⩿_ where
  ⩿-⋆    : (uv : u ℕ.≤ v) → ⋆ u ⟨ ⩿-At n ⟩ ⋆ v
  ⩿-𝚷    : (ss : S′ ⩿ S) (tt : T ⩿ T′) → 𝚷[ π / S ] T ⩿ 𝚷[ π / S′ ] T′
  ⩿-refl : S ⩿ S
  -- (maybe recurse into other structures?)
infix 4 _⩿_

module _ where
  open Relation

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

-- weakˣ′ x M inserts an extra bound variable between x - 1 and x
weakᵗ′ : Var (suc n) → Term n → Term (suc n)
weakᵉ′ : Var (suc n) → Elim n → Elim (suc n)
weakᵇ′ : Var (suc n) → Binder n → Binder (suc n)
weakᵗ′ x (CORE 𝔗) = CORE 𝔗
weakᵗ′ x (BIND 𝔅 T) = BIND (weakᵇ′ x 𝔅) (weakᵗ′ (suc x) T)
weakᵗ′ x (s ⟪ o ⟫ t) = weakᵗ′ x s ⟪ o ⟫ weakᵗ′ x t
weakᵗ′ x 0ᵘ = 0ᵘ
weakᵗ′ x (sucᵘ π) = sucᵘ (weakᵗ′ x π)
weakᵗ′ x (↑ π) = ↑ weakᵗ′ x π
weakᵗ′ x ωᵘ = ωᵘ
weakᵗ′ x [ e ] = [ weakᵉ′ x e ]
weakᵉ′ x (` y) = ` Fin.punchIn x y
weakᵉ′ x (f ∙ s) = weakᵉ′ x f ∙ weakᵗ′ x s
weakᵉ′ x (𝓤-elim T ρ ρᵀ z s π) =
  let x′ = suc x ; x″ = suc x′ in
  𝓤-elim (weakᵗ′ x′ T) (weakᵗ′ x ρ) (weakᵗ′ x ρᵀ)
         (weakᵗ′ x z) (weakᵗ′ x″ s) (weakᵗ′ x π)
weakᵉ′ x (𝓤ω-elim T ρ d w π) =
  let x′ = suc x in
  𝓤ω-elim (weakᵗ′ x′ T) (weakᵗ′ x ρ)
          (weakᵗ′ x′ d) (weakᵗ′ x w) (weakᵗ′ x π)
weakᵉ′ x (s ⦂ S) = weakᵗ′ x s ⦂ weakᵗ′ x S
weakᵇ′ x `𝚷[ π / S ] = `𝚷[ weakᵗ′ x π / weakᵗ′ x S ]
weakᵇ′ x `𝛌 = `𝛌


module _ {F : Set → Set} (A : RawApplicative F) where
  open RawApplicative A
  -- substˣ″ x M e replaces occurrences of variable x in M with the
  -- result of e (and shuffles the remaining indices accordingly)
  substᵗ″ : Var (suc n) → Term (suc n) → F (Elim n) → F (Term n)
  substᵉ″ : Var (suc n) → Elim (suc n) → F (Elim n) → F (Elim n)
  substᵇ″ : Var (suc n) → Binder (suc n) → F (Elim n) → F (Binder n)
  substᵗ″ x (CORE 𝔗) e = pure $ CORE 𝔗
  substᵗ″ x (BIND 𝔅 T) e =
    pure BIND ⊛ substᵇ″ x 𝔅 e
              ⊛ (substᵗ″ (suc x) T (weakᵉ′ x <$> e))
  substᵗ″ x (s ⟪ o ⟫ t) e =
    pure (_⟪ o ⟫_) ⊛ substᵗ″ x s e ⊛ substᵗ″ x t e
  substᵗ″ x 0ᵘ e = pure 0ᵘ
  substᵗ″ x (sucᵘ π) e = sucᵘ <$> substᵗ″ x π e
  substᵗ″ x (↑ π) e = ↑_ <$> substᵗ″ x π e
  substᵗ″ x ωᵘ e = pure ωᵘ
  substᵗ″ x [ f ] e = [_] <$> substᵉ″ x f e
  substᵉ″ x (` y) e = case x Fin.≟ y of λ where
    (yes _)  → e
    (no x≢y) → pure $ ` Fin.punchOut x≢y
  substᵉ″ x (f ∙ s) e = pure _∙_ ⊛ substᵉ″ x f e ⊛ substᵗ″ x s e
  substᵉ″ x (𝓤-elim T ρ ρᵀ z s π) e =
    let x′ = suc x  ; e′ = weakᵉ′ x  <$> e
        x″ = suc x′ ; e″ = weakᵉ′ x′ <$> e′ in
    pure 𝓤-elim ⊛ substᵗ″ x′ T e′
                ⊛ substᵗ″ x ρ e
                ⊛ substᵗ″ x ρᵀ e
                ⊛ substᵗ″ x  z e ⊛ substᵗ″ x″ s e″
                ⊛ substᵗ″ x  π e
  substᵉ″ x (𝓤ω-elim T ρ d w π) e =
    let x′ = suc x  ; e′ = weakᵉ′ x <$> e in
    pure 𝓤ω-elim ⊛ substᵗ″ x′ T e′
                 ⊛ substᵗ″ x ρ e
                 ⊛ substᵗ″ x′ d e′ ⊛ substᵗ″ x w e
                 ⊛ substᵗ″ x  π e
  substᵉ″ x (s ⦂ S) e = pure _⦂_ ⊛ substᵗ″ x s e ⊛ substᵗ″ x S e
  substᵇ″ x `𝚷[ π / S ] e = pure `𝚷[_/_] ⊛ substᵗ″ x π e ⊛ substᵗ″ x S e
  substᵇ″ x `𝛌 e = pure `𝛌

module Subst {ℓ} {𝒯 ℰ : ℕ → Set ℓ}
             (weak′  : ∀ {n} → Var (suc n) → 𝒯 n → 𝒯 (suc n))
             (subst″ : ∀ {F} → RawApplicative F → ∀ {n} →
                       Var (suc n) → 𝒯 (suc n) → F (ℰ n) → F (𝒯 n))
 where
  weak : 𝒯 n → 𝒯 (suc n)
  weak = weak′ 0

  subst′ : Var (suc n) → 𝒯 (suc n) → ℰ n → 𝒯 n
  subst′ = subst″ IdC.applicative

  subst : 𝒯 (suc n) → ℰ n → 𝒯 n
  subst = subst′ 0

  chop′ : Var (suc n) → 𝒯 (suc n) → Maybe (𝒯 n)
  chop′ x t = subst″ MaybeC.applicative x t nothing

  chop : 𝒯 (suc n) → Maybe (𝒯 n)
  chop = chop′ 0

open Subst weakᵗ′ substᵗ″ public using ()
  renaming (weak to weakᵗ ; subst′ to substᵗ′ ; subst to substᵗ ;
            chop′ to chopᵗ′ ; chop to chopᵗ)

open Subst weakᵉ′ substᵉ″ public using ()
  renaming (weak to weakᵉ ; subst′ to substᵉ′ ; subst to substᵉ ;
            chop′ to chopᵉ′ ; chop to chopᵉ)

open Subst weakᵇ′ substᵇ″ public using ()
  renaming (weak to weakᵇ ; subst′ to substᵇ′ ; subst to substᵇ ;
            chop′ to chopᵇ′ ; chop to chopᵇ)


punchIn-≢ : x ≢ Fin.punchIn x y
punchIn-≢ {x = zero}  {y}     ()
punchIn-≢ {x = suc x} {zero}  ()
punchIn-≢ {x = suc x} {suc y} eq = punchIn-≢ $ Fin.suc-injective eq

punchOutIn : (x≢pi : x ≢ Fin.punchIn x y) → Fin.punchOut x≢pi ≡ y
punchOutIn {x = zero}  {y}     _   = refl
punchOutIn {x = suc x} {zero}  _   = refl
punchOutIn {x = suc x} {suc y} ¬eq = ≡.cong suc (punchOutIn (¬eq ∘ ≡.cong suc))

subst-weakᵗ : (s : Term n) (x : Fin (suc n)) (e : Elim n) →
              substᵗ′ x (weakᵗ′ x s) e ≡ s
subst-weakᵉ : (f : Elim n) (x : Fin (suc n)) (e : Elim n) →
              substᵉ′ x (weakᵉ′ x f) e ≡ f
subst-weakᵇ : (𝔅 : Binder n) (x : Fin (suc n)) (e : Elim n) →
              substᵇ′ x (weakᵇ′ x 𝔅) e ≡ 𝔅
subst-weakᵗ (CORE _) x e = refl
subst-weakᵗ (BIND 𝔅 T) x e
  rewrite subst-weakᵇ 𝔅 x e | subst-weakᵗ T (suc x) (weakᵉ′ x e) = refl
subst-weakᵗ (s ⟪ o ⟫ t) x e
  rewrite subst-weakᵗ s x e | subst-weakᵗ t x e = refl
subst-weakᵗ 0ᵘ x e = refl
subst-weakᵗ (sucᵘ π) x e rewrite subst-weakᵗ π x e = refl
subst-weakᵗ (↑ π) x e rewrite subst-weakᵗ π x e = refl
subst-weakᵗ ωᵘ x e = refl
subst-weakᵗ [ f ] x e rewrite subst-weakᵉ f x e = refl
subst-weakᵉ (` y) x e with x Fin.≟ Fin.punchIn x y
... | yes p = ⊥-elim $ punchIn-≢ p
... | no  x≢pi rewrite punchOutIn x≢pi = refl
subst-weakᵉ (f ∙ s) x e rewrite subst-weakᵉ f x e | subst-weakᵗ s x e = refl
subst-weakᵉ (𝓤-elim T ρ ρᵀ z s π) x e
  rewrite subst-weakᵗ T  (suc x)       (weakᵉ′ x e)
        | subst-weakᵗ ρ  x             e
        | subst-weakᵗ ρᵀ x             e
        | subst-weakᵗ z  x             e
        | subst-weakᵗ s  (suc (suc x)) (weakᵉ′ (suc x) (weakᵉ′ x e))
        | subst-weakᵗ π  x             e = refl
subst-weakᵉ (𝓤ω-elim T ρ d w π) x e
  rewrite subst-weakᵗ T  (suc x) (weakᵉ′ x e)
        | subst-weakᵗ ρ  x             e
        | subst-weakᵗ d  (suc x) (weakᵉ′ x e)
        | subst-weakᵗ w  x       e
        | subst-weakᵗ π  x       e = refl
subst-weakᵉ (s ⦂ S) x e rewrite subst-weakᵗ s x e | subst-weakᵗ S x e = refl
subst-weakᵇ `𝚷[ π / S ] x e
  rewrite subst-weakᵗ π x e | subst-weakᵗ S x e = refl
subst-weakᵇ `𝛌 x e = refl


natToUsage : ℕ → Usage n
natToUsage zero    = 0ᵘ
natToUsage (suc n) = sucᵘ (natToUsage n)

instance number-Term : Number (Usage n)
number-Term .Number.Constraint _ = ⊤
number-Term .Number.fromNat    n = natToUsage n
