module QTT where

-- Terms, variable occurrences, etc, are indexed by the number of
-- bound variables in scope

open import Prelude
open ℕ using () renaming (_+_ to _+ᴺ_)

open import Category.Applicative
import Function.Identity.Categorical as IdC
import Data.Maybe.Categorical as MaybeC

private variable m n : ℕ

-- bound variables
Var = Fin
private variable x y : Var n

-- universe levels
Universe = ℕ
private variable u v : Universe

data Term n : Set
data Elim n : Set
Type   = Term
Usage  = Term -- finite usages
Usageω = Term -- maybe-infinite usages
Typeᴱ = Elim ; Usageᴱ = Elim ; Usageωᴱ = Elim


data CoreType : Set where
  `⋆  : (u : Universe) → CoreType -- type of types
  `𝓤  : CoreType -- type of finite usages
  `𝓤ω : CoreType -- type of maybe-infinite usages

data Binder n : Set where
  `𝚷[_/_] : (π : Usageω n) (S : Type n) → Binder n
  `𝛌      : Binder n

data BinOpKind : Set where `+ `* : BinOpKind

data BinOp n : Set where
  fin : (• : BinOpKind) (𝜋   : Usageᴱ n) (ρ : Usage n) → BinOp n -- finite
  inf : (• : BinOpKind) (𝜋 𝜌 : Usageωᴱ n)              → BinOp n -- infinite

-- terms which can be checked against a given type
data Term n where
  -- builtin type ⋆, 𝓤 or 𝓤ω
  CORE      : (K : CoreType) → Type n

  -- binder (λ or Π)
  BIND      : (B : Binder n) → Term (suc n) → Term n

  -- finite usages
  0ᵘ        : Usage n
  sucᵘ      : (π : Usage n) → Usage n

  -- infinite usages
  ↑_        : (π : Usage n) → Usageω n
  ωᵘ        : Usageω n

  -- elimination
  [_]       : (e : Elim n) → Term n
private variable s t : Term n ; S S′ T T′ : Type n ; π ρ ρᵀ : Usage n

infix 1000 ↑_

pattern ⋆ i = CORE (`⋆ i)
pattern 𝓤   = CORE `𝓤
pattern 𝓤ω  = CORE `𝓤ω

pattern 𝚷[_/_]_ π S T = BIND `𝚷[ π / S ] T
pattern 𝛌_          t = BIND `𝛌          t
infixr 150 𝚷[_/_]_ 𝛌_

-- eliminations, whose type can be inferred
data Elim n where
  -- bound variable
  `_ : (x : Var n) → Elim n
  -- application
  _∙_ : (f : Elim n) (s : Term n) → Elim n

  -- binary operation
  bin : (o : BinOp n) → Elim n

  -- eliminator for 𝓤
  𝓤-elim  :
    (T : Type (suc n)) →  -- result type
    (ρ ρᵀ : Usageω n) →   -- usages of π & T[π] in the suc case
    (z : Term n) →        -- '0ᵘ' case
    (s : Term (2 +ᴺ n)) → -- 'sucᵘ π' case (bound vars are π and T[π])
    (𝜋 : Usageᴱ n) → Elim n

  -- eliminator for 𝓤ω
  𝓤ω-elim :
    (T : Type (suc n)) → -- result type
    (ρ : Usageω n) →     -- usage of π in the ↑ case
    (d : Term (suc n)) → -- '↑ π' case (d for definite; bound var is π)
    (w : Term n) →       -- 'ω' case
    (𝜋 : Usageᴱ n) → Elim n

  -- type annotation for a term
  _⦂_ : (s : Term n) (S : Type n) → Elim n
private variable e f : Elim n ; 𝜋 𝜌 : Usageᴱ n

infix  1000 `_
infixl 400  _∙_
infix  100  _⦂_

pattern _+_  𝜋 ρ = bin (fin `+ 𝜋 ρ)
pattern _*_  𝜋 ρ = bin (fin `* 𝜋 ρ)
pattern _+ʷ_ 𝜋 𝜌 = bin (inf `+ 𝜋 𝜌)
pattern _*ʷ_ 𝜋 𝜌 = bin (inf `* 𝜋 𝜌)
infixl 310 _*_ _*ʷ_
infixl 300 _+_ _+ʷ_

pattern ‶_ x = [ ` x ] -- bound variable in a term
infix 1000 ‶_

-- weakˣ′ x M inserts an extra bound variable between x - 1 and x
-- in the common case weakˣ′ 0 (aka weakˣ) it inserts a variable
-- before all dangling variables without disturbing binders inside the
-- term
weakᵗ′ : Var (suc n) → Term n → Term (suc n)
weakᵉ′ : Var (suc n) → Elim n → Elim (suc n)
weakᵇ′ : Var (suc n) → Binder n → Binder (suc n)
weakᵒ′ : Var (suc n) → BinOp n → BinOp (suc n)
weakᵗ′ x (CORE K) = CORE K
weakᵗ′ x (BIND B T) = BIND (weakᵇ′ x B) (weakᵗ′ (suc x) T)
weakᵗ′ x 0ᵘ = 0ᵘ
weakᵗ′ x (sucᵘ π) = sucᵘ (weakᵗ′ x π)
weakᵗ′ x (↑ π) = ↑ weakᵗ′ x π
weakᵗ′ x ωᵘ = ωᵘ
weakᵗ′ x [ e ] = [ weakᵉ′ x e ]
weakᵉ′ x (` y) = ` Fin.punchIn x y
weakᵉ′ x (f ∙ s) = weakᵉ′ x f ∙ weakᵗ′ x s
weakᵉ′ x (bin o) = bin $ weakᵒ′ x o
weakᵉ′ x (𝓤-elim T ρ ρᵀ z s 𝜋) =
  let x′ = suc x ; x″ = suc x′ in
  𝓤-elim (weakᵗ′ x′ T) (weakᵗ′ x ρ) (weakᵗ′ x ρᵀ)
         (weakᵗ′ x z) (weakᵗ′ x″ s) (weakᵉ′ x 𝜋)
weakᵉ′ x (𝓤ω-elim T ρ d w 𝜋) =
  let x′ = suc x in
  𝓤ω-elim (weakᵗ′ x′ T) (weakᵗ′ x ρ)
          (weakᵗ′ x′ d) (weakᵗ′ x w) (weakᵉ′ x 𝜋)
weakᵉ′ x (s ⦂ S) = weakᵗ′ x s ⦂ weakᵗ′ x S
weakᵇ′ x `𝚷[ π / S ] = `𝚷[ weakᵗ′ x π / weakᵗ′ x S ]
weakᵇ′ x `𝛌 = `𝛌
weakᵒ′ x (fin • 𝜋 ρ) = fin • (weakᵉ′ x 𝜋) (weakᵗ′ x ρ)
weakᵒ′ x (inf • 𝜋 𝜌) = inf • (weakᵉ′ x 𝜋) (weakᵉ′ x 𝜌)

-- substˣ″ x M e replaces occurrences of variable x in M with the
-- result of e (and shuffles the remaining indices accordingly)
module _ {F : Set → Set} (A : RawApplicative F) where
  open RawApplicative A using (pure ; _<$>_ ; _⊛_)
  substᵗ″ : Var (suc n) → Term (suc n) → F (Elim n) → F (Term n)
  substᵉ″ : Var (suc n) → Elim (suc n) → F (Elim n) → F (Elim n)
  substᵇ″ : Var (suc n) → Binder (suc n) → F (Elim n) → F (Binder n)
  substᵒ″ : Var (suc n) → BinOp (suc n) → F (Elim n) → F (BinOp n)
  substᵗ″ x (CORE K) e = pure $ CORE K
  substᵗ″ x (BIND B T) e =
    pure BIND ⊛ substᵇ″ x B e
              ⊛ (substᵗ″ (suc x) T (weakᵉ′ x <$> e))
  substᵗ″ x 0ᵘ e = pure 0ᵘ
  substᵗ″ x (sucᵘ π) e = sucᵘ <$> substᵗ″ x π e
  substᵗ″ x (↑ π) e = ↑_ <$> substᵗ″ x π e
  substᵗ″ x ωᵘ e = pure ωᵘ
  substᵗ″ x [ f ] e = [_] <$> substᵉ″ x f e
  substᵉ″ x (` y) e = case x Fin.≟ y of λ where
    (yes _)  → e
    (no x≢y) → pure $ ` Fin.punchOut x≢y
  substᵉ″ x (f ∙ s) e = pure _∙_ ⊛ substᵉ″ x f e ⊛ substᵗ″ x s e
  substᵉ″ x (bin o) e = bin <$> substᵒ″ x o e
  substᵉ″ x (𝓤-elim T ρ ρᵀ z s 𝜋) e =
    let x′ = suc x  ; e′ = weakᵉ′ x  <$> e
        x″ = suc x′ ; e″ = weakᵉ′ x′ <$> e′ in
    pure 𝓤-elim ⊛ substᵗ″ x′ T e′
                ⊛ substᵗ″ x ρ e
                ⊛ substᵗ″ x ρᵀ e
                ⊛ substᵗ″ x  z e ⊛ substᵗ″ x″ s e″
                ⊛ substᵉ″ x  𝜋 e
  substᵉ″ x (𝓤ω-elim T ρ d w 𝜋) e =
    let x′ = suc x  ; e′ = weakᵉ′ x <$> e in
    pure 𝓤ω-elim ⊛ substᵗ″ x′ T e′
                 ⊛ substᵗ″ x ρ e
                 ⊛ substᵗ″ x′ d e′ ⊛ substᵗ″ x w e
                 ⊛ substᵉ″ x  𝜋 e
  substᵉ″ x (s ⦂ S) e = pure _⦂_ ⊛ substᵗ″ x s e ⊛ substᵗ″ x S e
  substᵇ″ x `𝚷[ π / S ] e = pure `𝚷[_/_] ⊛ substᵗ″ x π e ⊛ substᵗ″ x S e
  substᵇ″ x `𝛌 e = pure `𝛌
  substᵒ″ x (fin • 𝜋 ρ) e = pure (fin •) ⊛ substᵉ″ x 𝜋 e ⊛ substᵗ″ x ρ e
  substᵒ″ x (inf • 𝜋 𝜌) e = pure (inf •) ⊛ substᵉ″ x 𝜋 e ⊛ substᵉ″ x 𝜌 e

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

  -- chop′ x M removes the variable x from M's scope, which decreases
  -- all higher dB indices by 1. It fails if x is actually used anywhere
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

open Subst weakᵒ′ substᵒ″ public using ()
  renaming (weak to weakᵒ ; subst′ to substᵒ′ ; subst to substᵒ ;
            chop′ to chopᵒ′ ; chop to chopᵒ)


private
  -- 'punchIn x' increments all variables ≥ x, which leaves a hole
  -- where x itself was, so 'punchIn x y' can never be x
  punchIn-≢ : x ≢ Fin.punchIn x y
  punchIn-≢ {x = zero}  {y}     ()
  punchIn-≢ {x = suc x} {zero}  ()
  punchIn-≢ {x = suc x} {suc y} eq = punchIn-≢ $ Fin.suc-injective eq

  -- inserting a variable and then removing the same one is the same
  -- as doing nothing
  punchOutIn : (x≢y′ : x ≢ Fin.punchIn x y) → Fin.punchOut x≢y′ ≡ y
  punchOutIn {x = zero}  {y}     _    = refl
  punchOutIn {x = suc x} {zero}  _    = refl
  punchOutIn {x = suc x} {suc y} x≢y′ =
    ≡.cong suc $ punchOutIn $ x≢y′ ∘ ≡.cong suc

-- if you add a variable to a scope, and then substitute for that
-- (unused) variable, then overall nothing happens
--
-- [the only case which does anything is `_ on LL256-258]
subst-weakᵗ : (s : Term n) (x : Fin (suc n)) (e : Elim n) →
              substᵗ′ x (weakᵗ′ x s) e ≡ s
subst-weakᵉ : (f : Elim n) (x : Fin (suc n)) (e : Elim n) →
              substᵉ′ x (weakᵉ′ x f) e ≡ f
subst-weakᵇ : (B : Binder n) (x : Fin (suc n)) (e : Elim n) →
              substᵇ′ x (weakᵇ′ x B) e ≡ B
subst-weakᵒ : (o : BinOp n) (x : Fin (suc n)) (e : Elim n) →
              substᵒ′ x (weakᵒ′ x o) e ≡ o
subst-weakᵗ (CORE _) x e = refl
subst-weakᵗ (BIND B T) x e
  rewrite subst-weakᵇ B x e | subst-weakᵗ T (suc x) (weakᵉ′ x e) = refl
subst-weakᵗ 0ᵘ x e = refl
subst-weakᵗ (sucᵘ π) x e rewrite subst-weakᵗ π x e = refl
subst-weakᵗ (↑ π) x e rewrite subst-weakᵗ π x e = refl
subst-weakᵗ ωᵘ x e = refl
subst-weakᵗ [ f ] x e rewrite subst-weakᵉ f x e = refl
subst-weakᵉ (` y) x e with x Fin.≟ Fin.punchIn x y
... | yes x≡y′ = ⊥-elim $ punchIn-≢ x≡y′
... | no  x≢y′ rewrite punchOutIn x≢y′ = refl
subst-weakᵉ (f ∙ s) x e rewrite subst-weakᵉ f x e | subst-weakᵗ s x e = refl
subst-weakᵉ (bin o) x e rewrite subst-weakᵒ o x e = refl
subst-weakᵉ (𝓤-elim T ρ ρᵀ z s 𝜋) x e
  rewrite subst-weakᵗ T  (suc x)       (weakᵉ′ x e)
        | subst-weakᵗ ρ  x             e
        | subst-weakᵗ ρᵀ x             e
        | subst-weakᵗ z  x             e
        | subst-weakᵗ s  (suc (suc x)) (weakᵉ′ (suc x) (weakᵉ′ x e))
        | subst-weakᵉ 𝜋  x             e = refl
subst-weakᵉ (𝓤ω-elim T ρ d w 𝜋) x e
  rewrite subst-weakᵗ T  (suc x) (weakᵉ′ x e)
        | subst-weakᵗ ρ  x             e
        | subst-weakᵗ d  (suc x) (weakᵉ′ x e)
        | subst-weakᵗ w  x       e
        | subst-weakᵉ 𝜋 x       e = refl
subst-weakᵉ (s ⦂ S) x e rewrite subst-weakᵗ s x e | subst-weakᵗ S x e = refl
subst-weakᵇ `𝚷[ π / S ] x e
  rewrite subst-weakᵗ π x e | subst-weakᵗ S x e = refl
subst-weakᵇ `𝛌 x e = refl
subst-weakᵒ (fin • 𝜋 ρ) x e
  rewrite subst-weakᵉ 𝜋 x e | subst-weakᵗ ρ x e = refl
subst-weakᵒ (inf • 𝜋 𝜌) x e
  rewrite subst-weakᵉ 𝜋 x e | subst-weakᵉ 𝜌 x e = refl

-- convert a natural to a constant usage
natToUsage : ℕ → Usage n
natToUsage zero    = 0ᵘ
natToUsage (suc n) = sucᵘ $ natToUsage n

-- use numerals in terms for usages
-- (which means bound vars have to be ` n)
instance number-Term : Number (Usage n)
number-Term .Number.Constraint _ = ⊤
number-Term .Number.fromNat    n = natToUsage n
