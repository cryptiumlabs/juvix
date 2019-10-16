module QTT where

open import Prelude

private variable m n : ℕ

Var = Fin
private variable x y : Var n

Universe = ℕ
private variable u v : Universe

data Term n : Set
data Elim n : Set
Type = Term
Usage = Term


-- maybe the usage stuff should be elims since their types can always
-- be inferred as 𝓤
-- (and then the bits be renamed to ChkTerm and InfTerm or something i
-- suppose)

data Term n where
  ⋆         : (u : Universe) → Type n
  𝓤         : Type n
  𝚷[_/_]_   : (π : Usage n) (S : Type n) (T : Type (suc n)) → Type n
  𝛌_        : (t : Term (suc n)) → Term n
  0ᵘ ωᵘ     : Usage n
  sucᵘ      : (π   : Usage n) → Usage n
  _+ᵘ_ _*ᵘ_ : (π ρ : Usage n) → Usage n
  [_]       : (e : Elim n) → Term n
infixr 150 𝚷[_/_]_ 𝛌_
private
 variable
  s s′ t t′ : Term n
  S S′ T T′ R R′ : Type n
  π π′ ρ ρ′ : Usage n

data Elim n where
  `_     : (x : Var n) → Elim n
  _∙_    : (f : Elim n) (s : Term n) → Elim n
  𝓤-elim : (T : Type (suc n)) →
           (z : Term n) (s : Term (suc n)) (w : Term n) →
           (π : Usage n) → Elim n
  _⦂_    : (s : Term n) (S : Type n) → Elim n
infix 1000 `_ ; infixl 200 _∙_ ; infix 100 _⦂_
private variable e e′ f f′ : Elim n


data _⩿_ : Rel (Type n) lzero where
  ⋆    : u ℕ.≤ v → ⋆ u ⩿ ⋆ {n} v
  𝚷    : S′ ⩿ S → T ⩿ T′ → 𝚷[ π / S ] T ⩿ 𝚷[ π / S′ ] T′
  refl : S ⩿ S
  -- (maybe recurse into other structures?)
infix 4 _⩿_

weakᵗ′ : Var (suc n) → Term n → Term (suc n)
weakᵉ′ : Var (suc n) → Elim n → Elim (suc n)
weakᵗ′ i (⋆ u) = ⋆ u
weakᵗ′ u 𝓤 = 𝓤
weakᵗ′ i (𝚷[ π / S ] T) = 𝚷[ weakᵗ′ i π / weakᵗ′ i S ] weakᵗ′ (suc i) T
weakᵗ′ i (𝛌 t) = 𝛌 weakᵗ′ (suc i) t
weakᵗ′ i 0ᵘ = 0ᵘ
weakᵗ′ i ωᵘ = ωᵘ
weakᵗ′ i (sucᵘ π) = sucᵘ (weakᵗ′ i π)
weakᵗ′ i (π +ᵘ ρ) = weakᵗ′ i π +ᵘ weakᵗ′ i ρ
weakᵗ′ i (π *ᵘ ρ) = weakᵗ′ i π *ᵘ weakᵗ′ i ρ
weakᵗ′ i [ e ] = [ weakᵉ′ i e ]
weakᵉ′ i (` x) = ` Fin.punchIn i x
weakᵉ′ i (f ∙ s) = weakᵉ′ i f ∙ weakᵗ′ i s
weakᵉ′ i (s ⦂ S) = weakᵗ′ i s ⦂ weakᵗ′ i S
weakᵉ′ i (𝓤-elim T z s w π) =
  𝓤-elim (weakᵗ′ (suc i) T)
         (weakᵗ′ i       z)
         (weakᵗ′ (suc i) s)
         (weakᵗ′ i       w)
         (weakᵗ′ i       π)

weakᵗ : Term n → Term (suc n)
weakᵗ = weakᵗ′ zero
weakᵉ : Elim n → Elim (suc n)
weakᵉ = weakᵉ′ zero

substᵗ′ : Var (suc n) → Term (suc n) → Elim n → Term n
substᵉ′ : Var (suc n) → Elim (suc n) → Elim n → Elim n
substᵗ′ i (⋆ u) e = ⋆ u
substᵗ′ i 𝓤 e = 𝓤
substᵗ′ i (𝚷[ π / S ] T) e =
  𝚷[ substᵗ′ i π e / substᵗ′ i S e ] substᵗ′ (suc i) T (weakᵉ′ i e)
substᵗ′ i (𝛌 t) e = 𝛌 substᵗ′ (suc i) t (weakᵉ′ i e)
substᵗ′ i 0ᵘ e = 0ᵘ
substᵗ′ i ωᵘ e = ωᵘ
substᵗ′ i (sucᵘ π) e = sucᵘ (substᵗ′ i π e)
substᵗ′ i (π +ᵘ ρ) e = substᵗ′ i π e +ᵘ substᵗ′ i ρ e
substᵗ′ i (π *ᵘ ρ) e = substᵗ′ i π e *ᵘ substᵗ′ i ρ e
substᵗ′ i [ f ] e = [ substᵉ′ i f e ]
substᵉ′ i (` x) e = case i Fin.≟ x of λ where
  (yes _)  → e
  (no i≢x) → ` Fin.punchOut i≢x
substᵉ′ i (f ∙ s) e = substᵉ′ i f e ∙ substᵗ′ i s e
substᵉ′ i (s ⦂ S) e = substᵗ′ i s e ⦂ substᵗ′ i S e
substᵉ′ i (𝓤-elim T z s w π) e =
  𝓤-elim (substᵗ′ (suc i) T (weakᵉ′ i e))
         (substᵗ′ i       z e)
         (substᵗ′ (suc i) s (weakᵉ′ i e))
         (substᵗ′ i       w e)
         (substᵗ′ i       π e)

substᵗ : Term (suc n) → Elim n → Term n
substᵗ = substᵗ′ zero
substᵉ : Elim (suc n) → Elim n → Elim n
substᵉ = substᵉ′ zero
