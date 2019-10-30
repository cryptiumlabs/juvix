-- there should be nothing surprising in here, it just reexports the most
-- common parts of the stdlib
--
-- * Unit, Empty, Nat, Fin¹, Bool
-- * Sum, Product, Maybe
-- * Binary relations, Decidability, Equality
-- * Function, which contains conveniences like `id`, `_$_`, `case_of_`, etc
--
-- ¹`Fin n` is [isomorphic to] the naturals less than `n`

module Prelude where

open import Agda.Primitive public using (Level ; lzero ; lsuc ; _⊔_)
open import Agda.Builtin.FromNat public

-- like the one from stdlib but with more instance
record Lift {a} ℓ (A : Set a) : Set (a ⊔ ℓ) where
  instance constructor lift
  field ⦃ lower ⦄ : A

module ⊤ where
  open import Data.Unit public hiding (module ⊤)
open ⊤ public using (⊤ ; tt)

module ⊥ where
  open import Data.Empty public hiding (module ⊥)
open ⊥ public using (⊥ ; ⊥-elim)

module ℕ where
  open import Data.Nat public hiding (module ℕ)
  open import Data.Nat.Properties public
  import Data.Nat.Literals as Lit

  instance number = Lit.number
open ℕ public using (ℕ ; zero ; suc)

module Fin where
  import Data.Fin hiding (module Fin ;
                          0F ; 1F ; 2F ; 3F ; 4F ; 5F ; 6F ; 7F ; 8F ; 9F)
  open Data.Fin public
  open import Data.Fin.Properties public
  import Data.Fin.Literals as Lit

  instance number : ∀ {n} → Number (Fin n)
  number = Lit.number _
open Fin public using (Fin ; zero ; suc ; #_)

module Bool where
  open import Data.Bool public hiding (module Bool)
  open import Data.Bool.Properties public
open Bool public using (Bool ; true ; false ; if_then_else_)

module ⊎ where
  open import Data.Sum public hiding (module _⊎_)
  open import Data.Sum.Properties public
open ⊎ public using (_⊎_ ; inj₁ ; inj₂)

module Σ where
  open import Data.Product public hiding (module Σ)
open Σ public using (Σ ; Σ-syntax ; _×_ ; ∃ ; ∃-syntax ; ∄ ; ∄-syntax ;
                     _,_ ; -,_ ; proj₁ ; proj₂)

module Maybe where
  open import Data.Maybe public hiding (module Maybe)
  open import Data.Maybe.Properties public
open Maybe public using (Maybe ; nothing ; just)

module Relation where
  open import Relation.Nullary public renaming (Irrelevant to Irrelevant₀)
  open import Relation.Nullary.Decidable public
  open import Relation.Unary public
    renaming (Irrelevant to Irrelevant₁ ; Recomputable to Recomputable₁ ;
              Universal to Universal₁ ; Decidable to Decidable₁ ; _⇒_ to _⇒₁_)
  open import Relation.Binary public
    renaming (Irrelevant to Irrelevant₂ ; Recomputable to Recomputable₂ ;
              Universal to Universal₂ ; Decidable to Decidable₂ ; _⇒_ to _⇒₂_)

  module _ where
    private variable ℓ₁ ℓ₂ ℓ₃ ℓ₄ ℓ₅ ℓ₆ : Level ; A B C D E F : Set _

    _Preserves₃_⟶_⟶_⟶_ :
      (A → B → C → D) → Rel A ℓ₁ → Rel B ℓ₂ → Rel C ℓ₃ → Rel D ℓ₄ → Set _
    f Preserves₃ _∼ᵃ_ ⟶ _∼ᵇ_ ⟶ _∼ᶜ_ ⟶ _∼ᵈ_ =
      ∀ {a a′ b b′ c c′} →
      a ∼ᵃ a′ → b ∼ᵇ b′ → c ∼ᶜ c′ →
      f a b c ∼ᵈ f a′ b′ c′

    _Preserves₄_⟶_⟶_⟶_⟶_ :
      (A → B → C → D → E) →
      Rel A ℓ₁ → Rel B ℓ₂ → Rel C ℓ₃ → Rel D ℓ₄ → Rel E ℓ₅ → Set _
    f Preserves₄ _∼ᵃ_ ⟶ _∼ᵇ_ ⟶ _∼ᶜ_ ⟶ _∼ᵈ_ ⟶ _∼ᵉ_ =
      ∀ {a a′ b b′ c c′ d d′} →
      a ∼ᵃ a′ → b ∼ᵇ b′ → c ∼ᶜ c′ → d ∼ᵈ d′ →
      f a b c d ∼ᵉ f a′ b′ c′ d′

    _Preserves₅_⟶_⟶_⟶_⟶_⟶_ :
      (A → B → C → D → E → F) →
      Rel A ℓ₁ → Rel B ℓ₂ → Rel C ℓ₃ → Rel D ℓ₄ → Rel E ℓ₅ → Rel F ℓ₆ → Set _
    f Preserves₅ _∼ᵃ_ ⟶ _∼ᵇ_ ⟶ _∼ᶜ_ ⟶ _∼ᵈ_ ⟶ _∼ᵉ_ ⟶ _∼ᶠ_ =
      ∀ {a a′ b b′ c c′ d d′ e e′} →
      a ∼ᵃ a′ → b ∼ᵇ b′ → c ∼ᶜ c′ → d ∼ᵈ d′ → e ∼ᵉ e′ →
      f a b c d e ∼ᶠ f a′ b′ c′ d′ e′

open Relation public
  using (Dec ; yes ; no ; ¬_ ; True ; False ; ⌊_⌋ ;
         Pred ; Decidable₁ ; Rel ; Decidable₂)

module Algebra where
  open import Algebra public
  open import Algebra.FunctionProperties.Core public

  import Algebra.Structures as S
  import Algebra.FunctionProperties as F

  module Generic where
    open S public ; open F public hiding (Op₁ ; Op₂)
  module WithEq {a ℓ} {A : Set a} (≈ : Rel A ℓ) where
    open S ≈ public ; open F ≈ public hiding (Op₁ ; Op₂)
open Algebra public using (Op₁ ; Op₂)

module ≡ where
  open import Relation.Binary.PropositionalEquality public hiding (module _≡_)

  At : ∀ {a} (A : Set a) → Rel A _
  At A = _≡_ {A = A}
open ≡ public using (_≡_ ; refl) renaming (At to ≡-At)

open import Function public
