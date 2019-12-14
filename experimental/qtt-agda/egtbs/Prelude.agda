module Prelude where

open import Agda.Builtin.FromNat public

open import Agda.Primitive public using (Level ; lzero ; lsuc ; _⊔_)
import Level.Literals
instance numberLevel = Level.Literals.Levelℕ

it : ∀ {a} {A : Set a} ⦃ x : A ⦄ → A
it ⦃ x ⦄ = x

-- like the one from stdlib but with more instance
record Lift {a} ℓ (A : Set a) : Set (a ⊔ ℓ) where
  instance constructor lift
  field ⦃ lower ⦄ : A

pattern lift′ x = lift ⦃ x ⦄

open import Function public

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
  import Data.Fin
    hiding (module Fin ; 0F ; 1F ; 2F ; 3F ; 4F ; 5F ; 6F ; 7F ; 8F ; 9F)
  open Data.Fin public
  open import Data.Fin.Properties public
  import Data.Fin.Literals as Lit

  instance number : ∀ {n} → Number (Fin n)
  number = Lit.number _
open Fin public using (Fin ; zero ; suc ; #_)


module Erased where
  record Erased {a} (A : Set a) : Set a where
    constructor erase
    field @0 erased : A
  open Erased public

  _>>=_ : ∀ {a b} {A : Set a} {B : Set b} →
          Erased A → (A → Erased B) → Erased B
  e >>= k = k (e .erased)
  infixl 1 _>>=_


  join : ∀ {a} {A : Set a} → Erased (Erased A) → Erased A
  join e = e >>= id

  map : ∀ {a b} {A : Set a} {B : Set b} →
        (A → B) → Erased A → Erased B
  map f e = e >>= erase ∘ f

  case₀_return_of_ :
    ∀ {a b} {A : Set a} →
    (@0 x : A) → (B : @0 A → Set b) → (∀ (@0 x) → B x) → B x
  case₀ x return _ of f = f x

  case₀-as-syntax = case₀_return_of_
  syntax case₀-as-syntax x (λ x′ → B) f = case₀ x as x′ return B of f

  case₀_of_ : ∀ {a b} {A : Set a} {B : Set b} → @0 A → (@0 A → B) → B
  case₀ x of f = f x
  infixl 0 case₀_return_of_ case₀-as-syntax case₀_of_
open Erased public
  using (Erased ; erase ; erased ;
         case₀_return_of_ ; case₀-as-syntax ; case₀_of_)
  hiding (module Erased)

module ⊎ where
  open import Data.Sum public hiding (module _⊎_)
  open import Data.Sum.Properties public
open ⊎ public using (_⊎_ ; inj₁ ; inj₂)

module Σ where
  open import Data.Product public hiding (module Σ)

  Σ₀ : ∀ {a b} (A : Set a) → (A → Set b) → _
  Σ₀ A B = Σ A (Erased ∘ B)

  syntax Σ₀-syntax A (λ x → B) = Σ₀[ x ∈ A ] B
  Σ₀-syntax = Σ₀
  infixr 2 Σ₀-syntax

  ∃₀ : ∀ {a b} {A : Set a} → (A → Set b) → _
  ∃₀ = Σ₀ _

  syntax ∃₀-syntax (λ x → B) = ∃₀[ x ] B
  ∃₀-syntax = ∃₀
  infixr 2 ∃₀-syntax

  pattern _,₀_ x y = x , erase y
  pattern -,₀_ y   = _ ,₀ y
  infixr 4 _,₀_ -,₀_

  map₀ : ∀ {a b} {A A′ : Set a} {B : A → Set b} {B′ : A′ → Set b} →
         (f : A → A′) (g : ∀ {x} → B x → B′ (f x)) →
         Σ₀ A B → Σ₀ A′ B′
  map₀ f g = map f $ Erased.map g
open Σ public
  using (Σ ; proj₁ ; proj₂ ; Σ-syntax ; _×_ ; ∃ ; ∃-syntax ; ∃₂ ; _,_ ; -,_ ;
         Σ₀ ; Σ₀-syntax ; ∃₀ ; ∃₀-syntax ; _,₀_ ; -,₀_)


module Maybe where
  open import Data.Maybe public hiding (module Maybe)
  open import Data.Maybe.Properties public
open Maybe public using (Maybe ; nothing ; just)

module Bool where
  open import Data.Bool public hiding (module Bool)
  open import Data.Bool.Properties public
open Bool public using (Bool ; true ; false ; if_then_else_)

module Relation where
  open import Relation.Nullary public
    renaming (Irrelevant to Irrelevant₀)
  open import Relation.Nullary.Decidable public
    renaming (map to mapDec ; map′ to mapDec′)
  open import Relation.Unary public
    renaming (Universal to Universal₁ ;
              Decidable to Decidable₁ ;
              Irrelevant to Irrelevant₁ ;
              Recomputable to Recomputable₁ ;
              _⇒_ to _⇒₁_)
  open import Relation.Binary public
    renaming (Universal to Universal₂ ;
              Decidable to Decidable₂ ;
              Irrelevant to Irrelevant₂ ;
              Recomputable to Recomputable₂ ;
              _⇒_ to _⇒₂_)
open Relation public
  using (Rel ; Pred ; Dec ; yes ; no ; Decidable₁ ; Decidable₂ ;
         ¬_ ; True ; False ; ⌊_⌋)

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
