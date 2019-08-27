module Example where

open import Prelude

open import Usage
open import ExtNat hiding (_*_ ; _+_)
open import QTT NoSub.any
open import Type NoSub.any

variable
  n : ℕ
  e : Elim n

A : ∀ {n} → Tm n
A = sort 0

f = Fin 2 ∋ # 1
x = Fin 2 ∋ # 0

-- 2 f: (2 x: A) (3 y: A) → A, 10 x: A ⊢ 2 f x x: A
-- though note that the usages in the context are *outputs*
-- i.e. they're not checked against anything
example : ε ⨟ Π 2 A (Π 3 A A) ⨟ A ⊢ 2 - ` f ∙ [ ` x ] ∙ [ ` x ] ∈ A ▷ ε ⨟ 2 ⨟ 10
example =
  let Γ = ε ⨟ Π 2 A (Π 3 A A) ⨟ A in
  app (ε ⨟ 2 ⨟ 10 ≡ (ε ⨟ 2 ⨟ 4) ⊕ 3 ⨵ (ε ⨟ 0 ⨟ 2)   ∋ refl)
      (A ≡ substᵗ A ([ ` x ] ⦂ A)   ∋ refl)
    (app (ε ⨟ 2 ⨟ 4 ≡ (ε ⨟ 2 ⨟ 0) ⊕ 2 ⨵ (ε ⨟ 0 ⨟ 2)   ∋ refl)
         (Π 3 A A ≡ substᵗ (Π 3 A A) ([ ` x ] ⦂ A)   ∋ refl)
      (var (lookup Γ f ≡ Π 2 A (Π 3 A A)   ∋ refl)
           (Only 2 f (ε ⨟ 2 ⨟ 0)   ∋ ε ⨟[ refl ] ⨟ refl))
      (elim (A ≼ A   ∋ refl)
        (var (lookup Γ x ≡ A   ∋ refl)
             (Only 2 x (ε ⨟ 0 ⨟ 2)   ∋ ε ⨟ refl ⨟[ refl ]))))
    (elim (A ≼ A   ∋ refl)
      (var (lookup Γ x ≡ A   ∋ refl)
           (Only 2 x (ε ⨟ 0 ⨟ 2)   ∋ ε ⨟ refl ⨟[ refl ])))
