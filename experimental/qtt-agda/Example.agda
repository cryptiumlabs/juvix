-- ⚠ caution: de Bruijn indices ⚠

module Example where

open import Prelude

open import Usage
open import ExtNat
open import QTT NoSub.any hiding (_+_ ; _*_)
open import Type NoSub.any

variable
  n : ℕ
  e : Elim n

A : ∀ {n} → Tm n
A = sort 0

-- 2 f: 2 A → 3 A → A, 10 x: A ⊢ 2 f x x ∈ A
-- though note that the usages in the context are *outputs*
-- i.e. they're not checked against anything
example : ε ⨟ Π 2 A (Π 3 A A) ⨟ A ⊢ 2 - (1 ∙ 0 ∙ 0) ∈ A ▷ ε ⨟ 2 ⨟ 10
example =
  let Γ = ε ⨟ Π 2 A (Π 3 A A) ⨟ A in
  app (ε ⨟ 2 ⨟ 10 ≡ (ε ⨟ 2 ⨟ 4) ⊕ 3 ⨵ (ε ⨟ 0 ⨟ 2)   ∋ refl)
      (A ≡ substᵗ A (0 ⦂ A)   ∋ refl)
    (app (ε ⨟ 2 ⨟ 4 ≡ (ε ⨟ 2 ⨟ 0) ⊕ 2 ⨵ (ε ⨟ 0 ⨟ 2)   ∋ refl)
         (Π 3 A A ≡ substᵗ (Π 3 A A) (0 ⦂ A)   ∋ refl)
      (var (lookup Γ 1 ≡ Π 2 A (Π 3 A A)   ∋ refl)
           (Only 2 1 (ε ⨟ 2 ⨟ 0)   ∋ ε ⨟[ refl ] ⨟ refl))
      (elim (A ⩿ A   ∋ refl)
        (var (lookup Γ 0 ≡ A   ∋ refl)
             (Only 2 0 (ε ⨟ 0 ⨟ 2)   ∋ ε ⨟ refl ⨟[ refl ]))))
    (elim (A ⩿ A   ∋ refl)
      (var (lookup Γ 0 ≡ A   ∋ refl)
           (Only 2 0 (ε ⨟ 0 ⨟ 2)   ∋ ε ⨟ refl ⨟[ refl ])))

-- ⊢ 2 (1 f: (2 A → 3 A → A)) → 5 A → A ∋ λ f x. f x x
example′ : ε ⊢ 2 - Π 1 (Π 2 A (Π 3 A A)) (Π 5 A A) ∋ Λ (Λ [ 1 ∙ 0 ∙ 0 ]) ▷ ε
example′ = lam refl $ lam refl $ elim refl example

-- A, B, C: sort 0 ⊢ 1 (1 (1 A → 1 B → C) → 1 A → 2 B → C) ∋ λ x y z. x z (y z)
S : ε ⨟ sort 0 ⨟ sort 0 ⨟ sort 0
      ⊢ 1 - Π 1 (Π 1 2 (Π 1 2 2)) (Π 1 (Π 1 3 3) (Π 2 4 3))
      ∋ Λ (Λ (Λ [ 2 ∙ 0 ∙ [ 1 ∙ 0 ] ]))
      ▷ ε ⨟ 0 ⨟ 0 ⨟ 0
S =
  let Γ = ε ⨟ sort 0 ⨟ sort 0 ⨟ sort 0 ⨟ Π 1 2 (Π 1 2 2) ⨟ Π 1 3 3 ⨟ 4 in
  lam (1 ≾ᵗ 1 * 1   ∋ refl)
    (lam (1 ≾ᵗ 1 * 1   ∋ refl)
      (lam (2 ≾ᵗ 1 * 2   ∋ refl)
        (elim (3 ⩿ 3   ∋ refl)
          (app ((ε ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 1 ⨟ 1 ⨟ 2) ≡
                (ε ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 1 ⨟ 0 ⨟ 1) ⊕ 1 ⨵ (ε ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 1 ⨟ 1)
                  ∋ refl)
               (3 ≡ substᵗ 4 ([ 1 ∙ 0 ] ⦂ 4)   ∋ refl)
            (app ((ε ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 1 ⨟ 0 ⨟ 1) ≡
                  (ε ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 1 ⨟ 0 ⨟ 0) ⊕ 1 ⨵ (ε ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 1)
                    ∋ refl)
                 (Π 1 4 4 ≡ substᵗ (Π 1 5 5) (0 ⦂ 5)   ∋ refl)
              (var (lookup Γ 2 ≡ Π 1 5 (Π 1 5 5)  ∋ refl)
                   (Only 1 2 (ε ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 1 ⨟ 0 ⨟ 0) ∋
                     ε ⨟ refl ⨟ refl ⨟ refl ⨟[ refl ] ⨟ refl ⨟ refl))
              (elim (5 ⩿ 5   ∋ refl)
                (var (lookup Γ 0 ≡ 5   ∋ refl)
                     (Only 1 0 (ε ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 1) ∋
                       ε ⨟ refl ⨟ refl ⨟ refl ⨟ refl ⨟ refl ⨟[ refl ]))))
            (elim (4 ⩿ 4   ∋ refl)
              (app ((ε ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 1 ⨟ 1) ≡
                    (ε ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 1 ⨟ 0) ⊕ 1 ⨵ (ε ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 1)
                      ∋ refl)
                   (4 ≡ substᵗ 5 (0 ⦂ 5)   ∋ refl)
                (var (lookup Γ 1 ≡ Π 1 5 5   ∋ refl)
                     (Only 1 1 (ε ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 1 ⨟ 0) ∋
                      ε ⨟ refl ⨟ refl ⨟ refl ⨟ refl ⨟[ refl ] ⨟ refl))
                (elim (5 ⩿ 5   ∋ refl)
                  (var (lookup Γ 0 ≡ 5   ∋ refl)
                       (Only 1 0 (ε ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 0 ⨟ 1) ∋
                        ε ⨟ refl ⨟ refl ⨟ refl ⨟ refl ⨟ refl ⨟[ refl ])))))))))

-- 0 A, B : sort 0 ⊢ 1 (1 A → 0 B → A) ∋ λ x y. x
K : ε ⨟ sort 0 ⨟ sort 0 ⊢ 1 - Π 1 1 (Π 0 1 3) ∋ Λ (Λ 1) ▷ ε ⨟ 0 ⨟ 0
K =
  let Γ = ε ⨟ sort 0 ⨟ sort 0 ⨟ 1 ⨟ 1 in
  lam (1 * 1 ≼ 1   ∋ refl)
    (lam (0 * 1 ≼ 0   ∋ refl)
      (elim (3 ⩿ 3   ∋ refl)
        (var (lookup Γ 1 ≡ 3   ∋ refl)
             (Only 1 1 (ε ⨟ 0 ⨟ 0 ⨟ 1 ⨟ 0) ∋
               ε ⨟ refl ⨟ refl ⨟[ refl ] ⨟ refl))))

-- A : sort 0 ⊢ 1 (1 A → A) ∋ λ x . x
I : ε ⨟ sort 0 ⊢ 1 - Π 1 0 1 ∋ Λ 0 ▷ ε ⨟ 0
I = lam refl (elim refl (var refl (Only 1 0 (ε ⨟ 0 ⨟ 1) ∋ ε ⨟ refl ⨟[ refl ])))


ChurchZero = K

-- for useful non-∞ usages we'd need usage polymorphism.
-- which might be a nice thing to have imo.
-- (is polynomial equality decidable? it seems like it should be)
--
-- 0 A, B, C : sort 0
--   ⊢ 1 (∞ (∞ (∞ A → B) → ∞ C → A) → ∞ (∞ A → B) → ∞ C → B
--   ∋ λ n f x. f (n f x)
ChurchSuc :
  ε ⨟ sort 0 ⨟ sort 0 ⨟ sort 0
    ⊢ 1 - Π ∞ (Π ∞ (Π ∞ 2 2) (Π ∞ 1 4)) (Π ∞ (Π ∞ 3 3) (Π ∞ 2 4))
    ∋ Λ (Λ (Λ [ 1 ∙ [ 2 ∙ 1 ∙ 0 ] ]))
    ▷ ε ⨟ 0 ⨟ 0 ⨟ 0
ChurchSuc =
  lam refl (lam refl (lam refl
    (elim refl
      (app refl refl
        (var refl (ε ⨟ refl ⨟ refl ⨟ refl ⨟ refl ⨟[ refl ] ⨟ refl))
        (elim refl
          (app refl refl
            (app refl refl
              (var refl (ε ⨟ refl ⨟ refl ⨟ refl ⨟[ refl ] ⨟ refl ⨟ refl))
              (elim refl
                (var refl (ε ⨟ refl ⨟ refl ⨟ refl ⨟ refl ⨟[ refl ] ⨟ refl))))
            (elim refl
              (var refl (ε ⨟ refl ⨟ refl ⨟ refl ⨟ refl ⨟ refl ⨟[ refl ])))))))))

{-
0 | A : Type ⊢
suc :
  ∀ π0 ... π6.
  (π0 | (π1 | (π2 | A) → A) → (π3 | A) → A) →
  (π4 | (π5 | A) → A) →
  (π6 | A) →
  A
where
  π5 = π2
  π2 * π3 = π6
  1 + π2 * π1 = π4
  π2 = π0
i.e.
suc :
  ∀ πⁿ πᶠ πˣ π′.
  (0 | A : Type) →
  (πⁿ | n : (πᶠ | f : (πⁿ | x : A) → A) → (πˣ | x : A) → A) →
  (πᶠ * π′ + 1 | f : (πⁿ | A) → A) →
  (πˣ * π′ | x : A) →
  A
-}
