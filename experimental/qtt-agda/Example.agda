-- ⚠ caution: de Bruijn indices ⚠

{-# OPTIONS --rewriting #-}

module Example where

open import Prelude

open import QTT
open import Type
open import Eval

open import Relation.Binary.Construct.Closure.ReflexiveTransitive
open import Relation.Binary.Construct.Closure.Symmetric

variable
  n : ℕ
  π : Term n
  e : Elim n

A : Term n
A = ⋆ 0

pattern _⨟!_ xs x = xs ⨟[ refl ] x
infixl 5 _⨟!_

-- a macro to generate the long strings of ... ⨟ +ᵘ-ℕ refl ⨟ ... and
-- similar boring things would probably go a long way to helping
-- readability


-- 2 f: 2 A → 3 A → A, 10 x: A ⊢ 2 f x x ∈ A
-- though note that the usages in the context are *outputs*
-- i.e. they're not checked against anything
f-x-x : ε ⨟ 𝚷[ 2 / A ] 𝚷[ 3 / A ] A ⨟ A
        ⊢ 2 - (` 1 ∙ `` 0  ∙ `` 0) ∈ A
        ▷ ε ⨟ 2 ⨟ 10
f-x-x =
  ty-∙ (ε ⨟! *ᵘ-ℕ refl ⨟! *ᵘ-ℕ refl)
       (ε ⨟ +ᵘ-ℕ refl ⨟ +ᵘ-ℕ refl)
       refl
    (ty-∙ (ε ⨟! *ᵘ-ℕ refl ⨟! *ᵘ-ℕ refl)
          (ε ⨟ +ᵘ-ℕ refl ⨟ +ᵘ-ℕ refl)
          refl
      (ty-` (there here) (there (here ε) ε))
      (ty-[] refl (ty-` here (here (ε ⨟ ε)))))
    (ty-[] refl (ty-` here (here (ε ⨟ ε))))



-- ⊢ 2 (1 f: (2 A → 3 A → A)) → 5 A → A ∋ λ f x. f x x
f-x-x′ : ε ⊢ 2 - 𝚷[ 1 / 𝚷[ 2 / A ] 𝚷[ 3 / A ] A ] 𝚷[ 5 / A ] A
             ∋ 𝛌 𝛌 [ ` 1 ∙ `` 0 ∙ `` 0 ] ▷ ε
f-x-x′ =
  ty-𝛌 (refl (*ᵘ-ℕ′ refl))
    (ty-𝛌 (refl (*ᵘ-ℕ′ refl)) (ty-[] refl f-x-x))


 -- A, B, C: ⋆ 0 ⊢ 1 (1 (1 A → 1 B → C) → 1 A → 2 B → C) ∋ λ x y z. x z (y z)
S : ε ⨟ ⋆ 0 ⨟ ⋆ 0 ⨟ ⋆ 0
      ⊢ 1 - 𝚷[ 1 / 𝚷[ 1 / `` 2 ] 𝚷[ 1 / `` 2 ] `` 2 ]
            𝚷[ 1 / 𝚷[ 1 / `` 3 ] `` 3 ] 𝚷[ 2 / `` 4 ] `` 3
      ∋ 𝛌 𝛌 𝛌 [ ` 2 ∙ `` 0 ∙ [ ` 1 ∙ `` 0 ] ]
      ▷ ε ⨟ 0 ⨟ 0 ⨟ 0
S =
  ty-𝛌 (refl (*ᵘ-ℕ′ refl))
    (ty-𝛌 (refl (*ᵘ-ℕ′ refl))
      (ty-𝛌 (refl (*ᵘ-ℕ′ refl))
        (ty-[] refl
          (ty-∙ (ε ⨟! *ᵘ-ℕ refl ⨟! *ᵘ-ℕ refl ⨟! *ᵘ-ℕ refl ⨟! *ᵘ-ℕ refl ⨟! *ᵘ-ℕ refl ⨟! *ᵘ-ℕ refl)
                (ε ⨟ +ᵘ-ℕ refl ⨟ +ᵘ-ℕ refl ⨟ +ᵘ-ℕ refl ⨟ +ᵘ-ℕ refl ⨟ +ᵘ-ℕ refl ⨟ +ᵘ-ℕ refl)
                refl
            (ty-∙ (ε ⨟! *ᵘ-ℕ refl ⨟! *ᵘ-ℕ refl ⨟! *ᵘ-ℕ refl ⨟! *ᵘ-ℕ refl ⨟! *ᵘ-ℕ refl ⨟! *ᵘ-ℕ refl)
                  (ε ⨟ +ᵘ-ℕ refl ⨟ +ᵘ-ℕ refl ⨟ +ᵘ-ℕ refl ⨟ +ᵘ-ℕ refl ⨟ +ᵘ-ℕ refl ⨟ +ᵘ-ℕ refl)
                  refl
              (ty-` (there (there here))
                    (there (there (here (ε ⨟ ε ⨟ ε ⨟ ε)) ε) ε))
              (ty-[] refl
                (ty-` here (here (ε ⨟ ε ⨟ ε ⨟ ε ⨟ ε ⨟ ε)))))
            (ty-[] refl
              (ty-∙ (ε ⨟! *ᵘ-ℕ refl ⨟! *ᵘ-ℕ refl ⨟! *ᵘ-ℕ refl ⨟! *ᵘ-ℕ refl ⨟! *ᵘ-ℕ refl ⨟! *ᵘ-ℕ refl)
                    (ε ⨟ +ᵘ-ℕ refl ⨟ +ᵘ-ℕ refl ⨟ +ᵘ-ℕ refl ⨟ +ᵘ-ℕ refl ⨟ +ᵘ-ℕ refl ⨟ +ᵘ-ℕ refl)
                    refl
                (ty-` (there here) (there (here (ε ⨟ ε ⨟ ε ⨟ ε ⨟ ε)) ε))
                (ty-[] refl
                  (ty-` here (here (ε ⨟ ε ⨟ ε ⨟ ε ⨟ ε ⨟ ε))))))))))

-- 0 A, B : ⋆ 0 ⊢ 1 (1 A → 0 B → A) ∋ λ x y. x
K : ε ⨟ ⋆ 0 ⨟ ⋆ 0 ⊢ 1 - 𝚷[ 1 / `` 1 ] 𝚷[ 0 / `` 1 ] `` 3 ∋ 𝛌 𝛌 `` 1 ▷ ε ⨟ 0 ⨟ 0
K =
  ty-𝛌 (refl (*ᵘ-ℕ′ refl))
    (ty-𝛌 (refl (*ᵘ-ℕ′ refl))
      (ty-[] refl
        (ty-` (there here) (there (here (ε ⨟ ε ⨟ ε)) ε))))

-- A : ⋆ 0 ⊢ 1 (1 A → A) ∋ λ x . x
I : ε ⨟ ⋆ 0 ⊢ 1 - 𝚷[ 1 / `` 0 ] `` 1 ∋ 𝛌 `` 0 ▷ ε ⨟ 0
I = ty-𝛌 (refl (*ᵘ-ℕ′ refl)) (ty-[] refl (ty-` here (here (ε ⨟ ε))))

ChurchZero = K


-- 0 A : ⋆₀
-- ⊢ (0 u: 𝓤) → 1 (u (1 A → A) → 1 A → A) → {suc u} (1 A → A) → 1 A → A
-- ∋ λu. λn. λs. λz. s (n s z)
ChurchSuc : ε ⨟ ⋆ 0
          ⊢ 1
          - 𝚷[ 0 / 𝓤 ]
            𝚷[ 1 / 𝚷[ `` 0 / 𝚷[ 1 / `` 1 ] `` 2 ] 𝚷[ 1 / `` 2 ] `` 3 ]
            𝚷[ sucᵘ (`` 1) / 𝚷[ 1 / `` 2 ] `` 3 ] 𝚷[ 1 / `` 3 ] `` 4
          ∋ 𝛌 𝛌 𝛌 𝛌 [ ` 1 ∙ [ ` 2 ∙ `` 1 ∙ `` 0 ] ]
          ▷ ε ⨟ 0
ChurchSuc =
  ty-𝛌 (refl ε)
    (ty-𝛌 (refl ε)
      (ty-𝛌 (refl 1-*ᵘ)
        (ty-𝛌 (refl ε)
          (ty-[] refl
            (ty-∙ (ε ⨟! ε ⨟! ε ⨟! ε ⨟! ε ⨟! ε)
                  (ε ⨟ aux₀ ⨟ (inj₁ +ᵘ-0 ◅ ε) ⨟ aux₁ ⨟ aux₂ ⨟ aux₃)
                  refl
              (ty-` (there here) (there (here (ε ⨟ ε ⨟ ε ⨟ ε)) ε))
              (ty-[] refl
                (ty-∙ (ε ⨟! ε ⨟! ε ⨟! ε ⨟! ε ⨟! ε)
                      (ε ⨟ ε ⨟ aux₄ ⨟ ε ⨟ ε ⨟ ε)
                      refl
                  (ty-∙ (zero (ε ⨟ ε ⨟ ε) refl ⨟! ε ⨟! ε ⨟! ε)
                        (ε ⨟ ε ⨟ ε ⨟ ε ⨟ ε ⨟ ε)
                        refl
                    (ty-` (there (there here))
                          (there (there (here (ε ⨟ ε ⨟ ε)) ε) ε))
                    (ty-[] refl
                      (ty-` (there here) (there (here (ε ⨟ ε ⨟ ε ⨟ ε)) ε))))
                  (ty-[] refl
                    (ty-` here (here (ε ⨟ ε ⨟ ε ⨟ ε ⨟ ε)))))))))))
 where
  postulate
    -- I think there are at least three potential options here:
    -- 
    -- 1. make `ChurchSuc` require a use of `subst`
    -- 2. add a couple more reduction rules, including these
    -- 3. add a semiring solver for arithmetic expressions
    FIXME-*0 : π *ᵘ 0 ≋ᵗ 0
    FIXME-*1 : π *ᵘ 1 ≋ᵗ π
    FIXME-+0 : π +ᵘ 0 ≋ᵗ π

  aux₀ : 0 +ᵘ 1 *ᵘ (0 +ᵘ 0 +ᵘ 1 *ᵘ 0) ≋ᵗ (Term n ∋ 0)
  aux₀ = fwd +ᵘ-0 ◅ 1-*ᵘ ◅◅ +ᵘ-cong (fwd +ᵘ-0 ◅ ε) 1-*ᵘ ◅◅ fwd +ᵘ-0 ◅ ε

  aux₁ : 0 +ᵘ 1 *ᵘ (1 +ᵘ π *ᵘ 0 +ᵘ 1 *ᵘ 0) ≋ᵗ 1 *ᵘ 1
  aux₁ = fwd +ᵘ-0 ◅
         *ᵘ-cong ε (+ᵘ-cong (+ᵘ-cong ε FIXME-*0 ◅◅ +ᵘ-ℕ refl) (*ᵘ-ℕ refl) ◅◅
         +ᵘ-ℕ refl)

  aux₂ : 1 +ᵘ 1 *ᵘ (0 +ᵘ π *ᵘ 1 +ᵘ 1 *ᵘ 0) ≋ᵗ 1 *ᵘ (1 *ᵘ sucᵘ π)
  aux₂ = +ᵘ-cong ε (1-*ᵘ ◅◅ +ᵘ-cong (fwd +ᵘ-0 ◅ FIXME-*1) (*ᵘ-ℕ refl) ◅◅
                    FIXME-+0) ◅◅
         fwd +ᵘ-suc ◅ sucᵘ-cong (fwd +ᵘ-0 ◅ ε) ◅◅
         Evalᵗ.≋-sym (1-*ᵘ ◅◅ 1-*ᵘ)

  aux₃ : 0 +ᵘ 1 *ᵘ (0 +ᵘ π *ᵘ 0 +ᵘ 1 *ᵘ 1) ≋ᵗ 1 *ᵘ 1
  aux₃ = fwd +ᵘ-0 ◅
         *ᵘ-cong ε (+ᵘ-cong (fwd +ᵘ-0 ◅ FIXME-*0) (*ᵘ-ℕ refl) ◅◅ fwd +ᵘ-0 ◅ ε)

  aux₄ : 0 +ᵘ 0 +ᵘ 1 *ᵘ 0 ≋ᵗ (Term n ∋ 0)
  aux₄ = +ᵘ-cong (fwd +ᵘ-0 ◅ ε) (fwd *ᵘ-suc ◅ +ᵘ-cong (fwd *ᵘ-0 ◅ ε) ε) ◅◅
         fwd +ᵘ-0 ◅ fwd +ᵘ-0 ◅ ε
