-- ⚠ caution: de Bruijn indices ⚠

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

-- a macro to generate the long strings of ... ⨟ +-ℕ refl ⨟ ... and
-- similar boring things would probably go a long way to helping
-- readability


-- 2 f: 2 A → 3 A → A, 10 x: A ⊢ 2 f x x ∈ A
-- though note that the usages in the context are *outputs*
-- i.e. they're not checked against anything
f-x-x : ε ⨟ 𝚷[ ↑ 2 / A ] 𝚷[ ↑ 3 / A ] A ⨟ A
        ⊢ ↑ 2 - (` 1 ∙ ‶ 0  ∙ ‶ 0) ∈ A
        ▷ ε ⨟ ↑ 2 ⨟ ↑ 10
f-x-x =
  ty-∙ (ε ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl)
       (ε ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl)
       refl
    (ty-∙ (ε ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl)
          (ε ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl)
          refl
      (ty-` (there here) (there (here ε) ε))
      (ty-[] ⩿-refl (ty-` here (here (ε ⨟ ε)))))
    (ty-[] ⩿-refl (ty-` here (here (ε ⨟ ε))))



-- ⊢ 2 (1 f: (2 A → 3 A → A)) → 5 A → A ∋ λ f x. f x x
f-x-x′ : ε ⊢ ↑ 2 - 𝚷[ ↑ 1 / 𝚷[ ↑ 2 / A ] 𝚷[ ↑ 3 / A ] A ] 𝚷[ ↑ 5 / A ] A
             ∋ 𝛌 𝛌 [ ` 1 ∙ ‶ 0 ∙ ‶ 0 ] ▷ ε
f-x-x′ =
  ty-𝛌′ (refl (*ʷ-ℕ′ refl))
    (ty-𝛌′ (refl (*ʷ-ℕ′ refl)) (ty-[] ⩿-refl f-x-x))

 -- A, B, C: ⋆ 0 ⊢ 1 (1 (1 A → 1 B → C) → 1 A → 2 B → C) ∋ λ x y z. x z (y z)
S : ε ⨟ ⋆ 0 ⨟ ⋆ 0 ⨟ ⋆ 0
      ⊢ ↑ 1 - 𝚷[ ↑ 1 / 𝚷[ ↑ 1 / ‶ 2 ] 𝚷[ ↑ 1 / ‶ 2 ] ‶ 2 ]
              𝚷[ ↑ 1 / 𝚷[ ↑ 1 / ‶ 3 ] ‶ 3 ] 𝚷[ ↑ 2 / ‶ 4 ] ‶ 3
      ∋ 𝛌 𝛌 𝛌 [ ` 2 ∙ ‶ 0 ∙ [ ` 1 ∙ ‶ 0 ] ]
      ▷ ε ⨟ ↑ 0 ⨟ ↑ 0 ⨟ ↑ 0
S =
  ty-𝛌′ (refl (*ʷ-ℕ′ refl))
    (ty-𝛌′ (refl (*ʷ-ℕ′ refl))
      (ty-𝛌′ (refl (*ʷ-ℕ′ refl))
        (ty-[] ⩿-refl
          (ty-∙ (ε ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl)
                (ε ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl)
                refl
            (ty-∙ (ε ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl)
                  (ε ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl)
                  refl
              (ty-` (there (there here))
                    (there (there (here (ε ⨟ ε ⨟ ε ⨟ ε)) ε) ε))
              (ty-[] ⩿-refl
                (ty-` here (here (ε ⨟ ε ⨟ ε ⨟ ε ⨟ ε ⨟ ε)))))
            (ty-[] ⩿-refl
              (ty-∙ (ε ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl)
                    (ε ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl)
                    refl
                (ty-` (there here) (there (here (ε ⨟ ε ⨟ ε ⨟ ε ⨟ ε)) ε))
                (ty-[] ⩿-refl
                  (ty-` here (here (ε ⨟ ε ⨟ ε ⨟ ε ⨟ ε ⨟ ε))))))))))

-- 0 A, B : ⋆ 0 ⊢ 1 (1 A → 0 B → A) ∋ λ x y. x
K : ε ⨟ ⋆ 0 ⨟ ⋆ 0
  ⊢ ↑ 1 - 𝚷[ ↑ 1 / ‶ 1 ] 𝚷[ ↑ 0 / ‶ 1 ] ‶ 3
  ∋ 𝛌 𝛌 ‶ 1
  ▷ ε ⨟ ↑ 0 ⨟ ↑ 0
K =
  ty-𝛌′ (refl (*ʷ-ℕ′ refl))
    (ty-𝛌′ (refl (*ʷ-ℕ′ refl))
      (ty-[] ⩿-refl
        (ty-` (there here) (there (here (ε ⨟ ε ⨟ ε)) ε))))

-- A : ⋆ 0 ⊢ 1 (1 A → A) ∋ λ x . x
I : ε ⨟ ⋆ 0 ⊢ ↑ 1 - 𝚷[ ↑ 1 / ‶ 0 ] ‶ 1 ∋ 𝛌 ‶ 0 ▷ ε ⨟ ↑ 0
I = ty-𝛌′ (refl (*ʷ-ℕ′ refl)) (ty-[] ⩿-refl (ty-` here (here (ε ⨟ ε))))

ChurchZero = K


-- 0 A : ⋆₀
-- ⊢ (0 u: 𝓤) → 1 (u (1 A → A) → 1 A → A) → {suc u} (1 A → A) → 1 A → A
-- ∋ λu. λn. λs. λz. s (n s z)
ChurchSuc : ε ⨟ ⋆ 0
          ⊢ ↑ 1
          - 𝚷[ ↑ 0 / 𝓤 ]
            𝚷[ ↑ 1 / 𝚷[ ↑ ‶ 0 / 𝚷[ ↑ 1 / ‶ 1 ] ‶ 2 ] 𝚷[ ↑ 1 / ‶ 2 ] ‶ 3 ]
            𝚷[ ↑ sucᵘ (‶ 1) / 𝚷[ ↑ 1 / ‶ 2 ] ‶ 3 ] 𝚷[ ↑ 1 / ‶ 3 ] ‶ 4
          ∋ 𝛌 𝛌 𝛌 𝛌 [ ` 1 ∙ [ ` 2 ∙ ‶ 1 ∙ ‶ 0 ] ]
          ▷ ε ⨟ ↑ 0
ChurchSuc =
  ty-𝛌′ (refl ε)
    (ty-𝛌′ (refl ε)
      (ty-𝛌′ (refl ε)
        (ty-𝛌′ (refl ε)
          (ty-[] ⩿-refl
            (ty-∙′ (ε ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl ⨟!
                         1-*ʷ ⨟! *ʷ-ℕ refl)
                   (ε ⨟ +ʷ-ℕ refl ⨟ 0+0=1*0 ⨟ 0+1=1*1 ⨟ 1+π=1*sucπ ⨟ 0+1=1*1)
              (ty-` (there here)
                    (there (here (ε ⨟ ε ⨟ ε ⨟ ε)) ε))
              (ty-[] ⩿-refl
                (ty-∙′ (ε ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl ⨟! *ʷ-ℕ refl ⨟!
                             *ʷ-ℕ refl ⨟! *ʷ-ℕ refl)
                       (ε ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl ⨟
                            FIXME-+ʷ-0 ⨟ +ʷ-ℕ refl)
                  (ty-∙′ (zero (ε ⨟ ε ⨟ ε) refl ⨟!
                           FIXME-*ʷ-0 ⨟! FIXME-*ʷ-1 ⨟! FIXME-*ʷ-0)
                         (ε ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl ⨟ +ʷ-ℕ refl ⨟ 0-+ʷ ⨟ 0-+ʷ)
                    (ty-` (there (there here))
                          (there (there (here (ε ⨟ ε ⨟ ε)) ε) ε))
                    (ty-[] ⩿-refl
                      (ty-` (there here)
                            (there (here (ε ⨟ ε ⨟ ε ⨟ ε)) ε))))
                  (ty-[] ⩿-refl
                    (ty-` here
                          (here (ε ⨟ ε ⨟ ε ⨟ ε ⨟ ε)))))))))))
  where
    postulate
      -- I think there are at least three potential options here:
      --
      -- 1. make `ChurchSuc` require a use of `subst`
      -- 2. add a couple more reduction rules, including these
      -- 3. add a semiring solver for arithmetic expressions
      FIXME-*ʷ-0 : ↑ π *ʷ ↑ 0 ≋ᵗ ↑ 0
      FIXME-*ʷ-1 : ↑ π *ʷ ↑ 1 ≋ᵗ ↑ π
      FIXME-+ʷ-0 : ↑ π +ʷ ↑ 0 ≋ᵗ ↑ π

    0+0=1*0 : ↑ 0 +ʷ ↑ 0 ≋ᵗ ↑ 1 *ʷ ↑ 0
    0+0=1*0 = +ʷ-ℕ refl ◅◅ *ʷ-ℕ′ refl

    -- (this one is used twice with different ns hence the ≋ᵗ-At business)
    0+1=1*1 : ↑ 0 +ʷ ↑ 1 ⟨ ≋ᵗ-At n ⟩ ↑ 1 *ʷ ↑ 1
    0+1=1*1 = +ʷ-ℕ refl ◅◅ *ʷ-ℕ′ refl

    1+π=1*sucπ : ↑ 1 +ʷ ↑ π ≋ᵗ ↑ 1 *ʷ ↑ sucᵘ π
    1+π=1*sucπ =
      fwd +ʷ-↑ ◅
      ↑-cong (fwd +-s ◅ sucᵘ-cong (fwd +-0 ◅ ε) ◅◅ Evalᵗ.≋-sym 1-*) ◅◅
      bwd *ʷ-↑ ◅ ε
