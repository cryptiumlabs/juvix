open import Prelude
open import Metatheory.ChurchRosser

module Metatheory.Eval
  {𝒯 : ℕ → Set} {⟿-At : ∀ n → Rel (𝒯 n) lzero}
  (strongCR : StrongCR ⟿-At)
 where

open import QTT
open import Eval

open Relation
open import Relation.Binary.Construct.Closure.ReflexiveTransitive

private
 variable
  n : ℕ
  X X′ Y Y′ Z : 𝒯 n


open Eval.Derived ⟿-At public hiding (make-≋)

-- because of the definition of ≋ this is just strong c-r!
--
-- S     T     U
--  ↘ ↙ ↘ ↙
--    *     *
--    V     W
--     ↘ ↙
--       *
--       X -- we need this

≋-trans : Transitive $ ≋-At n
≋-trans (make-≋ SV TV) (make-≋ TW UW) =
  let make-≋ VX WX = strongCR _ TV TW in
  make-≋ (SV ◅◅ VX) (UW ◅◅ WX)

≋-equiv : IsEquivalence $ ≋-At n
≋-equiv = record { refl = ≋-refl ; sym = ≋-sym ; trans = ≋-trans }

≋-setoid : ℕ → Setoid _ _
≋-setoid n = record { isEquivalence = ≋-equiv {n} }


-- a term has only one normal form...
eval-unique : X ⟿! Y → X ⟿! Z → Y ≡ Z
eval-unique (XY , ¬RY) (XZ , ¬RZ) with strongCR _ XY XZ
... | make-≋ ε        ε        = refl
... | make-≋ ε        (RZ ◅ _) = ⊥-elim $ ¬RZ (-, RZ)
... | make-≋ (RY ◅ _) _        = ⊥-elim $ ¬RY (-, RY)


-- ...which means that checking convertibility is just checking equality
-- of normal forms
≟-to-≋? : X ⟿! X′ → Y ⟿! Y′ →
          Dec (X′ ≡ Y′) → Dec (X ≋ Y)
≟-to-≋? (RX , _) (RY , _) (yes refl) = yes $ make-≋ RX RY
≟-to-≋? {X = X} {Y = Y} (XX′ , X⇓) (YY′ , Y⇓) (no ¬eq) = no nope where
  nope : ¬ (X ≋ Y)
  nope (make-≋ XZ YZ) with strongCR _ XX′ XZ | strongCR _ YY′ YZ
  ... | make-≋ (RX′ ◅ _) _ | _ = X⇓ $ -, RX′
  ... | make-≋ ε _ | make-≋ (RY′ ◅ _) _ = Y⇓ $ -, RY′
  ... | make-≋ ε ZX′ | make-≋ ε ZY′ =
    ¬eq (eval-unique (ZX′ , X⇓) (ZY′ , Y⇓))
