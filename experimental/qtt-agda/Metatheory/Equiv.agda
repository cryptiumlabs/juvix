open import Prelude
open import Metatheory.ChurchRosser

module Metatheory.Equiv
  {𝒯 : ℕ → Set} {⟿-At : ∀ n → Rel (𝒯 n) lzero}
  (strongCR : StrongCR ⟿-At)
 where

open import QTT
open import Eval

open Relation
open import Relation.Binary.Construct.Closure.ReflexiveTransitive using (_◅◅_)

private variable n : ℕ


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
