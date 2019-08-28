module Usage where

open import Prelude
open Algebra ; open Algebra.Generic
open Relation

record IsUsages {j j′ t t′ t″} {J : Set j} {T : Set t}
                (_≈ʲ_ : Rel J j′) (_≈ᵗ_ : Rel T t′) (_≾ᵗ_ : Rel T t″)
                (⟦_⟧ : J → T)
                (_+_ _*_ : Op₂ T) 
                (0# 1# : J) : Set (j ⊔ t ⊔ j′ ⊔ t′ ⊔ t″) where
  field
    isDecEquivalenceʲ : IsDecEquivalence _≈ʲ_
    inj : ∀ {π ρ} → ⟦ π ⟧ ≈ᵗ ⟦ ρ ⟧ → π ≈ʲ ρ
    isDecPartialOrderᵗ : IsDecPartialOrder _≈ᵗ_ _≾ᵗ_
    isSemiringᵗ : IsSemiring _≈ᵗ_ _+_ _*_ ⟦ 0# ⟧ ⟦ 1# ⟧

  open IsDecEquivalence isDecEquivalenceʲ public
    renaming (isEquivalence to isEquivalenceʲ ;
              refl to reflʲ ; sym to symʲ ; trans to transʲ ;
              reflexive to reflexiveʲ ;
              _≟_ to _≟ʲ_)
  open IsDecPartialOrder isDecPartialOrderᵗ public using ()
    renaming (_≟_ to _≟ᵗ_ ; _≤?_ to _≾ᵗ?_ ;
              isPartialOrder to isPartialOrderᵗ ;
              isPreorder to isPreorderᵗ ;
              antisym to ≾ᵗ-antisym ;
              reflexive to ≾ᵗ-reflexive ;
              refl to ≾ᵗ-refl ; trans to ≾ᵗ-trans ;
              ≤-respˡ-≈ to ≾ᵗ-respˡ-≈ᵗ ;
              ≤-respʳ-≈ to ≾ᵗ-respʳ-≈ᵗ ;
              ≤-resp-≈ to ≾ᵗ-resp-≈ᵗ)
  open IsSemiring isSemiringᵗ public
    renaming (isEquivalence to isEquivalenceᵗ ;
              refl to reflᵗ ; sym to symᵗ ; trans to transᵗ ;
              reflexive to reflexiveᵗ ;
              setoid to setoidᵗ)

  decSetoidʲ : DecSetoid _ _
  decSetoidʲ = record { isDecEquivalence = isDecEquivalenceʲ }

  setoidʲ : Setoid _ _
  setoidʲ = record { isEquivalence = isEquivalenceʲ }

  decPosetᵗ : DecPoset _ _ _
  decPosetᵗ = record { isDecPartialOrder = isDecPartialOrderᵗ }

  posetᵗ : Poset _ _ _
  posetᵗ = record { isPartialOrder = isPartialOrderᵗ }

  semiringᵗ : Semiring _ _
  semiringᵗ = record { isSemiring = isSemiringᵗ }

record Usages j j′ t t′ t″ : Set (lsuc (j ⊔ t ⊔ j′ ⊔ t′ ⊔ t″)) where
  infix  4 _≈ʲ_ _≈ᵗ_ _≾ᵗ_
  infixl 6 _+_
  infixl 7 _*_
  field
    Usageʲ   : Set j
    Usageᵗ   : Set t
    _≈ʲ_     : Rel Usageʲ j′
    _≈ᵗ_     : Rel Usageᵗ t′
    _≾ᵗ_     : Rel Usageᵗ t″
    ⟦_⟧      : Usageʲ → Usageᵗ
    0# 1#    : Usageʲ
    _+_ _*_  : Op₂ Usageᵗ
    isUsages : IsUsages _≈ʲ_ _≈ᵗ_ _≾ᵗ_ ⟦_⟧ _+_ _*_ 0# 1#
  open IsUsages isUsages public

  0#ᵗ 1#ᵗ : Usageᵗ
  0#ᵗ = ⟦ 0# ⟧ ; 1#ᵗ = ⟦ 1# ⟧


data Bit : Set where `0 `1 : Bit

instance number-Bit : Number Bit
number-Bit = λ where
  .Constraint 0 → ⊤
  .Constraint 1 → ⊤
  .Constraint (suc (suc _)) → ⊥
  .fromNat 0 → `0
  .fromNat 1 → `1
 where open Number

_≟ᵇ_ : Decidable $ ≡-At Bit
`0 ≟ᵇ `0 = yes refl
`0 ≟ᵇ `1 = no (λ ())
`1 ≟ᵇ `0 = no (λ ())
`1 ≟ᵇ `1 = yes refl
infix 4 _≟ᵇ_

≡ᵇ-isDecEquivalence : IsDecEquivalence $ ≡-At Bit
≡ᵇ-isDecEquivalence =
  record { ≡ ; _≟_ = _≟ᵇ_ }


module _ {a} {A : Set a} where
  private ≡A = ≡-At A

  ≡-isPartialOrder : IsPartialOrder ≡A ≡A
  ≡-isPartialOrder = record { ≡ ; antisym = const }

  ≡-isDecPartialOrder : Decidable ≡A → IsDecPartialOrder ≡A ≡A
  ≡-isDecPartialOrder ≟ =
    record { isPartialOrder = ≡-isPartialOrder ; _≟_ = ≟ ; _≤?_ = ≟ }
