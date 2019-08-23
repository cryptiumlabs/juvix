module NatUsage where

-- four values of type Usages _ _ _ _ _ here:
-- * NoSub.any: no subusaging, anything on judgments
-- * NoSub.0-1: no subusaging, `0 or `1 on judgments
-- * ≤-Sub.any: ≤ for subusaging, anything on judgments
-- * ≤-Sub.0-1: ≤ for subusaging, `0 or `1 on judgments


open import Function
open import Data.Nat
open import Data.Nat.Properties
open import Relation.Binary
open import Relation.Binary.PropositionalEquality as ≡ using (_≡_ ; refl)
open import Usage

fromBit : Bit → ℕ
fromBit `0 = 0 ; fromBit `1 = 1

fromBit-inj : ∀ {x y} → fromBit x ≡ fromBit y → x ≡ y
fromBit-inj {`0} {`0} refl = refl
fromBit-inj {`1} {`1} refl = refl

module NoSub where
  module Any where
    isUsages : IsUsages _≡_ _≡_ _≡_ id _+_ _*_ 0 1
    isUsages =
      record {
        isDecEquivalenceʲ = ≡-isDecEquivalence ;
        inj = id ;
        isDecPartialOrderᵗ = ≡-isDecPartialOrder _≟_ ;
        isSemiringᵗ = *-+-isSemiring
      }

  module 0-1 where
    isUsages : IsUsages _≡_ _≡_ _≡_ fromBit _+_ _*_ `0 `1
    isUsages =
      record {
        IsUsages Any.isUsages ;
        isDecEquivalenceʲ = ≡ᵇ-isDecEquivalence ;
        inj = fromBit-inj
      }

  any 0-1 : Usages _ _ _ _ _
  any = record { Any }
  0-1 = record { 0-1 }

module ≤-Sub where
  module Any where
    isUsages : IsUsages _≡_ _≡_ _≤_ id _+_ _*_ 0 1
    isUsages =
      record {
        IsUsages NoSub.Any.isUsages ;
        isDecPartialOrderᵗ =
          record { IsDecTotalOrder ≤-isDecTotalOrder }
      }

  module 0-1 where
    isUsages : IsUsages _≡_ _≡_ _≤_ fromBit _+_ _*_ `0 `1
    isUsages =
      record {
        IsUsages Any.isUsages using (isDecPartialOrderᵗ ; isSemiringᵗ) ;
        IsUsages NoSub.0-1.isUsages using (isDecEquivalenceʲ ; inj)
      }

  any 0-1 : Usages _ _ _ _ _
  any = record { Any }
  0-1 = record { 0-1 }
