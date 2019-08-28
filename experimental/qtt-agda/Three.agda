module _ where

open import Prelude
open Relation ; open Algebra
open import Usage

data Three : Set where `0 `1 ω : Three
private variable π π′ ρ ρ′ φ φ′ : Three

instance number-Three : Number Three
number-Three = λ where
  .Constraint → number-Bit .Constraint
  .fromNat 0  → `0
  .fromNat 1  → `1
 where open Number

open Algebra.WithEq (≡-At Three)

_+_ : Op₂ Three
`0 + ρ  = ρ
`1 + `0 = 1
`1 + `1 = ω
`1 + ω  = ω
ω  + _  = ω
infixl 10 _+_

_*_ : Op₂ Three
`0 * _  = 0
`1 * ρ  = ρ
ω  * `0 = 0
ω  * `1 = ω
ω  * ω  = ω
infixl 11 _*_

data _≼_ : Rel Three lzero where
  refl : π ≼ π
  _≼ω : ∀ π → π ≼ ω
infix 4 _≼_ _≼ω

_≟_ : Decidable $ ≡-At Three
`0 ≟ `0 = yes refl
`0 ≟ `1 = no (λ ())
`0 ≟ ω  = no (λ ())
`1 ≟ `0 = no (λ ())
`1 ≟ `1 = yes refl
`1 ≟ ω  = no (λ ())
ω  ≟ `0 = no (λ ())
ω  ≟ `1 = no (λ ())
ω  ≟ ω  = yes refl

_≼?_ : Decidable _≼_
`0 ≼? `0 = yes refl
`0 ≼? `1 = no (λ ())
`0 ≼? ω  = yes (`0 ≼ω)
`1 ≼? `0 = no (λ ())
`1 ≼? `1 = yes refl
`1 ≼? ω  = yes (`1 ≼ω)
ω  ≼? `0 = no (λ ())
ω  ≼? `1 = no (λ ())
ω  ≼? ω  = yes refl


-- the tedious proofs are entirely solvable by auto, thankfully

≡-isDecEquivalence : IsDecEquivalence $ ≡-At Three
≡-isDecEquivalence = record { ≡ ; _≟_ = _≟_ }

≼-refl : Reflexive _≼_
≼-refl = refl

≼-trans : Transitive _≼_
≼-trans     ij refl   = ij
≼-trans {π} _  (ρ ≼ω) = π ≼ω

≼-antisym : Antisymmetric _≡_ _≼_
≼-antisym refl   _ = refl
≼-antisym (ω ≼ω) _ = refl

≼-isDecPartialOrder : IsDecPartialOrder _≡_ _≼_
≼-isDecPartialOrder =
  record {
    isPartialOrder = record {
      isPreorder = record {
        isEquivalence = ≡.isEquivalence ;
        reflexive = λ{refl → refl} ;
        trans = ≼-trans
      } ;
      antisym = ≼-antisym
    } ;
    _≟_ = _≟_ ;
    _≤?_ = _≼?_
  }


+-assoc : Associative _+_
+-assoc `0 _  _  = refl
+-assoc `1 `0 _  = refl
+-assoc `1 `1 `0 = refl
+-assoc `1 `1 `1 = refl
+-assoc `1 `1 ω  = refl
+-assoc `1 ω  _  = refl
+-assoc ω  _  _  = refl

+-comm : Commutative _+_
+-comm `0 `0 = refl
+-comm `0 `1 = refl
+-comm `0 ω  = refl
+-comm `1 `0 = refl
+-comm `1 `1 = refl
+-comm `1 ω  = refl
+-comm ω  `0 = refl
+-comm ω  `1 = refl
+-comm ω  ω  = refl

*-assoc : Associative _*_
*-assoc `0 _  _  = refl
*-assoc `1 _  _  = refl
*-assoc ω  `0 _  = refl
*-assoc ω  `1 _  = refl
*-assoc ω  ω `0 = refl
*-assoc ω  ω `1 = refl
*-assoc ω  ω  ω = refl

distribˡ : _*_ DistributesOverˡ _+_
distribˡ `0 _  _  = refl
distribˡ `1 _  _  = refl
distribˡ ω  `0 _  = refl
distribˡ ω  `1 `0 = refl
distribˡ ω  `1 `1 = refl
distribˡ ω  `1 ω  = refl
distribˡ ω  ω  _  = refl

distribʳ : _*_ DistributesOverʳ _+_
distribʳ _  `0 _  = refl
distribʳ `0 `1 `0 = refl
distribʳ `0 `1 `1 = refl
distribʳ `0 `1 ω  = refl
distribʳ `1 `1 `0 = refl
distribʳ `1 `1 `1 = refl
distribʳ `1 `1 ω  = refl
distribʳ ω  `1 `0 = refl
distribʳ ω  `1 `1 = refl
distribʳ ω  `1 ω  = refl
distribʳ `0 ω  `0 = refl
distribʳ `0 ω  `1 = refl
distribʳ `0 ω  ω  = refl
distribʳ `1 ω  _  = refl
distribʳ ω  ω  _  = refl

distrib : _*_ DistributesOver _+_
distrib = distribˡ , distribʳ

*-identityˡ : LeftIdentity 1 _*_
*-identityˡ _ = refl

*-identityʳ : RightIdentity 1 _*_
*-identityʳ `0 = refl
*-identityʳ `1 = refl
*-identityʳ ω  = refl

*-identity : Identity 1 _*_
*-identity = *-identityˡ , *-identityʳ
 
+-identityˡ : LeftIdentity 0 _+_
+-identityˡ _ = refl

*-zeroˡ : LeftZero 0 _*_
*-zeroˡ _ = refl

*-zeroʳ : RightZero 0 _*_
*-zeroʳ `0 = refl
*-zeroʳ `1 = refl
*-zeroʳ ω  = refl

*-zero : Zero 0 _*_
*-zero = *-zeroˡ , *-zeroʳ

isSemiring : IsSemiring _+_ _*_ `0 `1
isSemiring =
  record {
    isSemiringWithoutAnnihilatingZero = record {
      +-isCommutativeMonoid = record {
        isSemigroup = record {
          isMagma = record {
            isEquivalence = ≡.isEquivalence ;
            ∙-cong = ≡.cong₂ _
          } ;
          assoc = +-assoc
        } ;
        identityˡ = λ _ → refl ;
        comm = +-comm
      } ;
      *-isMonoid = record {
        isSemigroup = record {
          isMagma = record {
            isEquivalence = ≡.isEquivalence ;
            ∙-cong = ≡.cong₂ _
          } ;
          assoc = *-assoc
        } ;
        identity = *-identity
      } ;
      distrib = distrib
    } ;
    zero = *-zero
  }

open IsSemiring isSemiring public
  using (+-identityʳ ; +-identity)

semiring : Semiring _ _
semiring = record { isSemiring = isSemiring }


fromBit : Bit → Three
fromBit `0 = 0 ; fromBit `1 = 1

fromBit-inj : ∀ {π ρ} → fromBit π ≡ fromBit ρ → π ≡ ρ
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
        isSemiringᵗ = isSemiring
      }

  any : Usages _ _ _ _ _
  any = record { Any }

  module 0-1 where
    isUsages : IsUsages _≡_ _≡_ _≡_ fromBit _+_ _*_ 0 1
    isUsages =
      record {
        IsUsages Any.isUsages ;
        isDecEquivalenceʲ = ≡ᵇ-isDecEquivalence ;
        inj = fromBit-inj
      }

  0-1 : Usages _ _ _ _ _
  0-1 = record { 0-1 }

module ≼-Sub where
  module Any where
    isUsages : IsUsages _≡_ _≡_ _≼_ id _+_ _*_ 0 1
    isUsages =
      record {
        IsUsages NoSub.Any.isUsages ;
        isDecPartialOrderᵗ = ≼-isDecPartialOrder
      }

  any : Usages _ _ _ _ _
  any = record { Any }

  module 0-1 where
    isUsages : IsUsages _≡_ _≡_ _≼_ fromBit _+_ _*_ 0 1
    isUsages =
      record {
        IsUsages Any.isUsages using (isDecPartialOrderᵗ ; isSemiringᵗ) ;
        IsUsages NoSub.0-1.isUsages using (isDecEquivalenceʲ ; inj)
      }

  0-1 : Usages _ _ _ _ _
  0-1 = record { 0-1 }
