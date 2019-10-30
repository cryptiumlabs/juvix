module ExtNat where

-- ≤ is the normal ℕ ordering extended with ∞ as a maximum element
-- ≼ has n ≼ ∞ for any finite n, but everything else unrelated

-- four values of type Usages _ _ _ _ _ here:
-- * NoSub.any: ≼ for subusaging, anything on judgments
-- * NoSub.0-1: ≼ for subusaging, `0 or `1 on judgments
-- * ≤-Sub.any: ≤ for subusaging, anything on judgments
-- * ≤-Sub.0-1: ≤ for subusaging, `0 or `1 on judgments

open import Prelude
open Relation

open import Usage
open Usage public using (Bit ; `0 ; `1)


data ℕ∞ : Set where
  `_ : (n : ℕ) → ℕ∞
  ∞  : ℕ∞
infix 1000 `_

open Algebra.WithEq (≡-At ℕ∞)

instance number-ℕ∞ : Number ℕ∞
number-ℕ∞ = λ where
  .Constraint _ → ⊤
  .fromNat    n → ` n
 where open Number

fromBit : Bit → ℕ∞
fromBit `0 = 0 ; fromBit `1 = 1

fromBit-inj : ∀ {x y} → fromBit x ≡ fromBit y → x ≡ y
fromBit-inj {`0} {`0} refl = refl
fromBit-inj {`1} {`1} refl = refl


_≟_ : Decidable₂ $ ≡-At ℕ∞
` m ≟ ` n with m ℕ.≟ n
` m ≟ ` n | yes p = yes $ ≡.cong `_ p
` m ≟ ` n | no ¬p = no λ{refl → ¬p refl}
` m ≟ ∞   = no λ ()
∞   ≟ ` n = no λ ()
∞   ≟ ∞   = yes refl
infix 4 _≟_

≡-isDecEquivalence : IsDecEquivalence $ ≡-At ℕ∞
≡-isDecEquivalence = record { ≡ ; _≟_ = _≟_ }


data _≤_ : Rel ℕ∞ lzero where
  n≤n : ∀ {m n} → m ℕ.≤ n → ` m ≤ ` n
  *≤∞ : ∀ {m}   → m ≤ ∞
infix 4 _≤_

_≤?_ : Decidable₂ _≤_
` m ≤? ` n with m ℕ.≤? n
` m ≤? ` n | yes p = yes $ n≤n p
` m ≤? ` n | no ¬p = no λ{(n≤n p) → ¬p p}
` m ≤? ∞   = yes *≤∞
∞   ≤? ` n = no λ ()
∞   ≤? ∞   = yes *≤∞
infix 4 _≤?_

≤-refl : Reflexive _≤_
≤-refl {` n} = n≤n ℕ.≤-refl
≤-refl {∞}   = *≤∞

≤-antisym : Antisymmetric _≡_ _≤_
≤-antisym (n≤n mn) (n≤n nm) = ≡.cong `_ $ ℕ.≤-antisym mn nm
≤-antisym *≤∞      *≤∞      = refl

≤-trans : Transitive _≤_
≤-trans (n≤n mn) (n≤n np) = n≤n $ ℕ.≤-trans mn np
≤-trans (n≤n _)  *≤∞      = *≤∞
≤-trans *≤∞      *≤∞      = *≤∞

≤-total : Total _≤_
≤-total (` m) (` n) = ⊎.map n≤n n≤n $ ℕ.≤-total m n
≤-total (` _) ∞     = inj₁ *≤∞
≤-total ∞     (` _) = inj₂ *≤∞
≤-total ∞     ∞     = inj₁ *≤∞

≤-isDecTotalOrder : IsDecTotalOrder _≡_ _≤_
≤-isDecTotalOrder =
  record {
    isTotalOrder = record {
      isPartialOrder = record {
        isPreorder = record {
          ≡ ;
          reflexive = λ{refl → ≤-refl} ;
          trans = ≤-trans
        } ;
        antisym = ≤-antisym
      } ;
      total = ≤-total
    } ;
    _≟_ = _≟_ ;
    _≤?_ = _≤?_
  }

≤-isDecPartialOrder : IsDecPartialOrder _≡_ _≤_
≤-isDecPartialOrder = record { IsDecTotalOrder ≤-isDecTotalOrder }

-- n ≼ ∞ for any n, otherwise equality
data _≼_ : Rel ℕ∞ lzero where
  refl : ∀ {n} → n ≼ n
  `≼∞  : ∀ {n} → ` n ≼ ∞
infix 4 _≼_

≼-antisym : Antisymmetric _≡_ _≼_
≼-antisym refl refl = refl

≼-trans : Transitive _≼_
≼-trans refl np   = np
≼-trans `≼∞  refl = `≼∞

_≼?_ : Decidable₂ _≼_
` m ≼? ` n  with m ℕ.≟ n
` m ≼? ` .m | yes refl = yes refl
` m ≼? ` n  | no ¬p    = no λ{refl → ¬p refl}
` _ ≼? ∞    = yes `≼∞
∞   ≼? ` _  = no (λ ())
∞   ≼? ∞    = yes refl
infix 4 _≼?_

≼-isDecPartialOrder : IsDecPartialOrder _≡_ _≼_
≼-isDecPartialOrder =
  record {
    isPartialOrder = record {
      isPreorder = record {
        ≡ ;
        reflexive = λ{refl → refl} ;
        trans = ≼-trans
      } ;
      antisym = ≼-antisym
    } ;
    _≟_ = _≟_ ;
    _≤?_ = _≼?_
  }


_+_ : Op₂ ℕ∞
` m + ` n = ` (m ℕ.+ n)
` _ + ∞   = ∞
∞   + ` _ = ∞
∞   + ∞   = ∞
infixl 6 _+_

_*_ : Op₂ ℕ∞
` m     * ` n     = ` (m ℕ.* n)
` 0     * ∞       = 0
` suc _ * ∞       = ∞
∞       * ` 0     = 0
∞       * ` suc _ = ∞
∞       * ∞       = ∞
infixl 7 _*_


+-isMagma : IsMagma _+_
+-isMagma = record { ≡ ; ∙-cong = ≡.cong₂ _+_ }

+-assoc : Associative _+_
+-assoc (` m) (` n) (` p) = ≡.cong `_ $ ℕ.+-assoc m n p
+-assoc (` _) (` _) ∞     = refl
+-assoc (` _) ∞     (` _) = refl
+-assoc (` _) ∞     ∞     = refl
+-assoc ∞     (` _) (` _) = refl
+-assoc ∞     (` _) ∞     = refl
+-assoc ∞     ∞     (` _) = refl
+-assoc ∞     ∞     ∞     = refl

+-comm : Commutative _+_
+-comm (` m) (` n) = ≡.cong `_ $ ℕ.+-comm m n
+-comm (` n) ∞ = refl
+-comm ∞ (` n) = refl
+-comm ∞ ∞ = refl

+-isSemigroup : IsSemigroup _+_
+-isSemigroup =
  record {
    isMagma = +-isMagma ;
    assoc = +-assoc
  }

+-isCommutativeMonoid : IsCommutativeMonoid _+_ 0
+-isCommutativeMonoid =
  record {
    isSemigroup = +-isSemigroup ;
    identityˡ = λ{(` n) → refl ; ∞ → refl} ;
    comm = +-comm
  }

open IsCommutativeMonoid +-isCommutativeMonoid public
  using ()
  renaming (isMonoid to +-isMonoid ;
            identity to +-identity ;
            identityˡ to +-identityˡ ;
            identityʳ to +-identityʳ)


*-isMagma : IsMagma _*_
*-isMagma = record { ≡ ; ∙-cong = ≡.cong₂ _*_ }

*-assoc : Associative _*_
*-assoc (` m)     (` n)     (` p)     = ≡.cong `_ $ ℕ.*-assoc m n p
*-assoc (` zero)  (` zero)  ∞         = refl
*-assoc (` zero)  (` suc n) ∞         = refl
*-assoc (` suc m) (` zero)  ∞         = *-assoc (` m) (` zero) ∞
*-assoc (` suc m) (` suc n) ∞         = refl
*-assoc (` zero)  ∞         (` zero)  = refl
*-assoc (` zero)  ∞         (` suc p) = refl
*-assoc (` suc m) ∞         (` zero)  rewrite ℕ.*-comm m 0 = refl
*-assoc (` suc m) ∞         (` suc p) = refl
*-assoc (` zero)  ∞         ∞         = refl
*-assoc (` suc m) ∞         ∞         = refl
*-assoc ∞         (` zero)  (` p)     = refl
*-assoc ∞         (` suc n) (` zero)  rewrite ℕ.*-comm n 0 = refl
*-assoc ∞         (` suc n) (` suc p) = refl
*-assoc ∞         (` zero)  ∞         = refl
*-assoc ∞         (` suc n) ∞         = refl
*-assoc ∞         ∞         (` zero)  = refl
*-assoc ∞         ∞         (` suc p) = refl
*-assoc ∞         ∞         ∞         = refl

*-comm : Commutative _*_
*-comm (` m)     (` n)     = ≡.cong `_ $ ℕ.*-comm m n
*-comm (` zero)  ∞         = refl
*-comm (` suc n) ∞         = refl
*-comm ∞         (` zero)  = refl
*-comm ∞         (` suc n) = refl
*-comm ∞         ∞         = refl

*-isSemigroup : IsSemigroup _*_
*-isSemigroup =
  record {
    isMagma = *-isMagma ;
    assoc = *-assoc
  }

*-identityˡ : LeftIdentity 1 _*_
*-identityˡ (` n) = ≡.cong `_ $ ℕ.*-identityˡ n
*-identityˡ ∞ = refl

*-isCommutativeMonoid : IsCommutativeMonoid _*_ 1
*-isCommutativeMonoid =
  record {
    isSemigroup = *-isSemigroup ;
    identityˡ = *-identityˡ ;
    comm = *-comm
  }

open IsCommutativeMonoid *-isCommutativeMonoid public
  using ()
  renaming (isMonoid to *-isMonoid ;
            identity to *-identity ;
            identityʳ to *-identityʳ)

*-distribʳ-+ : _*_ DistributesOverʳ _+_
*-distribʳ-+ (` m)     (` n)     (` p)     = ≡.cong `_ $ ℕ.*-distribʳ-+ m n p
*-distribʳ-+ (` zero)  (` zero)  ∞         = refl
*-distribʳ-+ (` zero)  (` suc n) ∞         = *-distribʳ-+ (` zero) (` n) ∞
*-distribʳ-+ (` suc m) (` n)     ∞         = refl
*-distribʳ-+ (` zero)  ∞         (` zero)  = refl
*-distribʳ-+ (` zero)  ∞         (` suc p) = *-distribʳ-+ (` zero) ∞ (` p)
*-distribʳ-+ (` suc m) ∞         (` p)     = refl
*-distribʳ-+ (` zero)  ∞         ∞         = refl
*-distribʳ-+ (` suc m) ∞         ∞         = refl
*-distribʳ-+ ∞         (` zero)  (` zero)  = refl
*-distribʳ-+ ∞         (` zero)  (` suc p) = refl
*-distribʳ-+ ∞         (` suc n) (` zero)  = refl
*-distribʳ-+ ∞         (` suc n) (` suc p) = refl
*-distribʳ-+ ∞         (` zero)  ∞         = refl
*-distribʳ-+ ∞         (` suc n) ∞         = refl
*-distribʳ-+ ∞         ∞         (` zero)  = refl
*-distribʳ-+ ∞         ∞         (` suc p) = refl
*-distribʳ-+ ∞         ∞         ∞         = refl

*-zeroˡ : LeftZero 0 _*_
*-zeroˡ (` n) = refl
*-zeroˡ ∞     = refl

isCommutativeSemiring : IsCommutativeSemiring _+_ _*_ 0 1
isCommutativeSemiring =
  record {
    +-isCommutativeMonoid = +-isCommutativeMonoid ;
    *-isCommutativeMonoid = *-isCommutativeMonoid ;
    distribʳ = *-distribʳ-+ ;
    zeroˡ = *-zeroˡ
  }

open IsCommutativeSemiring isCommutativeSemiring public
  using (isSemiring)
  renaming (zero to *-zero ;
            zeroʳ to *-zeroʳ ;
            distrib to *-distrib-+ ;
            distribˡ to *-distribˡ-+)


module NoSub where
  module Any where
    isUsages : IsUsages _≡_ _≡_ _≼_ id _+_ _*_ 0 1
    isUsages =
      record {
        isDecEquivalenceʲ = ≡-isDecEquivalence ;
        inj = id ;
        isDecPartialOrderᵗ = ≼-isDecPartialOrder ;
        isSemiringᵗ = isSemiring
      }

  module 0-1 where
    isUsages : IsUsages _≡_ _≡_ _≼_ fromBit _+_ _*_ 0 1
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
        isDecPartialOrderᵗ = ≤-isDecPartialOrder

      }

  module 0-1 where
    isUsages : IsUsages _≡_ _≡_ _≤_ fromBit _+_ _*_ 0 1
    isUsages =
      record {
        IsUsages Any.isUsages using (isDecPartialOrderᵗ ; isSemiringᵗ) ;
        IsUsages NoSub.0-1.isUsages using (isDecEquivalenceʲ ; inj)
      }

  any 0-1 : Usages _ _ _ _ _
  any = record { Any }
  0-1 = record { 0-1 }
