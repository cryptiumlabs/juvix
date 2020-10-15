module Metatheory.Value where

open import Prelude
open import QTT
open import Hole
open import Eval


private
 variable
  n : ℕ
  x : Var n
  s t z d w : Term n
  S T : Type n
  π ρ ρᵀ : Usageω n
  e f : Elim n
  𝜋 𝜌 : Usageωᴱ n
  B : Binder n
  o : BinOp n
  K : CoreType
  • : BinOpKind


data Value    {n} : Pred (Term   n) lzero
data Valueᵇ   {n} : Pred (Binder n) lzero
data Neutral  {n} : Pred (Elim   n) lzero
data Neutralᵒ {n} : Pred (BinOp  n) lzero

data Value where
  CORE : Value (CORE K)
  BIND : (VB : Valueᵇ B) (Vt : Value t) → Value (BIND B t)
  0ᵘ   : Value 0ᵘ
  sucᵘ : (Vπ : Value π) → Value (sucᵘ π)
  ↑_   : (Vπ : Value π) → Value (↑ π)
  ωᵘ   : Value ωᵘ
  [_]  : (Ne : Neutral e) → Value [ e ]

infix 1000 ↑_

data Valueᵇ where
  `𝚷[_/_] : (Vπ : Value π) (VS : Value S) → Valueᵇ `𝚷[ π / S ]
  `𝛌      : Valueᵇ `𝛌

data Neutral where
  [`] : Neutral (` x)
  _∙_ : (Nf : Neutral f) (Vs : Value s) → Neutral (f ∙ s)
  bin : (No : Neutralᵒ o) → Neutral (bin o)
  𝓤-elim :
    (VT : Value T) →
    (Vρ : Value ρ) (Vρᵀ : Value ρᵀ) →
    (Vz : Value z) →
    (Vs : Value s) →
    (N𝜋 : Neutral 𝜋) →
    Neutral (𝓤-elim T ρ ρᵀ z s 𝜋)
  𝓤ω-elim :
    (VT : Value T) →
    (Vρ : Value ρ) →
    (Vd : Value d) →
    (Vw : Value w) →
    (N𝜋 : Neutral 𝜋) →
    Neutral (𝓤ω-elim T ρ d w 𝜋)
  -- no _⦂_

data Neutralᵒ where
  fin : (N𝜋 : Neutral 𝜋) (Vρ : Value   ρ) → Neutralᵒ (fin • 𝜋 ρ)
  inf : (N𝜋 : Neutral 𝜋) (N𝜌 : Neutral 𝜌) → Neutralᵒ (inf • 𝜋 𝜌)

pattern _•ᶠ_ N𝜋 Vρ = bin (fin N𝜋 Vρ)
pattern _•ⁱ_ N𝜋 N𝜌 = bin (inf N𝜋 N𝜌)


abstract
  value-⇓    : Value    t → t ⇓ᵗ
  valueᵇ-⇓   : Valueᵇ   B → B ⇓ᵇ
  neutral-⇓  : Neutral  e → e ⇓ᵉ
  neutralᵒ-⇓ : Neutralᵒ o → o ⇓ᵒ

  value-⇓ CORE (_ , make-cs ■ ■ ())

  value-⇓ (BIND VB Vt) (_ , make-cs (BIND-B cs) (BIND-B ct) s) =
    valueᵇ-⇓ VB (-, make-cs cs ct s)
  value-⇓ (BIND VB Vt) (_ , make-cs (BIND-t cs) (BIND-t ct) s) =
    value-⇓ Vt (-, make-cs cs ct s)

  value-⇓ 0ᵘ (_ , make-cs ■ ■ ())

  value-⇓ (sucᵘ Vπ) (_ , make-cs (sucᵘ cs) (sucᵘ ct) s) =
    value-⇓ Vπ (-, make-cs cs ct s)

  value-⇓ (↑ Vπ) (_ , make-cs (↑ cs) (↑ ct) s) =
    value-⇓ Vπ (-, make-cs cs ct s)

  value-⇓ ωᵘ (_ , make-cs ■ ■ ())

  value-⇓ [ () ] (_ , make-cs ■ ■ υ)
  value-⇓ [ Ne ] (_ , make-cs [ cs ] [ ct ] s) =
    neutral-⇓ Ne (-, make-cs cs ct s)

  valueᵇ-⇓ `𝚷[ Vπ / VS ] (_ , make-cs (`𝚷-π cs) (`𝚷-π ct) s) =
    value-⇓ Vπ (-, make-cs cs ct s)
  valueᵇ-⇓ `𝚷[ Vπ / VS ] (_ , make-cs (`𝚷-S cs) (`𝚷-S ct) s) =
    value-⇓ VS (-, make-cs cs ct s)

  valueᵇ-⇓ `𝛌 (_ , make-cs ■ ■ ())

  neutral-⇓ [`] (_ , make-cs ■ ■ ())

  neutral-⇓ (() ∙ Vs) (_ , make-cs ■ ■ β-∙)
  neutral-⇓ (Nf ∙ Vs) (_ , make-cs ([∙ˡ] cs) ([∙ˡ] ct) s) =
    neutral-⇓ Nf (-, make-cs cs ct s)
  neutral-⇓ (Nf ∙ Vs) (_ , make-cs ([∙ʳ] cs) ([∙ʳ] ct) s) =
    value-⇓ Vs (-, make-cs cs ct s)

  neutral-⇓ (bin No) (_ , make-cs (bin cs) (bin ct) s) =
    neutralᵒ-⇓ No (-, make-cs cs ct s)
  neutral-⇓ (() •ᶠ Vρ) (_ , make-cs ■ ■ +-0)
  neutral-⇓ (() •ᶠ Vρ) (_ , make-cs ■ ■ +-s)
  neutral-⇓ (() •ᶠ Vρ) (_ , make-cs ■ ■ *-0)
  neutral-⇓ (() •ᶠ Vρ) (_ , make-cs ■ ■ *-s)
  neutral-⇓ (() •ⁱ N𝜌) (_ , make-cs ■ ■ +ʷ-↑)
  neutral-⇓ (() •ⁱ N𝜌) (_ , make-cs ■ ■ +ʷ-ωˡ)
  neutral-⇓ (N𝜋 •ⁱ ()) (_ , make-cs ■ ■ +ʷ-ωʳ)
  neutral-⇓ (() •ⁱ N𝜌) (_ , make-cs ■ ■ *ʷ-↑)
  neutral-⇓ (() •ⁱ N𝜌) (_ , make-cs ■ ■ *ʷ-0ω)
  neutral-⇓ (() •ⁱ N𝜌) (_ , make-cs ■ ■ *ʷ-ω0)
  neutral-⇓ (() •ⁱ N𝜌) (_ , make-cs ■ ■ *ʷ-sω)
  neutral-⇓ (() •ⁱ N𝜌) (_ , make-cs ■ ■ *ʷ-ωs)
  neutral-⇓ (() •ⁱ N𝜌) (_ , make-cs ■ ■ *ʷ-ωω)

  neutral-⇓ (𝓤-elim VT Vρ Vρᵀ Vz Vs ()) (_ , make-cs ■ ■ β-𝓤-0)
  neutral-⇓ (𝓤-elim VT Vρ Vρᵀ Vz Vs ()) (_ , make-cs ■ ■ β-𝓤-s)
  neutral-⇓ (𝓤-elim VT Vρ Vρᵀ Vz Vs N𝜋) (_ , make-cs (𝓤-elim-T cs) (𝓤-elim-T ct) s) =
    value-⇓ VT (-, make-cs cs ct s)
  neutral-⇓ (𝓤-elim VT Vρ Vρᵀ Vz Vs N𝜋) (_ , make-cs (𝓤-elim-ρ cs) (𝓤-elim-ρ ct) s) =
    value-⇓ Vρ (-, make-cs cs ct s)
  neutral-⇓ (𝓤-elim VT Vρ Vρᵀ Vz Vs N𝜋) (_ , make-cs (𝓤-elim-ρᵀ cs) (𝓤-elim-ρᵀ ct) s) =
    value-⇓ Vρᵀ (-, make-cs cs ct s)
  neutral-⇓ (𝓤-elim VT Vρ Vρᵀ Vz Vs N𝜋) (_ , make-cs (𝓤-elim-z cs) (𝓤-elim-z ct) s) =
    value-⇓ Vz (-, make-cs cs ct s)
  neutral-⇓ (𝓤-elim VT Vρ Vρᵀ Vz Vs N𝜋) (_ , make-cs (𝓤-elim-s cs) (𝓤-elim-s ct) s) =
    value-⇓ Vs (-, make-cs cs ct s)
  neutral-⇓ (𝓤-elim VT Vρ Vρᵀ Vz Vs N𝜋) (_ , make-cs (𝓤-elim-𝜋 cs) (𝓤-elim-𝜋 ct) s) =
    neutral-⇓ N𝜋 (-, make-cs cs ct s)

  neutral-⇓ (𝓤ω-elim VT Vρ Vd Vw ()) (_ , make-cs ■ ■ β-𝓤ω-↑)
  neutral-⇓ (𝓤ω-elim VT Vρ Vd Vw ()) (_ , make-cs ■ ■ β-𝓤ω-ω)
  neutral-⇓ (𝓤ω-elim VT Vρ Vd Vw N𝜋) (_ , make-cs (𝓤ω-elim-T cs) (𝓤ω-elim-T ct) s) =
    value-⇓ VT (-, make-cs cs ct s)
  neutral-⇓ (𝓤ω-elim VT Vρ Vd Vw N𝜋) (_ , make-cs (𝓤ω-elim-ρ cs) (𝓤ω-elim-ρ ct) s) =
    value-⇓ Vρ (-, make-cs cs ct s)
  neutral-⇓ (𝓤ω-elim VT Vρ Vd Vw N𝜋) (_ , make-cs (𝓤ω-elim-d cs) (𝓤ω-elim-d ct) s) =
    value-⇓ Vd (-, make-cs cs ct s)
  neutral-⇓ (𝓤ω-elim VT Vρ Vd Vw N𝜋) (_ , make-cs (𝓤ω-elim-w cs) (𝓤ω-elim-w ct) s) =
    value-⇓ Vw (-, make-cs cs ct s)
  neutral-⇓ (𝓤ω-elim VT Vρ Vd Vw N𝜋) (_ , make-cs (𝓤ω-elim-𝜋 cs) (𝓤ω-elim-𝜋 ct) s) =
    neutral-⇓ N𝜋 (-, make-cs cs ct s)

  neutralᵒ-⇓ (fin N𝜋 Vρ) (_ , make-cs (finˡ cs) (finˡ ct) s) =
    neutral-⇓ N𝜋 (-, make-cs cs ct s)
  neutralᵒ-⇓ (fin N𝜋 Vρ) (_ , make-cs (finʳ cs) (finʳ ct) s) =
    value-⇓ Vρ (-, make-cs cs ct s)
  neutralᵒ-⇓ (inf N𝜋 N𝜌) (_ , make-cs (infˡ cs) (infˡ ct) s) =
    neutral-⇓ N𝜋 (-, make-cs cs ct s)
  neutralᵒ-⇓ (inf N𝜋 N𝜌) (_ , make-cs (infʳ cs) (infʳ ct) s) =
    neutral-⇓ N𝜌 (-, make-cs cs ct s)
