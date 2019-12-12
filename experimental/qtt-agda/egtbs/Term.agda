open import Prelude hiding (Σ ; Σ-syntax ; ⊤ ; _×_ ; #_)
open import Relation.Binary.HeterogeneousEquality as ≅ using (_≅_ ; refl)

open import Scope hiding ([_])
open import Desc


Universe : Set
Universe = ℕ

-- 𝐓 for term, 𝐄 for elim, 𝐔 for usage (which are terms)
data TermSort : Sort where
  𝐓 : TermSort false
  𝐄 : TermSort true
𝐔 = 𝐓

𝐄₀ 𝐓₀ 𝐔₀ 𝐓₁ 𝐔₁ 𝐓₂ 𝐔₂ : Kind TermSort _
𝐄₀ = ◇ ⇒ 𝐄
𝐓₀ = ◇ ⇒ 𝐓             ; 𝐔₀ = 𝐓₀
𝐓₁ = (◇ ⍪ 𝐄₀)      ⇒ 𝐓 ; 𝐔₁ = 𝐓₁
𝐓₂ = (◇ ⍪ 𝐄₀ ⍪ 𝐄₀) ⇒ 𝐓 ; 𝐔₂ = 𝐓₂


data TERM : Set where
  `⋆ `𝓤 `0ᵁ `sucᵁ `+ᵁ `𝚷 `𝛌 `[] : TERM

data ELIM : Set where
  `∙ `⦂ : ELIM

DESC : Desc TermSort lzero
DESC 𝐓 = Σ TERM λ where
  `⋆  → Σ[ _ ∈ Universe ] ⊤
  `𝓤  → ⊤
  `0ᵁ → ⊤
  `sucᵁ → Rec 𝐔₀
  `+ᵁ → Rec 𝐔₀ × Rec 𝐔₀
  `𝚷  → Rec 𝐔₀ × Rec 𝐓₀ × Rec 𝐓₁ 
  `𝛌  → Rec 𝐓₁
  `[] → Rec 𝐄₀
DESC 𝐄 = Σ ELIM λ where
  `∙ → Rec 𝐄₀ × Rec 𝐓₀
  `⦂ → Rec 𝐓₀ × Rec 𝐓₀

Term Usage Elim : KScoped TermSort lzero true
Term  = Termᴿ DESC 𝐓
Usage = Term
Elim  = Termᴿ DESC 𝐄


private
 variable
  ℓ     : Level
  A     : Set ℓ
  x y   : A
  xs ys : Scope A
  S T U : KScoped TermSort lzero true
  θ φ ψ : xs ≤ ys
  ov    : Bool

⋆ : Universe → Term ⇑ xs
⋆ i = ⟨ `⋆ , i , it ⟩ ↑ it

𝓤 : Term ⇑ xs
𝓤 = ⟨ `𝓤 , it ⟩ ↑ it

0ᵁ : Term ⇑ xs
0ᵁ = ⟨ `0ᵁ , it ⟩ ↑ it

sucᵁ : Term ⇑_ ↠ Term ⇑_
sucᵁ (t ↑ θ) = ⟨ `sucᵁ , ◇ \\ t ⟩ ↑ θ

_+ᵁ_ : Term ⇑ xs → Term ⇑ xs → Term ⇑ xs
π +ᵁ ρ with π ,ᴿ ρ
... | pair π′ ρ′ c ↑ ψ = ⟨ `+ᵁ , (pair (◇-\\ π′) (◇-\\ ρ′) c) ⟩ ↑ ψ

𝚷_/_⇒_ : Usage ⇑ xs → Term ⇑ xs → ((◇ ⍪ 𝐄₀) ⊢ Term) ⇑ xs → Term ⇑ xs
𝚷 π / S ⇒ T with π ,ᴿ S ,ᴿ T
... | pair π′ (pair S′ T′ c₁ ↑ φ) c₂ ↑ ψ =
  ⟨ `𝚷 , pair (◇-\\ π′) (pair (◇-\\ S′) T′ c₁ ↑ φ) c₂ ⟩ ↑ ψ
infixr 200 𝚷_/_⇒_

𝛌_ : ((◇ ⍪ 𝐄₀) ⊢ Term) ⇑_ ↠ Term ⇑_
𝛌 (t ↑ ψ) = ⟨ `𝛌 , t ⟩ ↑ ψ
infixr 200 𝛌_

[_] : Elim ⇑_ ↠ Term ⇑_
[ e ↑ θ ] = ⟨ `[] , ◇ \\ e ⟩ ↑ θ


`_ : 𝐄₀ ∈ xs → Elim ⇑ xs
` θ = # (pair (only ↑ ◇ ✓) (lift′ tt ↑ ◇ ✘) (◇ ✓✘)) ↑ θ

_∙_ : Elim ⇑ xs → Term ⇑ xs → Elim ⇑ xs
f ∙ s with f ,ᴿ s
... | pair f′ s′ c ↑ ψ = ⟨ `∙ , pair (◇-\\ f′) (◇-\\ s′) c ⟩ ↑ ψ
infixl 900 _∙_

_⦂_ : Term ⇑ xs → Term ⇑ xs → Elim ⇑ xs
s ⦂ S with s ,ᴿ S
... | pair s′ S′ c ↑ ψ = ⟨ `⦂ , pair (◇-\\ s′) (◇-\\ S′) c ⟩ ↑ ψ
infix 100 _⦂_
