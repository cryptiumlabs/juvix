-- Quantitative type implementation inspired by
-- Atkey 2017 and McBride 2016.
module Juvix.Core.MainLang where

import           Numeric.Natural
import           Prelude

data NatAndw -- semiring of (Nat,w) for usage annotation
  = Natural Natural -- 0, 1, or n usage
  | Omega -- unspecified usage

instance Show NatAndw where
  show (Natural n) = show n
  show Omega       = "w"

instance Eq NatAndw where
  Natural x == Natural y = x == y
  Natural _ == Omega = True
  Omega == _ = True

-- checkable terms
data CTerm
  = Star Natural -- (sort i) i th ordering of (closed) universe.
  | Nat Natural -- (Prim) primitive type
  | Pi NatAndw CTerm CTerm -- formation rule of the dependent function type (PI).
                              -- the NatAndw(π) tracks how many times x is used.
  | Pm NatAndw CTerm CTerm -- dependent multiplicative conjunction (tensor product)
  | Pa NatAndw CTerm CTerm -- dependent additive conjunction type
  | NPm CTerm CTerm -- non-dependent multiplicative disjunction type
  | Lam NatAndw CTerm --(LAM) Introduction rule of PI.
                        -- The abstracted variable's usage is tracked with the NatAndw(π).
  | Conv ITerm --(CONV) conversion rule. TODO make sure 0Γ ⊢ S≡T
              -- Conv is the constructor that embeds ITerm to CTerm
  deriving (Show, Eq)

-- inferable terms
data ITerm
  = Bound Natural -- Bound variables, in de Bruijn indices
  | Free Name -- Free variables of type name (see below)
  | App NatAndw ITerm CTerm -- elimination rule of PI (APP).
                              -- the NatAndw(π) tracks how x is use.
  | Ann NatAndw CTerm CTerm --Annotation with usage.
  deriving (Show, Eq)

data Name
  = Global String -- Global variables are represented by name thus type string
  | Local Natural -- to convert a bound variable into a free one
  | Quote Natural
  deriving (Show, Eq)

--Values/types
data Value
  = VLam Value (Value → Value)
  | VStar Natural
  | VNat Natural
  | VPi NatAndw Value (Value → Value)
  | VPm NatAndw Value (Value → Value)
  | VPa NatAndw Value (Value → Value)
  | VNPm Value Value
  | VNeutral Neutral

--A neutral term is either a variable or an application of a neutral term to a value
data Neutral
  = NFree Name
  | NApp Neutral Value

showVal ∷ Value → String
showVal (VLam _ f) = showFun f
showVal (VStar i) = "*" ++ show i
showVal (VNat i) = show i
showVal (VPi n v f) = "[" ++ show n ++ "]" ++ showVal v ++ " -> " ++ showFun f
showVal (VPm n v f) =
  "([" ++ show n ++ "]" ++ showVal v ++ ", " ++ showFun f ++ ")"
showVal (VPa _ _ _) = "/\\"
showVal (VNPm _ _) = "\\/"
showVal (VNeutral _n) = "neutral "

showFun ∷ (Value → Value) → String
showFun _f = "\\x.t"

--vfree creates the value corresponding to a free variable
vfree ∷ Name → Value
vfree n = VNeutral (NFree n)

--Contexts map variables to their types.
type Type = Value

type Context = [(Name, Type)]

toInt ∷ Natural → Int
toInt = fromInteger . toInteger
{-
--Evaluation
type Env = [Value]

iEval :: ITerm -> Env -> Value
iEval (Star i) _d = VStar i
iEval (Pi Omega ty ty') d = VPi Omega (cEval ty d) (\x -> cEval ty' (x : d))
iEval (Pi (Natural n) ty ty') d =
  VPi (Natural (n - 1)) (cEval ty d) (\x -> cEval ty' (x : d))
iEval (Pm n ty ty') d =
  VPm (Natural (n - 1)) (cEval ty d) (\x -> cEval ty' (x : d))
iEval (Pa n ty ty') d =
  VPa (Natural (n - 1)) (cEval ty d) (\x -> cEval ty' (x : d))
iEval (Free x) _d = vfree x
iEval (Bound ii) d = d !! (toInt ii) --(!!) :: [a] -> Int -> a, the list lookup operator.
iEval (App n iterm cterm) d = vapp (iEval iterm d) (cEval cterm d)
iEval (Nat i) _ = VNat i
iEval (NPm ty ty') d = undefined VNPm ty ty' d

vapp :: Value -> Value -> Value
vapp (VLam n f) v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)
vapp x y =
  error
    ("Application (vapp) error. Cannot apply \n" ++
     showVal y ++ "\n to \n" ++ showVal x)

cEval :: CTerm -> Env -> Value
cEval (Conv ii) d = iEval ii d

--cEval (Lam  e)    d =  VLam (\ x -> cEval e (x : d))
--substitution function for inferable terms
iSubst :: Natural -> ITerm -> ITerm -> ITerm
iSubst ii r (Bound j)
  | ii == j = r
  | otherwise = Bound j
iSubst _ii _r (Free y) = Free y
--iSubst ii r (App iterm cterm) =
iSubst _ii _r (Star i) = Star i

--iSubst ii r (Pi n ty ty')     =  Pi
--substitution function for checkable terms
cSubst :: Natural -> ITerm -> CTerm -> CTerm
cSubst ii r (Conv e) = Conv (iSubst ii r e)
--cSubst ii r (Lam e) =  Lam (cSubst (ii + 1) r e)
-}
