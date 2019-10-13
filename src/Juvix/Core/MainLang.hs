-- Quantitative type implementation inspired by
-- Atkey 2017 and McBride 2016.
module Juvix.Core.MainLang where

import Control.Lens ((^?), ix)
import Control.Monad.Except (throwError)
import Juvix.Core.SemiRing
import Juvix.Library hiding (show)
import Numeric.Natural
import Prelude (Show (..), String, error, lookup)

-- | checkable terms
data CTerm
  = -- | (sort i) i th ordering of (closed) universe.
    Star Natural
  | -- | 'Prim' primitive type (naturals)
    Nats
  | -- | formation rule of the dependent function type 'PI'.
    -- the Usage(π) tracks how many times x is used.
    Pi Usage CTerm CTerm
  | -- | 'LAM' Introduction rule of PI.
    -- The abstracted variable's usage is tracked with the Usage(π).
    Lam CTerm
  | -- | 'CONV' conversion rule. TODO make sure 0Γ ⊢ S≡T
    -- 'Conv' is the constructor that embeds ITerm to CTerm
    Conv ITerm
  deriving (Eq)

instance Show CTerm where
  show (Star n) = "* " <> show n
  show Nats = "Nat "
  show (Pi _usage varTy resultTy) =
    "[Π] " <> show varTy <> "-> " <> show resultTy
  show (Lam var) = "\\x. " <> show var
  -- Conv should be invisible to users.
  show (Conv term) = show term

-- | inferable terms
data ITerm
  = -- | Bound variables, in de Bruijn indices
    Bound Natural
  | -- | Free variables of type name (see below)
    Free Name
  | -- | primitive constant (naturals)
    Nat Natural
  | -- | elimination rule of PI (APP).
    App ITerm CTerm
  | -- | Annotation with usage.
    Ann Usage CTerm CTerm
  deriving (Eq)

instance Show ITerm where
  show (Bound i) = "Bound " <> show i -- to be improved
  show (Free name) = show name -- using derived show Name instance, to be improved
  show (Nat i) = show i
  show (App f x) = show f <> show x
  show (Ann pi theTerm theType) =
    show theTerm <> " : [" <> show pi <> "] " <> show theType

data Name
  = -- | Global variables are represented by name thus type string
    Global String
  | -- | to convert a bound variable into a free one
    Local Natural
  | Quote Natural
  deriving (Show, Eq)

-- | Values/types
data Value
  = VStar Natural
  | VNats
  | VPi Usage Value (Value → Value)
  | VLam (Value → Value)
  | VNeutral Neutral
  | VNat Natural

instance Eq Value where
  x == y = quote0 x == quote0 y

instance Show Value where
  show x = show (quote0 x)

-- | A neutral term is either a variable or an application of a neutral term to a value
data Neutral
  = NFree Name
  | NApp Neutral Value
  deriving (Show, Eq)

-- | 'vfree' creates the value corresponding to a free variable
vfree ∷ Name → Value
vfree n = VNeutral (NFree n)

-- | 'Annotations' include usage and type.
type Annotation = (Usage, Value)

-- Contexts map variables to their types.
type Context = [(Name, Annotation)]

-- Evaluation
type Env = [Value]

-- initial environment
initEnv ∷ Env
initEnv = []

cEval ∷ CTerm → Env → Value
cEval (Star i) _d = VStar i
cEval Nats _d = VNats
cEval (Pi pi ty ty') d = VPi pi (cEval ty d) (\x → cEval ty' (x : d))
cEval (Lam e) d = VLam (\x → cEval e (x : d))
cEval (Conv ii) d = iEval ii d

toInt ∷ Natural → Int
toInt = fromInteger . toInteger

-- TODO ∷ Promote iEval and cEval into the maybe monad and all call sites
iEval ∷ ITerm → Env → Value
iEval (Free x) _d = vfree x
iEval (Nat n) _d = VNat n
iEval (App iterm cterm) d = vapp (iEval iterm d) (cEval cterm d)
iEval (Ann _pi term _type) d = cEval term d
iEval (Bound ii) d =
  case d ^? ix (toInt ii) of
    Just x → x
    Nothing → error ("unbound index " <> show ii)

vapp ∷ Value → Value → Value
vapp (VLam f) v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)
vapp x y =
  error
    ( "Application (vapp) error. Cannot apply \n"
        <> show y
        <> "\n to \n"
        <> show x
    )

-- substitution function for checkable terms
cSubst ∷ Natural → ITerm → CTerm → CTerm
cSubst _ii _r (Star i) = Star i
cSubst _ii _r Nats = Nats
cSubst ii r (Pi pi ty ty') = Pi pi (cSubst ii r ty) (cSubst (ii + 1) r ty')
cSubst ii r (Lam f) = Lam (cSubst (ii + 1) r f)
cSubst ii r (Conv e) = Conv (iSubst ii r e)

-- substitution function for inferable terms
iSubst ∷ Natural → ITerm → ITerm → ITerm
iSubst ii r (Bound j)
  | ii == j = r
  | otherwise = Bound j
iSubst _ii _r (Free y) = Free y
iSubst _ii _r (Nat n) = Nat n
iSubst ii r (App it ct) = App (iSubst ii r it) (cSubst ii r ct)
iSubst ii r (Ann pi term t) = Ann pi (cSubst ii r term) (cSubst ii r t)

-- Quotation: takes a value back to a term
quote0 ∷ Value → CTerm
quote0 = quote 0

quote ∷ Natural → Value → CTerm
quote _ii (VStar n) = Star n
quote _ii VNats = Nats
quote ii (VPi pi v f) =
  Pi pi (quote ii v) (quote (ii + 1) (f (vfree (Quote ii))))
quote ii (VLam f) = Lam (quote (ii + 1) (f (vfree (Quote ii))))
quote ii (VNeutral n) = Conv (neutralQuote ii n)
quote _ii (VNat n) = Conv (Nat n)

neutralQuote ∷ Natural → Neutral → ITerm
neutralQuote ii (NFree x) = boundfree ii x
neutralQuote ii (NApp n v) = App (neutralQuote ii n) (quote ii v)

-- checks if the variable occurring at the head of the application is a bound variable or a free name
boundfree ∷ Natural → Name → ITerm
boundfree ii (Quote k) = Bound (ii - k - 1)
boundfree _ii x = Free x

-- error message for inferring/checking types
errorMsg ∷ Natural → CTerm → Annotation → Annotation → String
errorMsg binder cterm expectedT gotT =
  "Type mismatched. \n"
    <> show cterm
    <> " \n (binder number "
    <> show binder
    <> ") is of type \n"
    <> show (show (snd gotT))
    <> " , with "
    <> show (fst gotT)
    <> " usage.\n But the expected type is "
    <> show (show (snd expectedT))
    <> " , with "
    <> show (fst expectedT)
    <> " usage."

-- Type (and usage) checking
type Result a = Either String a --when type checking fails, it throws an error.

-- | 'checker' for checkable terms checks the term against an annotation and returns ().
cType ∷ Natural → Context → CTerm → Annotation → Result ()
cType _ii _g (Star n) ann = do
  unless (SNat 0 == fst ann) (throwError "Sigma has to be 0.") -- checks sigma = 0.
  let ty = snd ann
  case ty of
    VStar j →
      unless
        (n < j)
        ( throwError $
            show (Star n)
              <> " is of type * of a higher universe. But the expected type "
              <> show (snd ann)
              <> " is * of a equal or lower universe."
        )
    _ → throwError $ "* n is of type * but " <> show (snd ann) <> " is not *."
cType ii _g Nats ann =
  unless
    (SNat 0 == fst ann && quote0 (snd ann) == Star 0)
    (throwError (errorMsg ii Nats (zero, VStar 0) ann))
cType ii g (Pi pi varType resultType) ann = do
  unless (SNat 0 == fst ann) (throwError "Sigma has to be 0.") -- checks sigma = 0.
  let ty = snd ann
  case ty of
    VStar _ → do
      cType ii g varType ann -- checks varType is of type Star i
      let ty = cEval varType []
      cType -- checks resultType is of type Star i
        (ii + 1)
        ((Local ii, (pi, ty)) : g)
        (cSubst 0 (Free (Local ii)) resultType)
        ann
    _ →
      throwError
        "The variable type and the result type must be of type * at the same level."
-- (Lam) introduction rule of dependent function type
cType ii g (Lam s) ann =
  case ann of
    (sig, VPi pi ty ty') →
      -- Lam s should be of dependent function type (Pi pi ty ty').
      cType
        (ii + 1)
        ((Local ii, (sig <.> pi, ty)) : g) -- put s in the context with usage sig*pi
        (cSubst 0 (Free (Local ii)) s) -- x (varType) in context S with sigma*pi usage.
        (sig, ty' (vfree (Local ii))) -- is of type M (usage sigma) in context T
    _ → throwError $ show (snd ann) <> " is not a function type but should be."
cType ii g (Conv e) ann = do
  ann' ← iType ii g e
  unless
    (fst ann == fst ann' && quote0 (snd ann) == quote0 (snd ann'))
    (throwError (errorMsg ii (Conv e) ann ann'))

-- inferable terms have type as output.
iType0 ∷ Context → ITerm → Result Annotation
iType0 = iType 0

iTypeErrorMsg ∷ Natural → Name → String
iTypeErrorMsg ii x =
  "Cannot find the type of \n"
    <> show x
    <> "\n (binder number "
    <> show ii
    <> ") in the environment."

iType ∷ Natural → Context → ITerm → Result Annotation
-- the type checker should never encounter a bound variable (as in LambdaPi)? To be confirmed.
iType _ii _g (Bound _) = error "Bound variable cannot be inferred"
iType ii g (Free x) =
  case lookup x g of
    Just ann → return ann
    Nothing → throwError (iTypeErrorMsg ii x)
-- Prim-Const typing rule
iType _ii _g (Nat _) = return (Omega, VNats)
-- App rule, function M applies to N
iType ii g (App m n) = do
  mTy ← iType ii g m -- annotation of M is usage sig and Pi with pi usage.
  case mTy of
    (sig, VPi pi varTy resultTy) → do
      cType ii g n (pi, varTy) -- N has to be of type varTy with usage pi
      return (sig, resultTy (cEval n []))
    _ →
      throwError
        ( show m
            <> "\n (binder number "
            <> show ii
            <> ") is not a function type and thus \n"
            <> show n
            <> "\n cannot be applied to it."
        )
iType ii g (Ann pi theTerm theType) =
  -- TODO check theType is of type Star first? But we have stakable universes now.
  -- cType ii g theType (0, VStar 0)
  do
    let ty = cEval theType []
    cType ii g theTerm (pi, ty)
    return (pi, ty)
