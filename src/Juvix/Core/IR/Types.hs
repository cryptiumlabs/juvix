-- | Quantitative type implementation inspired by
--   Atkey 2018 and McBride 2016.
module Juvix.Core.IR.Types
  ( module Juvix.Core.IR.Types,
    Name (..),
    Term' (..),
    Elim' (..),
    TermAll,
    ElimAll,
  )
where

import Juvix.Core.IR.Types.Base
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (show)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data NoExt

extendTerm "Term" [t|NoExt|] defaultExtTerm

extendElim "Elim" [t|NoExt|] defaultExtElim

-- | Values/types
data Value primTy primVal m
  = VStar Natural
  | VPrimTy primTy
  | VPi
      Usage.T
      (Value primTy primVal m)
      (Value primTy primVal m -> m (Value primTy primVal m))
  | VLam (Value primTy primVal m -> m (Value primTy primVal m))
  | VNeutral (Neutral primTy primVal m)
  | VPrim primVal

-- | A neutral term is either a variable or an application of a neutral term to a value
data Neutral primTy primVal m
  = NFree Name
  | NApp (Neutral primTy primVal m) (Value primTy primVal m)


-- Quotation: takes a value back to a term
quote0 ::
  forall primTy primVal m.
  (Monad m) =>
  Value primTy primVal m ->
  m (Term primTy primVal)
quote0 = quote 0

quote ::
  forall primTy primVal m.
  (Monad m) =>
  Natural ->
  Value primTy primVal m ->
  m (Term primTy primVal)
quote ii p =
  case p of
    VStar nat -> pure (Star nat)
    VPrimTy p -> pure (PrimTy p)
    VPi pi v f -> Pi pi <$> quote ii v <*> (quote (succ ii) =<< f (vfree (Quote ii)))
    VLam func -> Lam <$> (quote (succ ii) =<< func (vfree (Quote ii)))
    VPrim pri -> pure (Elim (Prim pri))
    VNeutral n -> Elim <$> neutralQuote ii n

neutralQuote ::
  forall primTy primVal m.
  (Monad m) =>
  Natural ->
  Neutral primTy primVal m ->
  m (Elim primTy primVal)
neutralQuote ii (NFree x) = pure (boundfree ii x)
neutralQuote ii (NApp n v) = App <$> neutralQuote ii n <*> quote ii v

-- | 'vfree' creates the value corresponding to a free variable
vfree :: Name -> Value primTy primVal m
vfree n = VNeutral (NFree n)

-- checks if the variable occurring at the head of
-- the application is a bound variable or a free name
boundfree :: Natural -> Name -> Elim primTy primVal
boundfree ii (Quote k) = Bound (ii - k - 1)
boundfree _ii x = Free x
