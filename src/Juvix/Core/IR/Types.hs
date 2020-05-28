-- | Quantitative type implementation inspired by
--   Atkey 2018 and McBride 2016.
module Juvix.Core.IR.Types
  ( module Juvix.Core.IR.Types,
    Name (..), GlobalName, PatternVar,
    Term' (..),
    Elim' (..),
    TermAll,
    ElimAll,
  )
where

import Juvix.Core.IR.Types.Base
import Juvix.Library hiding (show)

data NoExt

extendTerm "Term" [] [t|NoExt|] $ \_ _ -> defaultExtTerm

extendElim "Elim" [] [t|NoExt|] $ \_ _ -> defaultExtElim

extendValue "Value" [] [t|NoExt|] $ \_ _ -> defaultExtValue

extendNeutral "Neutral" [] [t|NoExt|] $ \_ _ -> defaultExtNeutral

-- Quotation: takes a value back to a term
quote0 :: Value primTy primVal -> Term primTy primVal
quote0 = quote 0

quote :: Natural -> Value primTy primVal -> Term primTy primVal
quote _ (VStar nat) = Star nat
quote _ (VPrimTy p) = PrimTy p
quote ii (VPi π s t) = Pi π (quote ii s) (quote (ii + 1) t)
quote ii (VLam s) = Lam (quote (ii + 1) s)
quote _ (VPrim pri) = Elim (Prim pri)
quote ii (VNeutral n) = Elim $ neutralQuote ii n

neutralQuote :: Natural -> Neutral primTy primVal -> Elim primTy primVal
neutralQuote _ (NBound x) = Bound x
neutralQuote _ (NFree x) = Free x
neutralQuote ii (NApp n v) = App (neutralQuote ii n) (quote ii v)

-- | 'VFree' creates the value corresponding to a free variable
pattern VFree ::
  ( XNFree ext primTy primVal ~ (),
    XVNeutral ext primTy primVal ~ ()
  ) =>
  Name ->
  Value' ext primTy primVal
pattern VFree n = VNeutral' (NFree' n ()) ()

-- | 'VBound' creates the value corresponding to a bound variable
pattern VBound ::
  ( XNBound ext primTy primVal ~ (),
    XVNeutral ext primTy primVal ~ ()
  ) =>
  Natural ->
  Value' ext primTy primVal
pattern VBound n = VNeutral' (NBound' n ()) ()
