{-# LANGUAGE ConstraintKinds, TypeFamilies #-}

-- |
-- Extensions to the IR abstract syntax.
module Juvix.Core.IR.Extension
where

import Juvix.Library
import Data.Kind (Type, Constraint)


-- | Marker type for having no extensions
data NoExt

-- | Extensions for terms and eliminations.
--
-- The types 'XStar', 'XPrimTy', etc are extra fields to be added to the
-- corresponding constructor. The types 'TermX' and 'ElimX' are for possible
-- extra constructors. Each type has a default for adding no information ('()'
-- in the former case, 'Void' in the latter).
--
-- For example usages see "Juvix.Core.IRAnn.Types" (extra fields) and
-- [TODO: unification] (extra constructor).
class TEExt ext where
  type XStar   ext primTy primVal
  type XStar   ext primTy primVal = ()
  type XPrimTy ext primTy primVal
  type XPrimTy ext primTy primVal = ()
  type XPi     ext primTy primVal
  type XPi     ext primTy primVal = ()
  type XLam    ext primTy primVal
  type XLam    ext primTy primVal = ()
  type XElim   ext primTy primVal
  type XElim   ext primTy primVal = ()

  type XBound  ext primTy primVal
  type XBound  ext primTy primVal = ()
  type XFree   ext primTy primVal
  type XFree   ext primTy primVal = ()
  type XPrim   ext primTy primVal
  type XPrim   ext primTy primVal = ()
  type XApp    ext primTy primVal
  type XApp    ext primTy primVal = ()
  type XAnn    ext primTy primVal
  type XAnn    ext primTy primVal = ()

  type TermX   ext primTy primVal
  type TermX   ext primTy primVal = Void
  type ElimX   ext primTy primVal
  type ElimX   ext primTy primVal = Void

instance TEExt NoExt

-- | A bundle constraint that requires each type in 'TEExt' to have an instance
-- of @c@.
type TEAll (c :: Type -> Constraint) ext primTy primVal =
  (c (XStar   ext primTy primVal),
   c (XPrimTy ext primTy primVal),
   c (XPi     ext primTy primVal),
   c (XLam    ext primTy primVal),
   c (XElim   ext primTy primVal),
   c (XBound  ext primTy primVal),
   c (XFree   ext primTy primVal),
   c (XPrim   ext primTy primVal),
   c (XApp    ext primTy primVal),
   c (XAnn    ext primTy primVal),
   c (TermX   ext primTy primVal),
   c (ElimX   ext primTy primVal))


-- | Combination of two extensions. Extra fields are pairs of both @e1@ and
-- @e2@\'s extensions, and extra constructors are 'Either' of them.
instance (TEExt e1, TEExt e2) => TEExt (e1, e2) where
  type XStar (e1, e2) primTy primVal =
    (XStar e1 primTy primVal, XStar e2 primTy primVal)
  type XPrimTy (e1, e2) primTy primVal =
    (XPrimTy e1 primTy primVal, XPrimTy e2 primTy primVal)
  type XPi (e1, e2) primTy primVal =
    (XPi e1 primTy primVal, XPi e2 primTy primVal)
  type XLam (e1, e2) primTy primVal =
    (XLam e1 primTy primVal, XLam e2 primTy primVal)
  type XElim (e1, e2) primTy primVal =
    (XElim e1 primTy primVal, XElim e2 primTy primVal)

  type XBound (e1, e2) primTy primVal =
    (XBound e1 primTy primVal, XBound e2 primTy primVal)
  type XFree (e1, e2) primTy primVal =
    (XFree e1 primTy primVal, XFree e2 primTy primVal)
  type XPrim (e1, e2) primTy primVal =
    (XPrim e1 primTy primVal, XPrim e2 primTy primVal)
  type XApp (e1, e2) primTy primVal =
    (XApp e1 primTy primVal, XApp e2 primTy primVal)
  type XAnn (e1, e2) primTy primVal =
    (XAnn e1 primTy primVal, XAnn e2 primTy primVal)

  type TermX (e1, e2) primTy primVal =
    Either (TermX e1 primTy primVal) (TermX e2 primTy primVal)
  type ElimX (e1, e2) primTy primVal =
    Either (ElimX e1 primTy primVal) (ElimX e2 primTy primVal)
