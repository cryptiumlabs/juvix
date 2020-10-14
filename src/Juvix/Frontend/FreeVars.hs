{-# LANGUAGE EmptyCase, UndecidableInstances #-}

module Juvix.Frontend.FreeVars
  (VarSet, FreeVars (..), BoundVars (..))
where

import Juvix.Library
import Juvix.Frontend.Types.Base
import qualified Juvix.Core.Common.NameSymbol as NameSymbol
import Data.HashSet (HashSet, difference)
import GHC.Generics

(\\) :: (Eq a, Hashable a) => HashSet a -> HashSet a -> HashSet a
(\\) = difference


type VarSet = HashSet NameSymbol.T


class FreeVars a where
  freeVars :: a -> VarSet
  default freeVars :: (Generic a, GFreeVars (Rep a)) => a -> VarSet
  freeVars = gfreeVars . from


type ExpressionAll' c ext =
  (ExpressionAll c ext,
   CondAll c ext (Expression' ext),
   LetAll c ext,
   ModuleEAll c ext,
   LetTypeAll c ext,
   MatchAll c ext,
   ModuleOpenExprAll c ext,
   LambdaAll c ext,
   ApplicationAll c ext,
   ListAll c ext,
   TupleAll c ext,
   BlockAll c ext,
   InfixAll c ext,
   ExpRecordAll c ext,
   DoAll c ext,
   ArrowExpAll c ext,
   NamedTypeAll c ext,
   TypeRefineAll c ext,
   UniverseExpressionAll c ext,
   TypeAll c ext)

instance ExpressionAll' FreeVars ext => FreeVars (Expression' ext)

instance (ExpressionAll' FreeVars ext,
          CondAll FreeVars ext a,
          FreeVars a)
  => FreeVars (Cond' ext a)

instance FreeVars (Constant' ext) where freeVars _ = mempty

instance ExpressionAll' FreeVars ext => FreeVars (ModuleE' ext) where
  freeVars (ModE' binds body ext) =
    freeVars (binds, body, ext) \\ boundVars binds
  freeVars (ModuleEX ext) = freeVars ext

instance (ExpressionAll' FreeVars ext, FreeVars a)
  => FreeVars (CondLogic' ext a)

instance ExpressionAll' FreeVars ext => FreeVars (Let' ext) where
  freeVars (Let''' binds body ext) =
    freeVars (binds, body, ext) \\ boundVars binds
  freeVars (LetX ext) = freeVars ext

instance ExpressionAll' FreeVars ext => FreeVars (LetType' ext) where
  freeVars (LetType''' binds body ext) =
    freeVars (binds, body, ext) \\ boundVars binds
  freeVars (LetTypeX ext) = freeVars ext

-- TODO: other syntax instances of FreeVars
-- * they can be empty unless there is some binding structure
-- * ones with a parameter need a more complex context
--   (like for @Cond'@ above)
-- * ExpressionAll' will probably also need some more XyzAlls added


instance FreeVars a => FreeVars [a]
instance FreeVars a => FreeVars (NonEmpty a)
instance FreeVars a => FreeVars (Maybe a)
instance (FreeVars a, FreeVars b) => FreeVars (Either a b)
instance (FreeVars a, FreeVars b) => FreeVars (a, b)
instance (FreeVars a, FreeVars b, FreeVars c) => FreeVars (a, b, c)


class GFreeVars f where gfreeVars :: f t -> VarSet

instance GFreeVars V1 where gfreeVars x = case x of {}
instance GFreeVars U1 where gfreeVars _ = mempty

instance (GFreeVars f, GFreeVars g) => GFreeVars (f :+: g) where
  gfreeVars (L1 x) = gfreeVars x
  gfreeVars (R1 y) = gfreeVars y

instance (GFreeVars f, GFreeVars g) => GFreeVars (f :*: g) where
  gfreeVars (x :*: y) = gfreeVars x <> gfreeVars y

instance GFreeVars f => GFreeVars (M1 i t f) where
  gfreeVars (M1 x) = gfreeVars x

instance FreeVars c => GFreeVars (K1 i c) where
  gfreeVars (K1 x) = freeVars x


class BoundVars a where boundVars :: a -> VarSet

-- TODO: instances for MatchLogic, Type, ...
