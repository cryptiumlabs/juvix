-- |
-- - Runs a substitution algorithm over core
module Juvix.Core.IR.Subst where

import Control.Lens
import qualified Data.HashSet as Set
import qualified Juvix.Core.Common.NameSymbol as NameSymbol
import qualified Juvix.Core.IR.Types as Types
import qualified Juvix.Core.Usage as Usage
import Juvix.Library
import qualified Juvix.Library.HashMap as Map

-- eventually a lot of this code will be replaced by a proper
-- inliner strategy, but this code will serve as the basis for
-- said iterative design

data Key
  = Bound Natural
  | Global NameSymbol.T
  deriving (Show, Eq, Generic)

instance Hashable Key

data T primTy primVal
  = T
      { _depth :: Natural,
        _sub :: Map.T Key (Types.Elim primTy primVal)
      }
  deriving (Show)

makeLenses ''T

-- useless with de bruijn indices ☹
type InScopeSet = Set.HashSet Types.Name

-- TODO ∷
-- - add a context for this to run with
-- - Turn the Context, T, InScopeSet to being in the Env
-- - when seeing a new binding determine if we should add that to the Τ type
--   + For now we just ignore extra values to inline, and continue on our way
f ::
  T primTy primVal -> Types.Term primTy primVal -> Types.Term primTy primVal
f _ (Types.PrimTy ty) =
  Types.PrimTy ty
f _ (Types.Star uni) =
  Types.Star uni
f subst (Types.Pi usage typ body) =
  Types.Pi usage (f subst typ) (f (over depth succ subst) body)
f subst (Types.Lam body) =
  Types.Lam (f (over depth succ subst) body)
f subst (Types.Let usage bound body)
  | inlineP usage bound =
    let depthPlus = subst ^. depth + 1
        -- we run a pre-inline for now, however this won't always happen
        newBound = substElim subst bound
        updated =
          T
            { _depth = depthPlus,
              _sub = Map.insert (Bound depthPlus) newBound (subst ^. sub)
            }
     in f updated body
  | otherwise =
    Types.Let usage bound (f (over depth succ subst) body)
f subst (Types.Elim elim) =
  Types.Elim (substElim subst elim)

substElim ::
  T primTy primVal -> Types.Elim primTy primVal -> Types.Elim primTy primVal
substElim subst (Types.Bound var)
  | term >= 0 =
    case (subst ^. sub) Map.!? Bound term of
      -- we only do pre-inlining currently...
      -- need to think how to do post inline techniques
      Just el -> el
      Nothing -> Types.Bound var
  -- The term is free from our context
  | otherwise = Types.Bound var
  where
    term = subst ^. depth - var
substElim subst (Types.Free (Types.Global name)) =
  case (subst ^. sub) Map.!? Global (NameSymbol.fromSymbol name) of
    Just el -> el
    Nothing -> Types.Free (Types.Global name)
substElim _ (Types.Free (Types.Pattern p)) =
  Types.Free (Types.Pattern p)
substElim _ (Types.Prim prim) =
  Types.Prim prim
substElim subst (Types.App fun arg) =
  Types.App (substElim subst fun) (f subst arg)
substElim subst (Types.Ann usage ann term uni) =
  Types.Ann usage (f subst ann) (f subst term) uni

-- TODO ∷
-- - take an environment of some kind
--   + This env should have meta data at some point to better make
--     decisions. Or better yet tag it to the structure itself in some
--     extension.
-- - Make this run on some type of size and usage measurement.
--   + The current metric just checks if the usage is one. However
--     this is bad, as we could move the evaluation in the middle of
--     some computation that degrades performance.

-- | 'inlineP' determines if a function should be inlined
inlineP :: Usage.T -> Types.Elim primTy primVal -> Bool
inlineP usage _
  | usage == one = True
  | otherwise = False

empty :: T primTy primVal
empty = T 0 mempty
