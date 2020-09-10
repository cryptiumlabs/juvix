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

-- useless with debrunj indincies ☹
type InScopeSet = Set.HashSet Types.Name

-- TODO ∷
-- - add a context for this to run with
-- - Turn the Context, T, InScopeSet to being in the Env
-- - when seeing a new binding determine if we should add that to the Τ type
--   + For now we just ignore extra values to inline, and continue on our way
f ::
  T primTy primVal ->
  Types.Term primTy primVal ->
  Types.Term primTy primVal
f subst (Types.PrimTy ty) =
  undefined
f subst (Types.Star uni) =
  undefined
f subst (Types.Pi usage typ body) =
  undefined
f subst (Types.Lam body) =
  undefined
f subst (Types.Let usage bound body)
  | inlineP usage bound =
    let depthPlus = subst ^. depth + 1
        updated =
          T
            { _depth = depthPlus,
              _sub = Map.insert (Bound depthPlus) bound (subst ^. sub)
            }
     in f updated body
  | otherwise =
    Types.Let usage bound (f (over depth succ subst) body)
f subst (Types.Elim elim) =
  undefined

-- TODO ∷
-- - take an environment of some kind
--   + This env should have meta data at some point to better make
--     decisions. Or better yet tag it to the structure itself in some
--     extension.
-- - Make this run on some type of size and usage measurement.
--   + The current metric just checks if the usage is one. However
--     this is bad, as we could move the evaluation in the middle of
--     some computation that degrades performance.

inlineP :: Usage.T -> Types.Elim primTy primVal -> Bool
inlineP usage _
  | usage == one = True
  | otherwise = False
-- replace :: T primTy primVal -> Types.Name -> Types.Term primTy primVal
-- replace subst name =
--   case Map.lookup subt name of
--     Just term -> term
--     Nothing -> Types.Elim (Types.)
