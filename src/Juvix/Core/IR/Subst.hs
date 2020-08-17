-- |
-- - Runs a substitution algorithm over core
module Juvix.Core.IR.Subst where

import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Data.HashSet as Set
import qualified Juvix.Core.IR.Types as Types

-- eventually a lot of this code will be replaced by a proper
-- inliner strategy, but this code will serve as the basis for
-- said iterative design

-- TODO Change Symbol to NameSymbol.T
type T primTy primVal = Map.T Types.Name (Types.Term primTy primVal)

type InScopeSet = Set.HashSet Types.Name

-- TODO ∷
-- - add a context for this to run with
-- - Turn the Context, T, InScopeSet to being in the Env
-- - when seeing a new binding determine if we should add that to the Τ type
--   + For now we just ignore extra values to inline, and continue on our way
f ::
  T primTy primVal -> InScopeSet -> Types.Term primTy primVal -> Types.Term primTy primVal
f subst seen (Types.PrimTy ty) = undefined


-- replace :: T primTy primVal -> Types.Name -> Types.Term primTy primVal
-- replace subst name =
--   case Map.lookup subt name of
--     Just term -> term
--     Nothing -> Types.Elim (Types.)
