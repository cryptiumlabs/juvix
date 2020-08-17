-- |
-- - Runs a substitution algorithm over core
module Juvix.Core.IR.Subst where

import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Data.HashSet as Set
import qualified Juvix.Core.IR.Types as Types

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
f = undefined
