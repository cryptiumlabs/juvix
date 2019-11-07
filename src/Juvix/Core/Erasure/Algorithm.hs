module Juvix.Core.Erasure.Algorithm
  ( erase,
  )
where

import qualified Juvix.Core.Erased as Erased
import Juvix.Core.Erasure.Types
import qualified Juvix.Core.HR.Types as Core
import qualified Juvix.Core.Usage as Core
import Juvix.Library hiding (empty)
import Juvix.Utility

erase ∷ Core.Term primTy primVal → Core.Usage → Core.Term primTy primVal → Either ErasureError (Erased.Term primVal, Erased.TypeAssignment primTy)
erase term usage ty =
  let (erased, env) = exec (erase' term usage ty)
   in erased >>| \erased →
        (erased, typeAssignment env)

exec ∷ EnvErasure primTy a → (Either ErasureError a, Env primTy)
exec (EnvEra env) = runState (runExceptT env) (Env empty 0 [])

erase' ∷
  ( HasState "typeAssignment" (Erased.TypeAssignment primTy) m,
    HasState "nextName" Int m,
    HasState "nameStack" [Int] m,
    HasThrow "erasureError" ErasureError m
  ) ⇒
  Core.Term primTy primVal →
  Core.Usage →
  Core.Term primTy primVal →
  m (Erased.Term primVal)
erase' term usage ty =
  case term of
    Core.Lam name body → do
      -- TODO: Instead calculate type of this lambda-bound variable.
      -- let ty = EAC.SymT name
      -- TODO ∷ replace map here with unordered map
      -- then remove the Ord deriving from the Symbol type.
      -- _stk ← get @"nameStack"
      -- modify @"typeAssignment" (insert name ty)
      body ← erase' body usage ty
      pure (Erased.Lam name body)
    {-
        Core.Elim elim → do
          case elim of
            Core.Bound n → do
              name ← unDeBruijin (fromIntegral n)
              pure (EAC.Var name)
            Core.Free n →
              case n of
                Core.Global s → pure (EAC.Var (intern s))
                Core.Local _s → throw @"erasureError" Unsupported
                Core.Quote _s → throw @"erasureError" Unsupported
            Core.App a b → do
              a ← erase (Core.Elim a)
              b ← erase b
              pure (EAC.App a b)
            Core.Ann _ _ a → do
              erase a
    -}
    _ → throw @"erasureError" Unsupported
