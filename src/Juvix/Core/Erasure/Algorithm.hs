module Juvix.Core.Erasure.Algorithm
  ( erase,
  )
where

import qualified Juvix.Core.Erased as Erased
import Juvix.Core.Erasure.Types
import qualified Juvix.Core.HR.Types as Core
import qualified Juvix.Core.Types as Core
import qualified Juvix.Core.Usage as Core
import Juvix.Library hiding (empty)
import Juvix.Utility

erase ∷ Core.Parameterisation primTy primVal → Core.Term primTy primVal → Core.Usage → Core.Term primTy primVal → Either ErasureError (Erased.Term primVal, Erased.TypeAssignment primTy)
erase parameterisation term usage ty =
  let (erased, env) = exec (eraseTerm parameterisation term usage ty)
   in erased >>| \erased →
        (erased, typeAssignment env)

exec ∷ EnvErasure primTy a → (Either ErasureError a, Env primTy)
exec (EnvEra env) = runState (runExceptT env) (Env empty 0 [])

eraseTerm ∷
  ∀ primTy primVal m.
  ( HasState "typeAssignment" (Erased.TypeAssignment primTy) m,
    HasState "nextName" Int m,
    HasState "nameStack" [Int] m,
    HasThrow "erasureError" ErasureError m
  ) ⇒
  Core.Parameterisation primTy primVal →
  Core.Term primTy primVal →
  Core.Usage →
  Core.Term primTy primVal →
  m (Erased.Term primVal)
eraseTerm parameterisation term usage ty =
  if usage == Core.SNat 0
    then throw @"erasureError" CannotEraseZeroUsageTerm
    else case term of
      Core.Star _ → throw @"erasureError" Unsupported
      Core.PrimTy _ → throw @"erasureError" Unsupported
      Core.Pi _ _ _ → throw @"erasureError" Unsupported
      Core.Lam name body → do
        let Core.Pi argUsage varTy retTy = ty
            bodyUsage = Core.SNat 1
        ty ← eraseType parameterisation varTy
        modify @"typeAssignment" (insert name ty)
        -- TODO resTy is a function, which we must deal with.
        body ← eraseTerm parameterisation body bodyUsage retTy
        -- If argument is not used, just return the erased body.
        if usage <> argUsage == Core.SNat 0
          then pure body
          else-- Otherwise, if argument is used, return a lambda function.

            pure (Erased.Lam name body)
      Core.Elim elim →
        case elim of
          Core.Var n → pure (Erased.Var n)
          Core.Prim p → pure (Erased.Prim p)
          Core.App f x → do
            -- TODO Find type of f, if f uses x erase to app else erase to f.
            f ← eraseTerm parameterisation (Core.Elim f) undefined undefined
            -- TODO Find type of x (determined by f arg type & usage).
            x ← eraseTerm parameterisation x undefined undefined
            pure (Erased.App f x)
          Core.Ann usage term ty → eraseTerm parameterisation term usage ty

eraseType ∷
  ∀ primTy primVal m.
  ( HasState "typeAssignment" (Erased.TypeAssignment primTy) m,
    HasThrow "erasureError" ErasureError m
  ) ⇒
  Core.Parameterisation primTy primVal →
  Core.Term primTy primVal →
  m (Erased.Type primTy)
eraseType parameterisation term = do
  case term of
    Core.Star n → pure (Erased.Star n)
    Core.PrimTy p → pure (Erased.PrimTy p)
    Core.Pi _ argTy retTy → do
      arg ← eraseType parameterisation argTy
      -- TODO retTy is a function on the arg value
      ret ← eraseType parameterisation retTy
      pure (Erased.Pi arg ret)
    Core.Lam _ _ → throw @"erasureError" Unsupported
    Core.Elim elim →
      case elim of
        Core.Var s → pure (Erased.SymT s)
        _ → throw @"erasureError" Unsupported
