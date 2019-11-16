module Juvix.Core.Erasure.Algorithm
  ( erase,
  )
where

import qualified Juvix.Core.Erased as Erased
import Juvix.Core.Erasure.Types
import qualified Juvix.Core.HR.Types as Core
import qualified Juvix.Core.IR as IR
import Juvix.Core.Translate (hrToIR, irToHR)
import qualified Juvix.Core.Types as Core
import qualified Juvix.Core.Usage as Core
import Juvix.Library hiding (empty)
import qualified Juvix.Library.HashMap as Map

erase ∷
  ∀ primTy primVal.
  (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒
  Core.Parameterisation primTy primVal →
  Core.Term primTy primVal →
  Core.Usage →
  Core.Term primTy primVal →
  Either ErasureError (Erased.Term primVal, Erased.TypeAssignment primTy)
erase parameterisation term usage ty =
  let (erased, env) = exec (eraseTerm parameterisation term usage ty)
   in erased >>| \erased →
        (erased, typeAssignment env)

exec ∷ EnvErasure primTy a → (Either ErasureError a, Env primTy)
exec (EnvEra env) = runState (runExceptT env) (Env Map.empty 0 [])

eraseTerm ∷
  ∀ primTy primVal m.
  ( HasState "typeAssignment" (Erased.TypeAssignment primTy) m,
    HasState "nextName" Int m,
    HasState "nameStack" [Int] m,
    HasThrow "erasureError" ErasureError m,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal
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
        -- The type must be a dependent function.
        let Core.Pi argUsage varTy retTy = ty
        -- retTy is a function, so we match out the body.
        let Core.Lam _ retTyBody = retTy
        -- TODO: Is this correct?
        let bodyUsage = Core.SNat 1
        ty ← eraseType parameterisation varTy
        modify @"typeAssignment" (Map.insert name ty)
        body ← eraseTerm parameterisation body bodyUsage retTyBody
        -- If argument is not used, just return the erased body.
        -- Otherwise, if argument is used, return a lambda function.
        pure (if usage <> argUsage == Core.SNat 0 then body else Erased.Lam name body)
      Core.Elim elim →
        case elim of
          Core.Var n → pure (Erased.Var n)
          Core.Prim p → pure (Erased.Prim p)
          Core.App f x → do
            let IR.Elim fIR = hrToIR (Core.Elim f)
            -- TODO: Correct context, will the empty context work?
            let Right (fUsage, fTy) = IR.iType0 parameterisation [] fIR
                fty@(Core.Pi argUsage fArgTy _) = irToHR (IR.quote0 fTy)
            f ← eraseTerm parameterisation (Core.Elim f) fUsage fty
            if argUsage == Core.SNat 0
              then pure f
              else do
                x ← eraseTerm parameterisation x argUsage fArgTy
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
      -- retTy is a function, so we match out the body.
      let Core.Lam _ retTyBody = retTy
      ret ← eraseType parameterisation retTyBody
      pure (Erased.Pi arg ret)
    Core.Lam _ _ → throw @"erasureError" Unsupported
    Core.Elim elim →
      case elim of
        Core.Var s → pure (Erased.SymT s)
        _ → throw @"erasureError" Unsupported
