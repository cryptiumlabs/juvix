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
  Either ErasureError ((Erased.Term primVal, Erased.Type primTy), Erased.TypeAssignment primTy)
erase parameterisation term usage ty =
  let (erased, env) = exec (eraseTerm parameterisation term usage ty)
   in erased >>| \erased →
        (erased, typeAssignment env)

exec ∷ EnvErasure primTy primVal a → (Either ErasureError a, Env primTy primVal)
exec (EnvEra env) = runState (runExceptT env) (Env Map.empty [] 0 [])

eraseTerm ∷
  ∀ primTy primVal m.
  ( HasState "typeAssignment" (Erased.TypeAssignment primTy) m,
    HasState "nextName" Int m,
    HasState "nameStack" [Int] m,
    HasThrow "erasureError" ErasureError m,
    HasState "context" (IR.Context primTy primVal (IR.EnvTypecheck primTy primVal)) m,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal
  ) ⇒
  Core.Parameterisation primTy primVal →
  Core.Term primTy primVal →
  Core.Usage →
  Core.Term primTy primVal →
  m (Erased.Term primVal, Erased.Type primTy)
eraseTerm parameterisation term usage ty =
  if usage == Core.SNat 0
    then throw @"erasureError" (CannotEraseZeroUsageTerm (show (term, usage, ty)))
    else case term of
      Core.Star _ → throw @"erasureError" Unsupported
      Core.PrimTy _ → throw @"erasureError" Unsupported
      Core.Pi _ _ _ → throw @"erasureError" Unsupported
      Core.Lam name body → do
        -- The type must be a dependent function.
        let Core.Pi argUsage varTy retTy = ty
        funcTy ← eraseType parameterisation ty
        -- TODO: Is this correct?
        let bodyUsage = Core.SNat 1
        ty ← eraseType parameterisation varTy
        modify @"typeAssignment" (Map.insert name ty)
        let (Right varTyIR, _) = IR.exec (IR.evalTerm parameterisation (hrToIR varTy) [])
        modify @"context" ((:) (IR.Global (show name), (argUsage, varTyIR)))
        (body, _) ← eraseTerm parameterisation body bodyUsage retTy
        -- If argument is not used, just return the erased body.
        -- Otherwise, if argument is used, return a lambda function.
        pure ((if usage <.> argUsage == Core.SNat 0 then body else Erased.Lam name body), funcTy)
      Core.Elim elim → do
        elimTy ← eraseType parameterisation ty
        case elim of
          Core.Var n → pure (Erased.Var n, elimTy)
          Core.Prim p → pure (Erased.Prim p, elimTy)
          Core.App f x → do
            let IR.Elim fIR = hrToIR (Core.Elim f)
            context ← get @"context"
            case fst (IR.exec (IR.typeElim0 parameterisation context fIR)) of
              Left err → throw @"erasureError" (InternalError (show err <> " while attempting to erase " <> show f))
              Right (fUsage, fTy) → do
                let (Right qFTy, _) = IR.exec (IR.quote0 fTy)
                let fty@(Core.Pi argUsage fArgTy _) = irToHR qFTy
                (f, _) ← eraseTerm parameterisation (Core.Elim f) fUsage fty
                if argUsage == Core.SNat 0
                  then pure (f, elimTy)
                  else do
                    (x, _) ← eraseTerm parameterisation x argUsage fArgTy
                    pure (Erased.App f x, elimTy)
          Core.Ann usage term ty → do
            (term, _) ← eraseTerm parameterisation term usage ty
            pure (term, elimTy)

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
    Core.Pi argUsage argTy retTy → do
      arg ← eraseType parameterisation argTy
      ret ← eraseType parameterisation retTy
      pure (Erased.Pi argUsage arg ret)
    Core.Lam _ _ → throw @"erasureError" Unsupported
    Core.Elim elim →
      case elim of
        Core.Var s → pure (Erased.SymT s)
        _ → throw @"erasureError" Unsupported
