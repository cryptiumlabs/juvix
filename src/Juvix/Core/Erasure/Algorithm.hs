module Juvix.Core.Erasure.Algorithm (erase) where

import qualified Juvix.Core.Erased as Erased
import qualified Juvix.Core.Erasure.Types as Erasure
import qualified Juvix.Core.HR.Types as HR
import qualified Juvix.Core.IR as IR
import Juvix.Core.Translate (hrToIR, irToHR)
import qualified Juvix.Core.Types as Core
import qualified Juvix.Core.Usage as Core
import Juvix.Library hiding (empty)
import qualified Juvix.Library.HashMap as Map

-- TODO ∷ find out if we can promote this somewhere else
type TermInfo primTy primVal result =
  Core.Parameterisation primTy primVal ->
  HR.Term primTy primVal ->
  Core.Usage ->
  HR.Term primTy primVal ->
  result

erase ::
  (Show primTy, Show primVal, Eq primTy, Eq primVal, Show compErr) =>
  TermInfo primTy primVal
    ( Either
        Erasure.Error
        (Core.AssignWithType primTy primVal compErr)
    )
erase parameterisation term usage ty =
  let (erased, env) = exec (eraseTerm parameterisation term usage ty)
   in erased >>| \(term, type') ->
        Core.WithType
          { Core.termAssign =
              Core.Assignment
                { Core.term = term,
                  Core.assignment = Erasure.typeAssignment env
                },
            Core.type' = type'
          }

exec ::
  Erasure.EnvT ty primVal a -> (Either Erasure.Error a, Erasure.Env ty primVal)
exec (Erasure.EnvEra env) = runState (runExceptT env) (Erasure.Env Map.empty [] 0 [])

eraseTerm ::
  forall primTy primVal m.
  ( HasState "typeAssignment" (Erased.TypeAssignment primTy) m,
    HasState "nextName" Int m,
    HasState "nameStack" [Int] m,
    HasThrow "erasureError" Erasure.Error m,
    HasState "context" (IR.Context primTy primVal) m,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal
  ) =>
  TermInfo primTy primVal (m (Erased.Term primVal, Erased.Type primTy))
eraseTerm parameterisation term usage ty =
  if usage == Core.SNat 0
    then
      throw @"erasureError"
        (Erasure.CannotEraseZeroUsageTerm (show (term, usage, ty)))
    else case term of
      HR.Star _ -> throw @"erasureError" Erasure.Unsupported
      HR.PrimTy _ -> throw @"erasureError" Erasure.Unsupported
      HR.Pi _ _ _ _ -> throw @"erasureError" Erasure.Unsupported
      HR.Lam name body -> do
        -- The type must be a dependent function.
        let HR.Pi argUsage _ varTy retTy = ty
        funcTy <- eraseType parameterisation ty
        -- TODO: Is this correct?
        let bodyUsage = Core.SNat 1
        --
        ty <- eraseType parameterisation varTy
        modify @"typeAssignment" (Map.insert name ty)
        --
        let (Right varTyIR, _) =
              IR.exec (IR.evalTerm parameterisation (hrToIR varTy))
        --
        modify @"context"
          (IR.contextElement (IR.Global $ show name) argUsage varTyIR :)
        (body, _) <- eraseTerm parameterisation body bodyUsage retTy
        -- If argument is not used, just return the erased body.
        -- Otherwise, if argument is used, return a lambda function.
        pure
          ( if usage <.> argUsage == mempty
              then body
              else Erased.Lam name body,
            funcTy
          )
      HR.Elim elim -> do
        elimTy <- eraseType parameterisation ty
        case elim of
          HR.Var n -> pure (Erased.Var n, elimTy)
          HR.Prim p -> pure (Erased.Prim p, elimTy)
          HR.App f x -> do
            let IR.Elim fIR = hrToIR (HR.Elim f)
            context <- get @"context"
            case IR.typeElim0 parameterisation context fIR
              |> fmap IR.getElimAnn
              |> IR.exec
              |> fst of
              Left err ->
                throw @"erasureError"
                  $ Erasure.InternalError
                  $ show err <> " while attempting to erase " <> show f
              Right (IR.Annotation fUsage fTy) -> do
                let qFTy = IR.quote0 fTy
                let fty@(HR.Pi argUsage _ fArgTy _) = irToHR qFTy
                (f, _) <- eraseTerm parameterisation (HR.Elim f) fUsage fty
                if argUsage == mempty
                  then pure (f, elimTy)
                  else do
                    (x, _) <- eraseTerm parameterisation x argUsage fArgTy
                    pure (Erased.App f x, elimTy)
          HR.Ann usage term ty _ -> do
            (term, _) <- eraseTerm parameterisation term usage ty
            pure (term, elimTy)

eraseType ::
  forall primTy primVal m.
  ( HasState "typeAssignment" (Erased.TypeAssignment primTy) m,
    HasThrow "erasureError" Erasure.Error m
  ) =>
  Core.Parameterisation primTy primVal ->
  HR.Term primTy primVal ->
  m (Erased.Type primTy)
eraseType parameterisation term = do
  case term of
    HR.Star n -> pure (Erased.Star n)
    HR.PrimTy p -> pure (Erased.PrimTy p)
    HR.Pi argUsage _ argTy retTy -> do
      arg <- eraseType parameterisation argTy
      ret <- eraseType parameterisation retTy
      pure (Erased.Pi argUsage arg ret)
    -- FIXME might need to check that the name doesn't occur
    -- in @retTy@ anywhere
    HR.Lam _ _ -> throw @"erasureError" Erasure.Unsupported
    HR.Elim elim ->
      case elim of
        HR.Var s -> pure (Erased.SymT s)
        _ -> throw @"erasureError" Erasure.Unsupported
