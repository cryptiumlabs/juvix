{-# LANGUAGE UndecidableInstances #-}

-- | The types as used by the evaluator.
module Juvix.Core.IR.Evaluator.Types where

import qualified Juvix.Core.HR.Pretty as HR
import qualified Juvix.Core.IR.Typechecker.Types as TC
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Core.Translate
import Juvix.Library
import qualified Juvix.Library.PrettyPrint as PP

-- | Datatype for describing errors during application.
data ApplyError primTy primVal
  = -- | Represent no error.
    NoApplyError
  | -- | Error for primitive values.
    ApplyErrorV (Param.ApplyError primVal)
  | -- | Error for primitive types.
    ApplyErrorT (Param.ApplyError primTy)

-- | Constraint definition for errors that can be pretty-printed.
type ApplyErrorPretty primTy primVal =
  ( PP.PrettyText (Param.ApplyError primTy),
    HR.ToPPAnn (PP.Ann (Param.ApplyError primTy)),
    PP.PrettySyntax primTy,
    HR.ToPPAnn (PP.Ann primTy),
    PP.PrettyText (Param.ApplyError primVal),
    HR.ToPPAnn (PP.Ann (Param.ApplyError primVal)),
    PP.PrettySyntax primVal,
    HR.ToPPAnn (PP.Ann primVal)
  )

-- | Get the corresponding syntax highlighting datatype for `ApplyError`.
type instance PP.Ann (ApplyError _ _) = HR.PPAnn

-- | Pretty printer instance for `ApplyError`.
instance
  ApplyErrorPretty primTy primVal =>
  PP.PrettyText (ApplyError primTy primVal)
  where
  prettyT = \case
    NoApplyError -> mempty
    ApplyErrorV e -> HR.toPPAnn <$> PP.prettyT e
    ApplyErrorT e -> HR.toPPAnn <$> PP.prettyT e

deriving instance
  ( Eq primTy,
    Eq primVal,
    Eq (Param.Arg primTy),
    Eq (Param.Arg primVal),
    Eq (Param.ApplyErrorExtra primTy),
    Eq (Param.ApplyErrorExtra primVal)
  ) =>
  Eq (ApplyError primTy primVal)

deriving instance
  ( Show primTy,
    Show primVal,
    Show (Param.Arg primTy),
    Show (Param.Arg primVal),
    Show (Param.ApplyErrorExtra primTy),
    Show (Param.ApplyErrorExtra primVal)
  ) =>
  Show (ApplyError primTy primVal)

-- | Errors that can occur during evaluation.
data Error extV extT primTy primVal
  = -- | Error during application.
    CannotApply
      { fun, arg :: IR.Value' extV primTy primVal,
        paramErr :: ApplyError primTy primVal
      }
  | -- | Unsupported term extension.
    UnsupportedTermExt (IR.TermX extT primTy primVal)
  | -- | Unsupported elimination extension.
    UnsupportedElimExt (IR.ElimX extT primTy primVal)

-- | Get the corresponding syntax highlighting datatype for `Error`.
type instance PP.Ann (Error IR.NoExt TC.T _ _) = HR.PPAnn

-- TODO generalise

-- | Pretty-printer intance for errors.
instance
  ApplyErrorPretty primTy primVal =>
  PP.PrettyText (Error IR.NoExt TC.T primTy primVal)
  where
  prettyT = \case
    CannotApply {fun, arg, paramErr} ->
      PP.vcat
        [ PP.sepIndent'
            [ (False, "Cannot apply"),
              (True, PP.pretty0 $ irToHR $ IR.quote fun),
              (False, "to argument"),
              (True, PP.pretty0 $ irToHR $ IR.quote arg)
            ],
          PP.prettyT paramErr
        ]
    UnsupportedTermExt x -> absurd x
    UnsupportedElimExt x -> absurd x

deriving instance
  ( Eq primTy,
    Eq primVal,
    IR.ValueAll Eq extV primTy primVal,
    IR.NeutralAll Eq extV primTy primVal,
    Eq (Param.Arg primTy),
    Eq (Param.Arg primVal),
    Eq (Param.ApplyErrorExtra primTy),
    Eq (Param.ApplyErrorExtra primVal),
    Eq (IR.TermX extT primTy primVal),
    Eq (IR.ElimX extT primTy primVal)
  ) =>
  Eq (Error extV extT primTy primVal)

deriving instance
  ( Show primTy,
    Show primVal,
    IR.ValueAll Show extV primTy primVal,
    IR.NeutralAll Show extV primTy primVal,
    Show (Param.Arg primTy),
    Show (Param.Arg primVal),
    Show (Param.ApplyErrorExtra primTy),
    Show (Param.ApplyErrorExtra primVal),
    Show (IR.TermX extT primTy primVal),
    Show (IR.ElimX extT primTy primVal)
  ) =>
  Show (Error extV extT primTy primVal)

-- | Function type for evaluation of extended terms.
type TermExtFun extG extT primTy primVal =
  LookupFun extG primTy primVal ->
  IR.TermX extT primTy primVal ->
  Either (Error IR.NoExt extT primTy primVal) (IR.Value primTy primVal)

-- | Function type for evaluation of extended eliminations.
type ElimExtFun extG extT primTy primVal =
  LookupFun extG primTy primVal ->
  IR.ElimX extT primTy primVal ->
  Either (Error IR.NoExt extT primTy primVal) (IR.Value primTy primVal)

-- | Pair of evaluation functions for extended terms and eliminations.
data ExtFuns extG extT primTy primVal = ExtFuns
  { tExtFun :: TermExtFun extG extT primTy primVal,
    eExtFun :: ElimExtFun extG extT primTy primVal
  }

-- | Function to remove extensions, both for terms and eliminations.
rejectExts :: ExtFuns extG extT primTy primVal
rejectExts =
  ExtFuns
    { tExtFun = \_ -> Left . UnsupportedTermExt,
      eExtFun = \_ -> Left . UnsupportedElimExt
    }

-- | Type synonym for a function that does a lookup for an @IR.Elim'@ based on
-- an @IR.GlobalName@.
type LookupFun ext primTy primVal =
  IR.GlobalName -> Maybe (IR.Elim' ext primTy primVal)
