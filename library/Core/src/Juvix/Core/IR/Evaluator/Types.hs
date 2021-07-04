{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Evaluator.Types
  ( ApplyError (..),
    ApplyErrorPretty,
    Error (..),
    ExtFuns (..),
    rejectExts,
    LookupFun,
  )
where

import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.HR.Pretty as HR
import qualified Juvix.Core.IR.Typechecker.Types as TC
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Core.Translate
import Juvix.Library
import qualified Juvix.Library.PrettyPrint as PP

data ApplyError primTy primVal
  = NoApplyError
  | ApplyErrorV (Param.ApplyError primVal)
  | ApplyErrorT (Param.ApplyError primTy)

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

type instance PP.Ann (ApplyError _ _) = HR.PPAnn

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

data Error extV extT primTy primVal
  = CannotApply
      { fun, arg :: Core.Value' extV primTy primVal,
        paramErr :: ApplyError primTy primVal
      }
  | UnsupportedTermExt (Core.TermX extT primTy primVal)
  | UnsupportedElimExt (Core.ElimX extT primTy primVal)

type instance PP.Ann (Error IR.T TC.T _ _) = HR.PPAnn

-- TODO generalise
instance
  ApplyErrorPretty primTy primVal =>
  PP.PrettyText (Error IR.T TC.T primTy primVal)
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
    Core.ValueAll Eq extV primTy primVal,
    Core.NeutralAll Eq extV primTy primVal,
    Eq (Param.Arg primTy),
    Eq (Param.Arg primVal),
    Eq (Param.ApplyErrorExtra primTy),
    Eq (Param.ApplyErrorExtra primVal),
    Eq (Core.TermX extT primTy primVal),
    Eq (Core.ElimX extT primTy primVal)
  ) =>
  Eq (Error extV extT primTy primVal)

deriving instance
  ( Show primTy,
    Show primVal,
    Core.ValueAll Show extV primTy primVal,
    Core.NeutralAll Show extV primTy primVal,
    Show (Param.Arg primTy),
    Show (Param.Arg primVal),
    Show (Param.ApplyErrorExtra primTy),
    Show (Param.ApplyErrorExtra primVal),
    Show (Core.TermX extT primTy primVal),
    Show (Core.ElimX extT primTy primVal)
  ) =>
  Show (Error extV extT primTy primVal)

type TermExtFun extG extT primTy primVal =
  LookupFun extG primTy primVal ->
  Core.TermX extT primTy primVal ->
  Either (Error IR.T extT primTy primVal) (IR.Value primTy primVal)

type ElimExtFun extG extT primTy primVal =
  LookupFun extG primTy primVal ->
  Core.ElimX extT primTy primVal ->
  Either (Error IR.T extT primTy primVal) (IR.Value primTy primVal)

data ExtFuns extG extT primTy primVal = ExtFuns
  { tExtFun :: TermExtFun extG extT primTy primVal,
    eExtFun :: ElimExtFun extG extT primTy primVal
  }

rejectExts :: ExtFuns extG extT primTy primVal
rejectExts =
  ExtFuns
    { tExtFun = \_ -> Left . UnsupportedTermExt,
      eExtFun = \_ -> Left . UnsupportedElimExt
    }

type LookupFun ext primTy primVal =
  Core.GlobalName -> Maybe (Core.Elim' ext primTy primVal)
