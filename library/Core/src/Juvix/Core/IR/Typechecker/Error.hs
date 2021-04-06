{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Typechecker.Error
  ( TypecheckError' (..),
    TypecheckError,
    HasThrowTC',
    HasThrowTC,
    throwTC,
  )
where

import qualified Juvix.Core.Application as App
import qualified Juvix.Core.HR.Pretty as HR
import qualified Juvix.Core.IR.Evaluator as Eval
import Juvix.Core.IR.Typechecker.Types
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Core.Translate
import Juvix.Library
import qualified Juvix.Library.PrettyPrint as PP
import qualified Juvix.Library.Usage as Usage

data TypecheckError' extV extT primTy primVal
  = TypeMismatch
      { typeSubject :: IR.Elim' extT primTy primVal,
        typeExpected, typeGot :: ValueT' extV primTy primVal
      }
  | UniverseMismatch
      { universeLower, universeHigher :: IR.Universe
      }
  | ShouldBeStar
      { typeActual :: ValueT' extV primTy primVal
      }
  | ShouldBeFunctionType
      { typeActual :: ValueT' extV primTy primVal
      }
  | ShouldBePairType
      { typeActual :: ValueT' extV primTy primVal
      }
  | ShouldBeUnitType
      { typeActual :: ValueT' extV primTy primVal
      }
  | LeftoverUsage
      { usageLeftover :: Usage.T
      }
  | InsufficientUsage
      { usageNeeded, usageActual :: Usage.T
      }
  | UnboundLocal
      { unboundIndex :: IR.BoundVar
      }
  | UnboundGlobal
      { unboundGlobal :: IR.GlobalName
      }
  | UnboundPatVar
      { unboundPatVar :: IR.PatternVar
      }
  | NotPrimTy
      { typeActual :: ValueT' extV primTy primVal
      }
  | WrongPrimTy
      { primVal :: primVal,
        primTy :: P.PrimType primTy
      }
  | UnsupportedTermExt
      { termExt :: IR.TermX extT primTy primVal
      }
  | UnsupportedElimExt
      { elimExt :: IR.ElimX extT primTy primVal
      }
  | PartiallyAppliedConstructor
      { pattern_ :: IR.Pattern' extT primTy primVal
      }
  | EvalError
      { evalErr :: Eval.Error IR.NoExt T primTy (P.TypedPrim primTy primVal)
      }

type TypecheckError = TypecheckError' IR.NoExt IR.NoExt

deriving instance
  ( Eq primTy,
    Eq primVal,
    Eq (P.Arg primTy),
    Eq (P.Arg (P.TypedPrim primTy primVal)),
    Eq (P.ApplyErrorExtra primTy),
    Eq (P.ApplyErrorExtra (P.TypedPrim primTy primVal)),
    IR.ValueAll Eq extV primTy (P.TypedPrim primTy primVal),
    IR.NeutralAll Eq extV primTy (P.TypedPrim primTy primVal),
    IR.TermAll Eq extT primTy primVal,
    Eq (IR.TermX extT primTy (P.TypedPrim primTy primVal)),
    IR.ElimAll Eq extT primTy primVal,
    Eq (IR.ElimX extT primTy (P.TypedPrim primTy primVal)),
    IR.PatternAll Eq extT primTy primVal
  ) =>
  Eq (TypecheckError' extV extT primTy primVal)

deriving instance
  ( Show primTy,
    Show primVal,
    Show (P.Arg primTy),
    Show (P.Arg (P.TypedPrim primTy primVal)),
    Show (P.ApplyErrorExtra primTy),
    Show (P.ApplyErrorExtra (P.TypedPrim primTy primVal)),
    IR.ValueAll Show extV primTy (P.TypedPrim primTy primVal),
    IR.NeutralAll Show extV primTy (P.TypedPrim primTy primVal),
    IR.TermAll Show extT primTy primVal,
    Show (IR.TermX extT primTy (P.TypedPrim primTy primVal)),
    IR.ElimAll Show extT primTy primVal,
    Show (IR.ElimX extT primTy (P.TypedPrim primTy primVal)),
    IR.PatternAll Show extT primTy primVal
  ) =>
  Show (TypecheckError' extV extT primTy primVal)

type instance PP.Ann (TypecheckError' IR.NoExt IR.NoExt _ _) = HR.PPAnn

type Doc = HR.Doc

-- TODO generalise
instance
  ( HR.PrimPretty primTy primVal,
    Eval.ApplyErrorPretty primTy (P.TypedPrim primTy primVal)
  ) =>
  PP.PrettyText (TypecheckError' IR.NoExt IR.NoExt primTy primVal)
  where
  prettyT = \case
    TypeMismatch term exp got ->
      PP.sepIndent'
        [ (False, "Type mismatch at term"),
          (True, prettyHR $ IR.Elim term),
          (False, "expected type:"),
          (True, prettyVal exp),
          (False, "actual type"),
          (True, prettyVal got)
        ]
    UniverseMismatch lo hi ->
      PP.sep
        [ PP.hsep ["Universe", prettySA lo],
          PP.hsep ["should be less than", prettySA hi]
        ]
    ShouldBeStar ty -> expected "a type" ty
    ShouldBeFunctionType ty -> expected "a function" ty
    ShouldBePairType ty -> expected "a pair" ty
    ShouldBeUnitType ty -> expected "a unit" ty
    LeftoverUsage π ->
      -- TODO: leftover usage of what???
      PP.hsep ["Usage", prettySA π, "left over"]
    InsufficientUsage πn πa ->
      -- TODO: insufficient usage of what???
      PP.sep
        [ PP.hsep ["Usage", prettySA πn, "needed but"],
          PP.hsep ["only", prettySA πa, "left over"]
        ]
    UnboundLocal i ->
      PP.hsep ["Unbound local variable", PP.annotate' HR.AName $ PP.show i]
    UnboundGlobal x ->
      PP.hsep
        [ "Name",
          PP.annotate' HR.AName $ PP.noAnn $ PP.prettyT x,
          "not in scope"
        ]
    UnboundPatVar i ->
      PP.hsep ["Unbound pattern variable", PP.annotate' HR.AName $ PP.show i]
    NotPrimTy ty ->
      PP.sepIndent'
        [ (False, "Not a primitive type:"),
          (True, prettyVal ty)
        ]
    WrongPrimTy val ty ->
      PP.sepIndent'
        [ (False, "Primitive value"),
          (True, HR.toPPAnn <$> PP.pretty0 val),
          (False, "cannot be given the type"),
          (True, HR.toPPAnn <$> PP.pretty0 ty)
        ]
    UnsupportedTermExt x -> absurd x -- TODO when generalised
    UnsupportedElimExt x -> absurd x -- TODO when generalised
    PartiallyAppliedConstructor pat ->
      PP.sepIndent'
        [ (False, "Pattern"),
          (True, PP.pretty0 $ fst $ irPatternToHR pat),
          (False, "contains an partially-applied constructor")
        ]
    EvalError err ->
      PP.prettyT err
    where
      expected what ty =
        PP.sepIndent'
          [ (False, PP.hsep ["Expected", what, "but got a term of type"]),
            (True, prettyVal ty)
          ]

prettySA :: Show a => a -> Doc
prettySA = PP.annotate' HR.ATyCon . PP.show

prettyVal :: HR.PrimPretty primTy primVal => IR.Value primTy primVal -> Doc
prettyVal = prettyHR . IR.quote

prettyHR :: HR.PrimPretty primTy primVal => IR.Term primTy primVal -> Doc
prettyHR = PP.pretty0 . irToHR

type HasThrowTC' extV extT primTy primVal m =
  HasThrow "typecheckError" (TypecheckError' extV extT primTy primVal) m

type HasThrowTC primTy primVal m =
  HasThrowTC' IR.NoExt IR.NoExt primTy primVal m

throwTC ::
  HasThrowTC' extV extT primTy primVal m =>
  TypecheckError' extV extT primTy primVal ->
  m z
throwTC = throw @"typecheckError"
