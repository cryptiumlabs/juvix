{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.ToCore.Types where

import Data.HashMap.Strict (HashMap)
import qualified Juvix.Core.Common.Context as Ctx
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding (show)
import qualified Juvix.Library.LineNum as LineNum
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import qualified Juvix.Library.Usage as Usage
import Text.Show (Show (..))

data Error primTy primVal
  = -- features not yet implemented

    -- | constraints are not yet implemented
    ConstraintsUnimplemented NameSymbol.T Sexp.T
  | -- | refinements are not yet implemented
    RefinementsUnimplemented Sexp.T
  | -- | universe polymorphism is not yet implemented
    UniversesUnimplemented Sexp.T
  | -- | implicit arguments are not yet implemented
    ImplicitsUnimplemented Sexp.T
  | -- | implicit arguments are not yet implemented
    ImplicitsUnimplementedA Sexp.T
  | -- | type inference for definitions is not yet implemented
    SigRequired NameSymbol.T (Ctx.Definition Sexp.T Sexp.T Sexp.T)
  | -- | head of application not an Elim
    NotAnElim Sexp.T
  | -- | pattern matching etc not yet implemented
    ExprUnimplemented Sexp.T
  | -- | local datatypes etc not yet implemented
    DefUnimplemented (Ctx.Definition Sexp.T Sexp.T Sexp.T)
  | -- | patterns other than single vars in @let@ not yet implemented
    PatternUnimplemented Sexp.T
  | -- | records not yet implemented
    RecordUnimplemented Sexp.T
  | -- | records not yet implemented
    ExpRecordUnimplemented Sexp.T
  | -- | records not yet implemented
    MatchRecordUnimplemented Sexp.T
  | -- | lists not yet implemented
    ListUnimplemented Sexp.T
  | -- actual errors

    -- | unknown found at declaration level
    UnknownUnsupported (Maybe Symbol)
  | -- | current backend doesn't support this type of constant
    UnsupportedConstant Sexp.T
  | -- | current backend doesn't have this primitive
    UnknownPrimitive NameSymbol.T
  | -- | expression is not a usage
    NotAUsage Sexp.T
  | -- | expression is not 0 or ω
    NotAGUsage Sexp.T
  | -- | expression is not a natural number
    NotAUniverse Sexp.T
  | -- | usage is not 0 or ω
    UsageNotGUsage Usage.T
  | -- | invalid signature for declaration (bug in this module)
    -- @'Just' s@ if @s@ is a signature of the wrong shape,
    -- 'Nothing' if no signature found
    WrongSigType NameSymbol.T (Maybe (CoreSigHR primTy primVal))
  | -- | e.g. single anonymous constructor that is not a record
    InvalidDatatype Sexp.T
  | -- | e.g. ml-style constructor in a datatype with a GADT header
    InvalidConstructor NameSymbol.T Sexp.T
  | -- | type is something other than a set of arrows ending in *
    InvalidDatatypeType NameSymbol.T (HR.Term primTy primVal)
  | -- | Unknown %Builtin.X
    UnknownBuiltin NameSymbol.T
  | -- | Builtin with usage
    BuiltinWithUsage (Ctx.Definition Sexp.T Sexp.T Sexp.T)
  | -- | Builtin with type signature
    BuiltinWithTypeSig (Ctx.Definition Sexp.T Sexp.T Sexp.T)
  | -- | Wrong number of arguments for a builtin
    WrongNumberBuiltinArgs Special Int Sexp.T
  | -- | Using omega as an expression
    UnexpectedOmega
  deriving (Eq, Generic)

instance (Show primTy, Show primVal) => Show (Error primTy primVal) where
  show = \case
    ConstraintsUnimplemented x cons ->
      "Definition " <> show x <> " has constraints\n"
        <> show cons
        <> "\n"
        <> "but constraints are not yet implemented"
    RefinementsUnimplemented r ->
      "Refinement\n" <> show r <> "\n"
        <> "found but refinements are not yet implemented"
    UniversesUnimplemented u ->
      "Universe\n" <> show u <> "\n"
        <> "found but universes in expressions are not yet implemented"
    ImplicitsUnimplemented arr ->
      "Implicit function type\n" <> show arr <> "\n"
        <> "found but implicits are not yet implemented"
    ImplicitsUnimplementedA arg ->
      "Implicit argument\n" <> show arg <> "\n"
        <> "found but implicits are not yet implemented"
    SigRequired x _def ->
      "Signature required for definition " <> show x <> "\n"
        <> "because type inference is not yet implemented"
    NotAnElim exp ->
      "Annotation required on expression\n" <> show exp <> "\n"
        <> "because type inference is not yet implemented"
    ExprUnimplemented exp ->
      "Elaboration of expression\n" <> show exp <> "\n"
        <> "is not yet implemented"
    DefUnimplemented def ->
      "Elaboration of definition\n" <> show def <> "\n"
        <> "is not yet implemented"
    PatternUnimplemented pat ->
      "Elaboration of pattern\n" <> show pat <> "\n"
        <> "is not yet implemented"
    RecordUnimplemented rec ->
      "Elaboration of record\n" <> show rec <> "\n"
        <> "is not yet implemented"
    ExpRecordUnimplemented rec ->
      "Elaboration of record expression\n" <> show rec <> "\n"
        <> "is not yet implemented"
    MatchRecordUnimplemented rec ->
      "Elaboration of record pattern\n" <> show rec <> "\n"
        <> "is not yet implemented"
    ListUnimplemented lst ->
      "Elaboration of list literal\n" <> show lst <> "\n"
        <> "is not yet implemented"
    UnknownUnsupported Nothing ->
      "Nameless unknown found in context"
    UnknownUnsupported (Just x) ->
      "Unknown " <> show x <> " found in context"
    UnsupportedConstant k ->
      "Constant " <> show k <> " unsupported by current backend"
    UnknownPrimitive p ->
      "Primitive " <> show p <> " unsupported by current backend"
    NotAUsage exp ->
      "Expected a usage, but got\n" <> show exp
    NotAGUsage exp ->
      "Expected a global usage, but got\n" <> show exp
    NotAUniverse exp ->
      "Expected a universe, but got\n" <> show exp
    UsageNotGUsage π ->
      "Usage " <> show π <> " cannot be applied to a global"
    WrongSigType x Nothing ->
      "Name " <> show x <> " not in scope\n"
        <> "(probably a bug in the elaborator from frontend)"
    WrongSigType x (Just sig) ->
      "Name " <> show x <> " has the wrong signature form\n"
        <> show sig
        <> "\n"
        <> "(probably a bug in the elaborator from frontend)"
    InvalidDatatype dt ->
      "Invalid datatype\n" <> show dt
    InvalidConstructor x con ->
      "Invalid constructor " <> show x <> " with form\n" <> show con
    InvalidDatatypeType x ty ->
      "Type of datatype " <> show x <> " is\n"
        <> show ty
        <> "\n"
        <> "which is not a valid sort" -- TODO rephrase this
    UnknownBuiltin x ->
      "Unknown builtin " <> show x
    BuiltinWithUsage def ->
      "Builtin binding\n" <> show def <> "\nshould not have a usage"
    BuiltinWithTypeSig def ->
      "Builtin binding\n" <> show def <> "\nshould not have a type signature"
    WrongNumberBuiltinArgs s n args ->
      "Builtin " <> show s <> " should have " <> show n <> " args\n"
        <> "but has been applied to\n"
        <> show args
    UnexpectedOmega ->
      "%Builtin.Omega cannot be used as an arbitrary term, only as\n"
        <> "the first argument of %Builtin.Arrow or %Builtin.Pair"

data CoreSig' ext primTy primVal
  = DataSig
      { dataType :: !(IR.Term' ext primTy primVal),
        dataCons :: [NameSymbol.T]
      }
  | ConSig
      { conType :: !(Maybe (IR.Term' ext primTy primVal)),
        conDef :: !(Maybe (Ctx.Def Sexp.T Sexp.T))
      }
  | ValSig
      { valUsage :: !IR.GlobalUsage,
        valType :: !(IR.Term' ext primTy primVal)
      }
  | SpecialSig !Special
  deriving (Generic)

-- | If two signatures can be merged (currently, only constructor signatures),
-- then do so, otherwise return the *first* unchanged
-- (since @insertWith@ calls it as @mergeSigs new old@).
mergeSigs ::
  CoreSig' ext primTy primVal ->
  CoreSig' ext primTy primVal ->
  CoreSig' ext primTy primVal
mergeSigs (ConSig newTy newDef) (ConSig oldTy oldDef) =
  ConSig (newTy <|> oldTy) (newDef <|> oldDef)
mergeSigs _ second = second

-- | Bindings that can't be given types, but can be given new names by the user.
data Special
  = -- | pi type, possibly with usage already supplied
    ArrowS (Maybe Usage.T)
  | -- | sigma type
    PairS (Maybe Usage.T)
  | -- | type annotation
    ColonS
  | -- | type of types
    TypeS
  | -- | omega usage
    OmegaS
    -- | constructor
  | ConstructorS
  deriving (Eq, Show, Data, Generic)

deriving instance
  ( Eq primTy,
    Eq primVal,
    IR.TermAll Eq ext primTy primVal,
    IR.ElimAll Eq ext primTy primVal
  ) =>
  Eq (CoreSig' ext primTy primVal)

deriving instance
  ( Show primTy,
    Show primVal,
    IR.TermAll Show ext primTy primVal,
    IR.ElimAll Show ext primTy primVal
  ) =>
  Show (CoreSig' ext primTy primVal)

deriving instance Data LineNum.T

deriving instance Data Sexp.Atom

deriving instance Data Sexp.T

deriving instance
  ( Data ext,
    Data primTy,
    Data primVal,
    IR.TermAll Data ext primTy primVal,
    IR.ElimAll Data ext primTy primVal
  ) =>
  Data (CoreSig' ext primTy primVal)

type CoreSigIR = CoreSig' IR.NoExt

type CoreSigHR = CoreSig' HR.T

type CoreSigs' ext primTy primVal =
  HashMap IR.GlobalName (CoreSig' ext primTy primVal)

type CoreSigsIR primTy primVal = CoreSigs' IR.NoExt primTy primVal

type CoreSigsHR primTy primVal = CoreSigs' HR.T primTy primVal

data CoreDef primTy primVal
  = CoreDef !(IR.RawGlobal primTy primVal)
  | SpecialDef !NameSymbol.T !Special
  deriving (Eq, Show, Data, Generic)

data CoreDefs primTy primVal = CoreDefs
  { order :: [NonEmpty NameSymbol.T],
    defs :: CoreMap primTy primVal
  }
  deriving (Eq, Show, Data, Generic)

type CoreMap primTy primVal = HashMap IR.GlobalName (CoreDef primTy primVal)

data FFState primTy primVal = FFState
  { frontend :: Ctx.T Sexp.T Sexp.T Sexp.T,
    param :: P.Parameterisation primTy primVal,
    coreSigs :: CoreSigsHR primTy primVal,
    core :: CoreMap primTy primVal,
    patVars :: HashMap IR.GlobalName IR.PatternVar,
    nextPatVar :: IR.PatternVar
  }
  deriving (Generic)

type EnvAlias primTy primVal =
  ExceptT (Error primTy primVal) (State (FFState primTy primVal))

newtype Env primTy primVal a = Env {unEnv :: EnvAlias primTy primVal a}
  deriving newtype (Functor, Applicative, Monad)
  deriving
    (HasThrow "fromFrontendError" (Error primTy primVal))
    via MonadError (EnvAlias primTy primVal)
  deriving
    ( HasSource "frontend" (Ctx.T Sexp.T Sexp.T Sexp.T),
      HasReader "frontend" (Ctx.T Sexp.T Sexp.T Sexp.T)
    )
    via ReaderField "frontend" (EnvAlias primTy primVal)
  deriving
    ( HasSource "param" (P.Parameterisation primTy primVal),
      HasReader "param" (P.Parameterisation primTy primVal)
    )
    via ReaderField "param" (EnvAlias primTy primVal)
  deriving
    ( HasSource "coreSigs" (CoreSigsHR primTy primVal),
      HasSink "coreSigs" (CoreSigsHR primTy primVal),
      HasState "coreSigs" (CoreSigsHR primTy primVal)
    )
    via StateField "coreSigs" (EnvAlias primTy primVal)
  deriving
    ( HasSource "core" (CoreMap primTy primVal),
      HasSink "core" (CoreMap primTy primVal),
      HasState "core" (CoreMap primTy primVal)
    )
    via StateField "core" (EnvAlias primTy primVal)
  deriving
    ( HasSource "patVars" (HashMap IR.GlobalName IR.PatternVar),
      HasSink "patVars" (HashMap IR.GlobalName IR.PatternVar),
      HasState "patVars" (HashMap IR.GlobalName IR.PatternVar)
    )
    via StateField "patVars" (EnvAlias primTy primVal)
  deriving
    ( HasSource "nextPatVar" IR.PatternVar,
      HasSink "nextPatVar" IR.PatternVar,
      HasState "nextPatVar" IR.PatternVar
    )
    via StateField "nextPatVar" (EnvAlias primTy primVal)

type HasThrowFF primTy primVal =
  HasThrow "fromFrontendError" (Error primTy primVal)

type HasFrontend =
  HasReader "frontend" (Ctx.T Sexp.T Sexp.T Sexp.T)

type HasParam primTy primVal =
  HasReader "param" (P.Parameterisation primTy primVal)

type HasCoreSigs primTy primVal =
  HasState "coreSigs" (CoreSigsHR primTy primVal)

type HasCore primTy primVal =
  HasState "core" (CoreMap primTy primVal)

type HasPatVars =
  HasState "patVars" (HashMap IR.GlobalName IR.PatternVar)

type HasNextPatVar =
  HasState "nextPatVar" IR.PatternVar

execEnv ::
  Ctx.T Sexp.T Sexp.T Sexp.T ->
  P.Parameterisation primTy primVal ->
  Env primTy primVal a ->
  Either (Error primTy primVal) a
execEnv ctx param (Env env) =
  fst $ runIdentity $ runStateT (runExceptT env) initState
  where
    initState =
      FFState
        { frontend = ctx,
          param,
          coreSigs = mempty,
          core = mempty,
          patVars = mempty,
          nextPatVar = 0
        }

throwFF :: HasThrowFF primTy primVal m => Error primTy primVal -> m a
throwFF = throw @"fromFrontendError"
