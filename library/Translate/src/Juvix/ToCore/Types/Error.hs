{-# LANGUAGE UndecidableInstances #-}

module Juvix.ToCore.Types.Error where

import qualified Juvix.Context as Ctx
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.HR as HR
import Juvix.Library hiding (show)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Sexp as Sexp
import Juvix.ToCore.Types.Defs
import Text.Show (Show (..))

data Error ext primTy primVal
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
    WrongSigType NameSymbol.T (Maybe (CoreSig ext primTy primVal))
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
  deriving (Generic)

deriving instance
  ( Core.CoreEq ext primTy primVal,
    Eq primTy,
    Eq primVal
  ) =>
  Eq (Error ext primTy primVal)

-- FIXME replace with PrettyText
instance
  ( Show primTy,
    Show primVal,
    Show ext,
    Core.CoreShow ext primTy primVal
  ) =>
  Show (Error ext primTy primVal)
  where
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
        <> "but has been applied to "
        <> show (length $ Sexp.toList args)
        <> "\n"
        <> show args
    UnexpectedOmega ->
      "%Builtin.Omega cannot be used as an arbitrary term, only as\n"
        <> "the first argument of %Builtin.Arrow or %Builtin.Pair"
