module Juvix.Backends.Michelson.Compilation.Types where

import           Control.Monad.Writer
import           Protolude

import           Juvix.Backends.Michelson.Typed
import qualified Juvix.Backends.Michelson.Untyped as U
import qualified Juvix.Lang                       as J
import           Juvix.Utility

data TypecheckError
  = CannotCastAs DynamicError
  | NotImplemented
  | Wrapped Text TypecheckError

data CompilationError
  = NotYetImplemented Text
  | InvalidInput Text
  | InternalFault Text
  | DidNotTypecheck TypecheckError

data CompilationLog
  = ExprToExpr J.Expr U.Expr
  | Compiling Text
  | Optimized SomeExpr SomeExpr
  | TryLift U.Expr DynamicType DynamicType

instance PrettyPrint DynamicError where
  prettyPrintValue (CannotCast (DynamicValue v) _)                      = "cannot cast " <> prettyPrintValue v <> " to "
  prettyPrintValue (CannotUnify (Proxy :: Proxy a) (Proxy :: Proxy b))  = "cannot unify " <> prettyPrintType (undefined :: a) <> " with " <> prettyPrintType (undefined :: b)
  prettyPrintValue (NotAnOptionType prx) = "not an option type: " <> prettyPrintProxy prx
  prettyPrintValue (NotAProductType prx) = "not a product type: " <> prettyPrintProxy prx
  prettyPrintValue (NotASumType prx)     = "not a sum type: " <> prettyPrintProxy prx
  prettyPrintValue (NotAnArrowType prx)  = "not an arrow type: " <> prettyPrintProxy prx

instance PrettyPrint TypecheckError where
  prettyPrintValue = \case
    Wrapped t e      -> prettyPrintValue e <> "; " <> t
    CannotCastAs e   -> "cannot cast: " <> prettyPrintValue e
    NotImplemented   -> "not yet implemented"

instance PrettyPrint CompilationError where
  prettyPrintValue = \case
    NotYetImplemented m -> "not yet implemented: " <> m
    InvalidInput m      -> "invalid input: " <> m
    InternalFault m     -> "internal compilation fault: " <> m <> " - this is a bug in Juvix and should be reported"
    DidNotTypecheck e   -> "did not typecheck: " <> prettyPrintValue e

instance PrettyPrint CompilationLog where
  prettyPrintValue = \case
    ExprToExpr a b    -> "expr to expr: " <> prettyPrintValue a <> " to " <> prettyPrintValue b
    Compiling t       -> "compiling: " <> t
    Optimized a b     -> "optimized: " <> prettyPrintValue a <> " to " <> prettyPrintValue b
    TryLift a b c     -> "try lift: " <> prettyPrintValue a <> " at stack " <> prettyPrintValue b <> " and storage " <> prettyPrintValue c
