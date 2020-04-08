module Juvix.Core.IR.Typechecker.Log where

import Juvix.Library as Lib
import qualified Juvix.Core.Usage as Usage
import Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Types as IR
import qualified Data.Text as T


type HasLogTc primTy primVal m =
  HasWriter "typecheckerLog" [Log primTy primVal] m

tellLog :: HasLogTc primTy primVal m => Log primTy primVal -> m ()
tellLog msg = tell @"typecheckerLog" [msg]


data Log primTy primVal
  = TermIntro
      (Context primTy primVal)
      (IR.Term primTy primVal)
      (Annotation primTy primVal)
  | ElimIntro (Context primTy primVal) (IR.Elim primTy primVal)
  | Typechecked (IR.Term primTy primVal) (Annotation primTy primVal)
  | TcError (TypecheckError primTy primVal)

  | CheckingStar
  | CheckingSigmaZero
  | SigmaIsZero
  | CheckingLevels
  | LevelOK Natural Natural

  | CheckingPi
  | CheckingPiAnnIsStar (IR.Value primTy primVal)
  | PiAnnIsStar (IR.Value primTy primVal)
  | CheckingPiArg
  | CheckingPiRes

  | CheckingPrimTy

  | CheckingLam
  | LamAnnIsPi
  | LamBodyWith Usage.T Usage.T

  | CheckingElim

  | InferringFree
  | FoundFree (Annotation primTy primVal)

  | InferringPrim

  | InferringApp
  | AppFunIsPi
      (IR.Elim primTy primVal)  -- ^ function
      Usage.T                   -- ^ usage in annotation
      (IR.Value primTy primVal) -- ^ function type
      (IR.Term primTy primVal)  -- ^ argument
  | AppInferredAs Usage.T (IR.Value primTy primVal)

  | InferringAnn
  | CheckingAnnIsType (IR.Term primTy primVal)
  | CheckingAnnTerm (IR.Term primTy primVal) Usage.T (IR.Term primTy primVal)
  deriving (Eq, Show)

describe :: (Show primTy, Show primVal) => Log primTy primVal -> Text
describe (TermIntro ctx tm (Annotation σ ty)) = mconcat
  ["- The current context is\n\t", show ctx, ".\n",
   "  Type checking the term\n\t", show tm, "\n",
   "  against the input annotation with usage of\n\t", show σ, ",\n",
   "  and type of\n\t", show ty, ".\n"]
describe (ElimIntro ctx tm) = mconcat
  ["- The current context is\n\t", show ctx, ".\n",
   "  Inferring the type and usage of elimination\n\t", show tm, ".\n"]
describe (Typechecked tm (Annotation σ ty)) = mconcat
  ["- The term\n\t", show tm, "\nis type checked successfully.\n",
   "  It has usage of\n\t", show σ, "\n",
   "  and type\n\t", show ty, ".\n"]
describe (TcError err) = mconcat
  ["- Type error:\n", indent $ show err]

describe CheckingStar =
  "- Matched a * term.\n"
describe CheckingSigmaZero =
  "- Checking that σ is 0.\n"
describe SigmaIsZero =
  "- The input usage is 0.\n"
describe CheckingLevels =
  "- Checking that the annotation is of type * j, and j > i.\n"
describe (LevelOK i j) = mconcat
  ["- The input annotation is of level\n\t", show j, ",\n",
   "  which is greater than the term's level\n\t", show i, ".\n"]

describe CheckingPi =
  "- Matched a function type.\n"
describe (CheckingPiAnnIsStar ty) = mconcat
  ["- Checking that the input type\n\t", show ty, "\n",
   "  is * i.\n"]
describe (PiAnnIsStar ty) = mconcat
  ["- The input type is\n\t", show ty, ".\n"]
describe CheckingPiArg =
  "- Checking the argument type.\n"
describe CheckingPiRes =
  "- Checking the result type.\n"

describe CheckingPrimTy =
  "- Matched a primitive type term.\n"

describe CheckingLam = mconcat
  ["- Matched a function term.\n",
   "  Checking the input annotation is a function type.\n"]
describe LamAnnIsPi =
  "- Input annotation is a function type.\n"
describe (LamBodyWith σ π) = mconcat
  ["- Checking the body with the argument at usage σ·π, i.e.\n\t",
   show σ, " *\n\t", show π, ".\n"]

describe CheckingElim = mconcat
  ["- Matched an elimination term.\n",
   "  Checking the input type is compatible with the inferred type.\n"]

describe InferringFree =
  "- Matched a free variable. Checking it is in the context.\n"
describe (FoundFree (Annotation π ty)) = mconcat
  ["- Found variable with usage\n\t", show π, "\n",
   "  and type\n\t", show ty, ".\n"]

describe InferringPrim =
  "- Matched a primitive value.\n"

describe InferringApp =
  "- Matched an application.\n"
describe (AppFunIsPi m σ t n) = mconcat
  ["- The function\n\t", show m, "\n",
   "  has usage\n\t", show σ, "\n",
   "  and type\n\t", show t, ".\n",
   "  Checking the argument\n\t", show n, ".\n"]
describe (AppInferredAs σ t) = mconcat
  ["- Application inferred to have usage\n\t", show σ, "\n",
   "  and type\n\t", show t, ".\n"]

describe InferringAnn =
  "- Matched an annotated term."
describe (CheckingAnnIsType t) = mconcat
  ["- Checking the type annotation\n\t", show t, "\n",
   "  is a type.\n"]
describe (CheckingAnnTerm s σ t) = mconcat
  ["- Checking the term\n\t", show s, "\n",
   "  against the usage\n\t", show σ, "\n",
   "  and type\n\t", show t, ".\n"]

indent :: Text -> Text
indent = T.concatMap $ \case
  '\n' -> "\n\t"
  c    -> T.singleton c


data LogType = Info | Fail | Pass
  deriving (Eq, Ord, Show, Enum, Bounded)

logType :: Log primTy primVal -> LogType
logType (TermIntro {})           = Info
logType (ElimIntro {})           = Info
logType (Typechecked {})         = Pass
logType (TcError {})             = Fail
logType (CheckingStar {})        = Info
logType (CheckingSigmaZero {})   = Info
logType (SigmaIsZero {})         = Pass
logType (CheckingLevels {})      = Info
logType (LevelOK {})             = Pass
logType (CheckingPi {})          = Info
logType (CheckingPiAnnIsStar {}) = Info
logType (PiAnnIsStar {})         = Pass
logType (CheckingPiArg {})       = Info
logType (CheckingPiRes {})       = Info
logType (CheckingPrimTy {})      = Info
logType (CheckingLam {})         = Info
logType (LamAnnIsPi {})          = Pass
logType (LamBodyWith {})         = Info
logType (CheckingElim {})        = Info
logType (InferringFree {})       = Info
logType (FoundFree {})           = Pass
logType (InferringPrim {})       = Info
logType (InferringApp {})        = Info
logType (AppFunIsPi {})          = Pass
logType (AppInferredAs {})       = Pass
logType (InferringAnn {})        = Info
logType (CheckingAnnIsType {})   = Info
logType (CheckingAnnTerm {})     = Info
