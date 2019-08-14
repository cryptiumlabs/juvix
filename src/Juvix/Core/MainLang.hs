-- Quantitative type implementation following
-- Atkey 2017 and McBride 2016.
module Juvix.Core.MainLang where

  import           Prelude
  import           Control.Monad.Except
  import           Numeric.Natural

  -- Inferable terms
  data ITerm
    =  Star Natural             -- (sort i) i th ordering of (closed) universe.
    |  Natural                  -- (Prim) primitive type
    |  Pi Natural CTerm CTerm   -- (PI) dependent function space, the first input (n:Natural)  
                                -- tracks how many times the second input (x:CTerm) is used.
    |  Pm Natural CTerm CTerm   -- dependent multiplicative conjunction (tensor product)
    |  Pa Natural CTerm CTerm   -- dependent additive conjunction type
    |  NPm CTerm CTerm      -- non-dependent multiplicative disjunction type
    |  Bound  Natural       -- Bound variables of type Natural because it's represented by de Bruijn indices
    |  Free   Name          -- Free variables of type name (see below)
    |  ITerm :@: CTerm      -- the infix constructor :@: denotes application
    deriving (Show, Eq)

  --Checkable terms
  data CTerm
    =  Inf  ITerm --(CHK) Inf is the constructor that embeds ITerm to CTerm
    |  Lam  CTerm --(LAM) Lam stands for Lambda abstractions
    deriving (Show, Eq)

  data Name
    =  Global  String -- Global variables are represented by name thus type string
    |  Local   Natural    -- to convert a bound variable into a free one
    |  Quote   Natural
    deriving (Show, Eq)

  --Values/types
  data Value
    = VLam (Value -> Value)
    | VStar Natural
    | VPi Natural Value (Value -> Value)
    | VPm Natural Value (Value -> Value)
    | VPa Natural Value (Value -> Value)
    | VNPm Value Value
    | VNeutral Neutral

  --A neutral term is either a variable or an application of a neutral term to a value
  data Neutral
    = NFree Name
    | NApp Neutral Value
  
  showVal :: Value -> String
  showVal (VLam _)         = "lambda"
  showVal (VStar i)        = "* " ++ show i
  showVal (VPi n v _f)     = "Pi " ++ show n ++ showVal v ++ " -> lambda"
  showVal (VNeutral _n)    = "neutral "
  
  --vfree creates the value corresponding to a free variable
  vfree :: Name -> Value
  vfree n = VNeutral (NFree n)

  --Contexts map variables to their types.
  type Type    = Value
  type Context = [(Name, Type)]

  --Evaluation
  type Env = [Value]
 -- iEval :: ITerm -> Env -> Value