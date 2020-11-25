-- | Quantitative type implementation inspired by
--   Atkey 2018 and McBride 2016.
module Juvix.Core.IR.Types
  ( module Juvix.Core.IR.Types,
    Name (..),
    GlobalUsage (..),
    GlobalName,
    PatternVar,
    BoundVar,
    Universe,
    DatatypeWith (..),
    RawDatatype',
    Datatype',
    DataArgWith (..),
    RawDataArg',
    DataArg',
    DataConWith (..),
    RawDataCon',
    DataCon',
    FunctionWith (..),
    RawFunction',
    Function',
    FunClause' (..),
    GlobalWith (..),
    RawGlobal',
    Global',
    GlobalsWith,
    RawGlobals',
    Globals',
  )
where

import Juvix.Core.IR.Types.Base
import Juvix.Library hiding (show)
import qualified Juvix.Library.Usage as Usage

data NoExt deriving (Data)

extendTerm "Term" [] [t|NoExt|] $ \_ _ -> defaultExtTerm

extendElim "Elim" [] [t|NoExt|] $ \_ _ -> defaultExtElim

extendValue "Value" [] [t|NoExt|] $ \_ _ -> defaultExtValue

extendNeutral "Neutral" [] [t|NoExt|] $ \_ _ -> defaultExtNeutral

extendPattern "Pattern" [] [t|NoExt|] $ \_ _ -> defaultExtPattern

type Datatype = Datatype' NoExt

type RawDatatype = RawDatatype' NoExt

type DataArg = DataArg' NoExt

type RawDataArg = RawDataArg' NoExt

type DataCon = DataCon' NoExt

type RawDataCon = RawDataCon' NoExt

type Function = Function' NoExt

type RawFunction = RawFunction' NoExt

type FunClause = FunClause' NoExt

-- (no RawFunClause since a clause contains no types anyway)

type Global = Global' NoExt

type RawGlobal = RawGlobal' NoExt

type Globals primTy primVal = Globals' NoExt primTy primVal

type RawGlobals primTy primVal = RawGlobals' NoExt primTy primVal

-- Quotation: takes a value back to a term
quote0 :: Value primTy primVal -> Term primTy primVal
quote0 = quote
{-# DEPRECATED quote0 "use quote directly" #-}

quote :: Value primTy primVal -> Term primTy primVal
quote (VStar nat) = Star nat
quote (VPrimTy p) = PrimTy p
quote (VPi π s t) = Pi π (quote s) (quote t)
quote (VLam s) = Lam (quote s)
quote (VSig π s t) = Sig π (quote s) (quote t)
quote (VPair s t) = Pair (quote s) (quote t)
quote VUnitTy = UnitTy
quote VUnit = Unit
quote (VPrim pri) = Prim pri
quote (VNeutral n) = Elim $ neutralQuote n

neutralQuote :: Neutral primTy primVal -> Elim primTy primVal
neutralQuote (NBound x) = Bound x
neutralQuote (NFree x) = Free x
neutralQuote (NApp n v) = App (neutralQuote n) (quote v)

-- | 'VFree' creates the value corresponding to a free variable
pattern VFree ::
  ( XNFree ext primTy primVal ~ (),
    XVNeutral ext primTy primVal ~ ()
  ) =>
  Name ->
  Value' ext primTy primVal
pattern VFree n = VNeutral' (NFree' n ()) ()

-- | 'VBound' creates the value corresponding to a bound variable
pattern VBound ::
  ( XNBound ext primTy primVal ~ (),
    XVNeutral ext primTy primVal ~ ()
  ) =>
  BoundVar ->
  Value' ext primTy primVal
pattern VBound n = VNeutral' (NBound' n ()) ()

usageToGlobal :: Usage.T -> Maybe GlobalUsage
usageToGlobal Usage.Omega = Just GOmega
usageToGlobal (Usage.SNat 0) = Just GZero
usageToGlobal _ = Nothing

type Signature = Map.Map Name SigDef

data SigDef -- A signature is a mapping of constants to its info
  -- function constant to its type, clauses, whether it's type checked
  = FunSig Value [Clause] Bool
  | ConSig Value -- constructor constant to its type
      -- data type constant to # parameters, positivity of parameters, sized, type
  | DataSig Int [Pos] Sized Value
  deriving (Show)

data Pos -- positivity
  = SPos
  | NSPos
  deriving (Eq, Show)

data Sized -- distinguish between sized and not sized data type.
  = Sized
  | NotSized
  deriving (Eq, Show)

-- declarations are either (inductive) data types or functions
data Declaration
  = -- a data declaration has a name,
    -- the positivity of its parameters,
    -- the telescope for its parameters,
    -- the expression,
    -- the list of constructors.
    DataDecl Name Sized [Pos] Telescope Expr [TypeSig]
  | -- a function declaration has a name, and an expression,
    -- and a list of clauses.
    FunDecl [(TypeSig, [Clause])]
  deriving (Eq, Show)

data TypeSig
  = TypeSig Name Expr
  deriving (Eq, Show)

-- A telescope is a sequence of types where
-- later types may depend on elements of previous types.
-- Used for parameters of data type declarations.
type Telescope = [TBind]

type TBind = (Name, Expr)
