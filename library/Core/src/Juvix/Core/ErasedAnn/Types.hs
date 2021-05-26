module Juvix.Core.ErasedAnn.Types (
  T,
  TypedPrim,
  Term(..),
  Type(..),
  AnnTerm(..),
  usageFromType,
  piToReturnType,
  piToList,
  piToListTy
  )
where

import Juvix.Core.Application (IsParamVar (..))
import Juvix.Core.IR.Types (Universe)
import Juvix.Core.Parameterisation (TypedPrim')
import Juvix.Library hiding (Type)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

data T

instance IsParamVar T where
  type ParamVar T = NameSymbol.T
  freeVar _ = Just
  boundVar _ _ = Nothing

type TypedPrim ty val = TypedPrim' T ty val

data Term primTy primVal
  = Var NameSymbol.T
  | Prim primVal
  | LamM
      { capture :: [NameSymbol.T], -- Capture
        arguments :: [NameSymbol.T], -- Arguments
        -- the Term in AnnTerm is not lam!
        body :: AnnTerm primTy primVal
      }
  | PairM (AnnTerm primTy primVal) (AnnTerm primTy primVal)
  | UnitM
  | AppM (AnnTerm primTy primVal) [AnnTerm primTy primVal]
  deriving (Show, Eq, Generic)

data Type primTy
  = SymT NameSymbol.T
  | Star Universe
  | PrimTy primTy
  | -- TODO: How to deal with dependency?
    Pi Usage.T (Type primTy) (Type primTy)
  | Sig Usage.T (Type primTy) (Type primTy)
  | UnitTy
  deriving (Show, Eq, Generic)

data AnnTerm primTy primVal = Ann
  { usage :: Usage.T,
    type' :: Type primTy,
    term :: Term primTy primVal
  }
  deriving (Show, Eq, Generic)


-- TODO ∷ make usageFromType Fold!
usageFromType :: Type primTy -> [Usage.T]
usageFromType (Pi usage _x xs) = usage : usageFromType xs
usageFromType Sig {} = []
usageFromType SymT {} = []
usageFromType Star {} = []
usageFromType PrimTy {} = []
usageFromType UnitTy {} = []

piToReturnType :: Type primTy -> Type primTy
piToReturnType (Pi _ _ rest) = piToReturnType rest
piToReturnType last = last

piToList :: Type primTy -> [(Usage.T, Type primTy)]
piToList (Pi usage aType rest) = (usage, aType) : piToList rest
piToList Sig {} = []
piToList SymT {} = []
piToList Star {} = []
piToList PrimTy {} = []
piToList UnitTy {} = []

piToListTy :: Type primTy -> [Type primTy]
piToListTy (Pi _usage ty xs) = ty : piToListTy xs
piToListTy Sig {} = []
piToListTy SymT {} = []
piToListTy Star {} = []
piToListTy PrimTy {} = []
piToListTy UnitTy {} = []

