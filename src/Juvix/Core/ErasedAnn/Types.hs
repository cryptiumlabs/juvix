module Juvix.Core.ErasedAnn.Types where

import qualified Data.HashMap.Strict as HM
import Juvix.Core.IR.Types (GlobalName, Universe)
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Type)

data Term primTy primVal
  = Var Symbol
  | Prim primVal
  | LamM
      { capture :: [Symbol], -- Capture
        arguments :: [Symbol], -- Arguments
          -- the Term in AnnTerm is not lam!
        body :: AnnTerm primTy primVal
      }
  | AppM (AnnTerm primTy primVal) [AnnTerm primTy primVal]
  deriving (Show, Eq, Generic)

data Type primTy primVal
  = SymT Symbol
  | Star Universe
  | PrimTy primTy
  | -- TODO: How to deal with dependency?
    Pi Usage.T (Type primTy primVal) (Type primTy primVal)
  deriving (Show, Eq, Generic)

data AnnTerm primTy primVal
  = Ann
      { usage :: Usage.T,
        type' :: Type primTy primVal,
        term :: Term primTy primVal
      }
  deriving (Show, Eq, Generic)
{-

type PreGlobals = HM.HashMap GlobalName ErasedGlobal

data ErasedGlobal
  = EGDatatype GlobalName [EDataCon]
  | EGDataCon EDataCon
  | EGFunction GlobalName Value (NonEmpty EFunClause)
  | EGAbstract

data EDataCon
  = EDataCon
      { eDataConName :: GlobalName,
        eDataConType :: Value
      }

data EFunClause
  = EFunClause [Pattern] Term

data Global
  = GDatatype
      { datatypeName :: GlobalName,
        datatypeADT :: ADT
      }
  | GDataCon
      { dataconName :: GlobalName,
        dataconDatatype :: GlobalName,
        dataconIndex :: Int,
        dataconType :: Type
      }
  | GFunction
      { funType :: Type,
        funCases :: Cases
      }

data ADT
  = Prm MU.Type
  | Sum ADT ADT
  | Product ADT ADT

data Con
  = LeftCon
  | RightCon
  | PairCon

data Cases
  = Terminal Term
  | BindVar GlobalName Cases
  | BindPair GlobalName GlobalName Cases
  | LeftRight GlobalName Cases GlobalName Cases

-}
