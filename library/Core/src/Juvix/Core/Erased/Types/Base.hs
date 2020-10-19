{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.Erased.Types.Base where

import Extensible
import Juvix.Core.IR.Types (Universe)
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Type)
import qualified Juvix.Library.HashMap as Map

extensible
  [d|
    data Term primVal
      = Var Symbol
      | Prim primVal
      | -- TODO ∷ add proper lam with capture and arguments here!
        Lam Symbol (Term primVal)
      | Pair (Term primVal) (Term primVal)
      | Let Symbol (Term primVal) (Term primVal)
      | App (Term primVal) (Term primVal)
      deriving (Show, Eq, Generic)

    data Type primTy
      = SymT Symbol
      | Star Universe
      | PrimTy primTy
      | -- TODO: How to deal with dependency?
        Pi Usage.T (Type primTy) (Type primTy)
      | Sig Usage.T (Type primTy) (Type primTy)
      deriving (Show, Eq, Generic)

    type TypeAssignment primTy = Map.T Symbol (Type primTy)
    |]
