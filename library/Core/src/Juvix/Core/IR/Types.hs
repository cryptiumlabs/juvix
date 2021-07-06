-- | Quantitative type implementation inspired by
--   Atkey 2018 and McBride 2016.
module Juvix.Core.IR.Types
  ( module Juvix.Core.IR.Types,
  )
where

import Juvix.Core.Base.Types
import Juvix.Library hiding (show)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

data T deriving (Data, Show)

extendTerm "Term" [] [t|T|] $ \_ _ -> defaultExtTerm

extendElim "Elim" [] [t|T|] $ \_ _ -> defaultExtElim

extendValue "Value" [] [t|T|] $ \_ _ -> defaultExtValue

extendNeutral "Neutral" [] [t|T|] $ \_ _ -> defaultExtNeutral

extendPattern "Pattern" [] [t|T|] $ \_ _ -> defaultExtPattern

type Datatype = Datatype' T T

type RawDatatype = RawDatatype' T

type DataArg = DataArg' T

type RawDataArg = RawDataArg' T

type DataCon = DataCon' T T

type RawDataCon = RawDataCon' T

type Function = Function' T T

type RawFunction = RawFunction' T

type FunClause = FunClause' T T

type RawFunClause = RawFunClause' T

type Abstract = Abstract' T

type RawAbstract = RawAbstract' T

type Global = Global' T T

type RawGlobal = RawGlobal' T

type Globals primTy primVal = Globals' T T primTy primVal

type RawGlobals primTy primVal = RawGlobals' T primTy primVal

usageToGlobal :: Usage.T -> Maybe GlobalUsage
usageToGlobal Usage.Omega = Just GOmega
usageToGlobal (Usage.SNat 0) = Just GZero
usageToGlobal _ = Nothing

globalToUsage :: GlobalUsage -> Usage.T
globalToUsage GOmega = Usage.Omega
globalToUsage GZero = Usage.SNat 0

globalName :: Global' extT extV primTy primVal -> NameSymbol.T
globalName (GDatatype (Datatype {dataName})) = dataName
globalName (GDataCon (DataCon {conName})) = conName
globalName (GFunction (Function {funName})) = funName
globalName (GAbstract (Abstract {absName})) = absName
