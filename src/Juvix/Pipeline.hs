{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.Pipeline where

import qualified Juvix.Core as Core
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.FromFrontend as FromFrontend
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Frontend as Frontend
import qualified Juvix.FrontendContextualise.InfixPrecedence.Environment as Target
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Data.HashMap.Strict as HM
import Juvix.Library
import Prelude (String)

data Error
  = PipeLine Core.Error
  | ParseErr String
  deriving (Show)

toCore :: [FilePath] -> IO (Either Error Target.FinalContext)
toCore paths = do
  x <- Frontend.ofPath paths
  pure $
    case x of
      Left er -> Left (ParseErr er)
      Right x ->
        case Core.ofFrontend x of
          Left errr -> Left (PipeLine errr)
          Right con -> Right con

contextToCore ::
  (Data primTy, Data primVal) =>
  Target.FinalContext ->
  P.Parameterisation primTy primVal ->
  Either (FromFrontend.Error primTy primVal) (IR.RawGlobals primTy primVal)
contextToCore ctx param =
  FromFrontend.execEnv ctx param do
    Context.traverseContext1_ addSig ctx
    Context.traverseContext1_ addDef ctx
    get @"core"
 where
  addSig x feDef = do
    msig <- FromFrontend.transformSig x feDef
    case msig of
      Just sig -> modify @"coreSigs" $ HM.insert x sig
      Nothing  -> pure ()
  addDef x feDef = do
    defs <- FromFrontend.transformDef x feDef
    for defs \def ->
      modify @"core" $ HM.insert (coreGlobalName def) def

coreGlobalName :: IR.GlobalWith ty ext primTy primVal -> NameSymbol.T
coreGlobalName = \case
  IR.GDatatype (IR.Datatype {dataName}) -> dataName
  IR.GDataCon  (IR.DataCon  {conName})  -> conName
  IR.GFunction (IR.Function {funName})  -> funName
  IR.GAbstract (IR.Abstract {absName})  -> absName
