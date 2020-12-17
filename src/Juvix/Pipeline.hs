{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.Pipeline where

import qualified Juvix.Core as Core
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.FromFrontend as FromFrontend
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Frontend as Frontend
import qualified Juvix.FrontendContextualise.InfixPrecedence.Environment as Target
import Juvix.Library
import Prelude (String)

data Error
  = PipeLine Core.Error
  | ParseErr String
  deriving (Show)

toCore :: [FilePath] -> IO (Either Error Target.FinalContext)
toCore paths = do
  x <- Frontend.ofPath paths
  case x of
    Left er -> pure $ Left (ParseErr er)
    Right x -> do
      from <- Core.ofFrontend x
      case from of
        Left errr -> pure $ Left (PipeLine errr)
        Right con -> pure $ Right con

contextToCore ::
  (Data primTy, Data primVal) =>
  Target.FinalContext ->
  P.Parameterisation primTy primVal ->
  Either (FromFrontend.Error primTy primVal) [IR.RawGlobal primTy primVal]
contextToCore ctx param =
  FromFrontend.execEnv ctx param $
    Context.traverseContext1 FromFrontend.transformDef ctx
