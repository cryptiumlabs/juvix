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
  pure $
    case x of
      Left er -> Left (ParseErr er)
      Right x ->
        case Core.ofFrontend x of
          Left errr -> Left (PipeLine errr)
          Right con -> Right con

contextToCore :: Target.FinalContext -> P.Parameterisation primTy primVal -> Either FromFrontend.Error [IR.Global primTy primVal]
contextToCore ctx param =
  FromFrontend.execEnv ctx param $ Context.traverseContext1 FromFrontend.transformDef ctx
