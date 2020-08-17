{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.Pipeline where

import qualified Juvix.Core as Core
import qualified Juvix.Frontend as Frontend
import qualified Juvix.FrontendContextualise.InfixPrecedence.Environment as Target
import Juvix.Library
import Prelude (String)

data Error
  = PipeLine Core.Error
  | ParseErr String

toCore :: [FilePath] -> IO (Either Error Target.FinalContext)
toCore paths = do
  x <- Frontend.ofPath paths
  case x of
    Left er -> pure (Left (ParseErr er))
    Right x ->
      case Core.ofFrontend x of
        Left err -> pure (Left (PipeLine err))
        Right succ -> pure (Right succ)
