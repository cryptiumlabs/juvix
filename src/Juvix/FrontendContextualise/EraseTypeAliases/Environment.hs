{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.EraseTypeAliases.Environment
  ( module Juvix.FrontendContextualise.EraseTypeAliases.Environment,
    module Juvix.FrontendContextualise.Environment,
  )
where

import qualified Juvix.Core.Common.Context as Context
import Juvix.FrontendContextualise.Environment
import qualified Juvix.FrontendContextualise.EraseTypeAliases.Types as New
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Old
import Juvix.Library

type Old f =
  f (NonEmpty (Old.FunctionLike Old.Expression)) Old.Signature Old.Type

type New f =
  f (NonEmpty (New.FunctionLike New.Expression)) New.Signature New.Type

type WorkingMaps m =
  ( HasState "old" (Old Context.T) m, -- old context
    HasState "new" (New Context.T) m
  )

data Environment
  = Env
      { old :: Old Context.T,
        new :: New Context.T
      }
  deriving (Generic)

type ContextAlias =
  State Environment

newtype Context a
  = Ctx {antiAlias :: ContextAlias a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "old" (Old Context.T),
      HasSink "old" (Old Context.T),
      HasSource "old" (Old Context.T)
    )
    via StateField "old" (ContextAlias)
  deriving
    ( HasState "new" (New Context.T),
      HasSink "new" (New Context.T),
      HasSource "new" (New Context.T)
    )
    via StateField "new" ContextAlias

runEnv :: Context a -> Old Context.T -> Environment
runEnv (Ctx c) old = execState c (Env old Context.empty)
