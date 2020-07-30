{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.InfixPrecedence.Environment
  ( module Juvix.FrontendContextualise.InfixPrecedence.Environment,
    module Juvix.FrontendContextualise.Environment,
  )
where

import qualified Juvix.Core.Common.Context as Context
import Juvix.FrontendContextualise.Environment
import qualified Juvix.FrontendContextualise.InfixPrecedence.ShuntYard as Shunt
import qualified Juvix.FrontendContextualise.InfixPrecedence.Types as New
import qualified Juvix.FrontendContextualise.ModuleOpen.Types as Old
import Juvix.Library

type Old f =
  f (NonEmpty (Old.FunctionLike Old.Expression)) Old.Signature Old.Type

type New f =
  f (NonEmpty (New.FunctionLike New.Expression)) New.Signature New.Type

type WorkingMaps m =
  ( HasState "old" (Old Context.T) m,
    HasState "new" (New Context.T) m,
    HasThrow "error" Error m
  )

data Environment
  = Env
      { old :: Old Context.T,
        new :: New Context.T
      }
  deriving (Generic)

data Error
  = UnknownSymbol Symbol
  | Clash Shunt.Precedence Shunt.Precedence
  | ImpossibleMoreEles

type ContextAlias =
  ExceptT Error (State Environment)

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
  deriving
    (HasThrow "error" Error)
    via MonadError ContextAlias

runEnv :: Context a -> Old Context.T -> (Either Error a, Environment)
runEnv (Ctx c) old = runState (runExceptT c) (Env old Context.empty)
