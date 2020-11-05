{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.FrontendContextualise.InfixPrecedence.Environment
  ( module Juvix.FrontendContextualise.InfixPrecedence.Environment,
    module Juvix.FrontendContextualise.Environment,
  )
where

import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as Local
import Juvix.FrontendContextualise.Environment
import qualified Juvix.FrontendContextualise.InfixPrecedence.ShuntYard as Shunt
import qualified Juvix.FrontendContextualise.InfixPrecedence.Types as New
import qualified Juvix.FrontendContextualise.ModuleOpen.Types as Old
import qualified Juvix.Library
import Juvix.Library hiding (ask)

type Old f =
  f (NonEmpty (Old.FunctionLike Old.Expression)) Old.Signature Old.Type

type New f =
  f (NonEmpty (New.FunctionLike New.Expression)) New.Signature New.Type

data EnvDispatch = EnvDispatch

type WorkingMaps tag m =
  ( HasState "old" (Old Context.T) m,
    HasState "new" (New Context.T) m,
    Query tag m,
    HasReader "dispatch" tag m,
    HasThrow "error" Error m
  )

data Environment
  = Env
      { old :: Old Context.T,
        new :: New Context.T,
        dispatch :: EnvDispatch
      }
  deriving (Generic)

type FinalContext = New Context.T

class Query a m where
  queryInfo' ::
    SymbLookup sym => sym -> a -> m (Maybe [Context.Information])

instance WorkingMaps t m => Query EnvDispatch m where
  queryInfo' sym EnvDispatch = do
    looked <- lookup sym
    lookedO <- ask sym
    let -- dealing with the second map winning lookup
        -- Main rules:
        -- Local Private > Any
        -- Local         > Global
        -- Global        > Nothing
        chooseProperScope (Just (Context.Outside _)) (Just (Context.Current x)) =
          extractInformation (Local.extractValue x)
        chooseProperScope Nothing (Just (Context.Current x)) =
          extractInformation (Local.extractValue x)
        chooseProperScope _ (Just (Context.Current (Local.Priv x))) =
          extractInformation x
        chooseProperScope Nothing (Just (Context.Outside x)) =
          extractInformation x
        -- dealing with the first map winning lookup
        chooseProperScope (Just x) _ =
          extractInformation (Context.extractValue x)
        chooseProperScope Nothing Nothing =
          Nothing
        extractInformation (Context.Def {precedence}) =
          Just [Context.Prec precedence]
        extractInformation (Context.Information is) =
          Just is
        extractInformation _ = Nothing
    pure (chooseProperScope lookedO looked)

queryInfo ::
  (HasReader "dispatch" a m, Query a m, SymbLookup sym) =>
  sym ->
  m (Maybe [Context.Information])
queryInfo s = Juvix.Library.ask @"dispatch" >>= queryInfo' s

data Error
  = UnknownSymbol Context.NameSymbol
  | Clash
      (Shunt.Precedence Context.NameSymbol)
      (Shunt.Precedence Context.NameSymbol)
  | ImpossibleMoreEles
  | PathError Context.NameSymbol
  deriving (Show)

type ContextAlias =
  ExceptT Error (State Environment)

newtype Context a = Ctx {antiAlias :: ContextAlias a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "old" (Old Context.T),
      HasSink "old" (Old Context.T),
      HasSource "old" (Old Context.T)
    )
    via StateField "old" ContextAlias
  deriving
    ( HasState "new" (New Context.T),
      HasSink "new" (New Context.T),
      HasSource "new" (New Context.T)
    )
    via StateField "new" ContextAlias
  deriving
    ( HasReader "dispatch" EnvDispatch,
      HasSource "dispatch" EnvDispatch
    )
    via ReaderField "dispatch" ContextAlias
  deriving
    (HasThrow "error" Error)
    via MonadError ContextAlias

runEnv :: Context a -> Old Context.T -> (Either Error a, Environment)
runEnv (Ctx c) old =
  Env old (Context.empty (Context.currentName old)) EnvDispatch
    |> runState (runExceptT c)
