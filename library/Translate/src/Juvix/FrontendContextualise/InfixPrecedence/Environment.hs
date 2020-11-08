{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Juvix.FrontendContextualise.InfixPrecedence.Environment
  ( module Juvix.FrontendContextualise.InfixPrecedence.Environment,
    module Juvix.FrontendContextualise.Environment,
  )
where

import Data.Kind (Constraint)
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as Local
import Juvix.FrontendContextualise.Environment
import qualified Juvix.FrontendContextualise.InfixPrecedence.ShuntYard as Shunt
import qualified Juvix.FrontendContextualise.InfixPrecedence.Types as New
import qualified Juvix.FrontendContextualise.ModuleOpen.Types as Old
import qualified Juvix.Library
import Juvix.Library hiding (ask)

--------------------------------------------------------------------------------
-- Type Aliases and effect setup
--------------------------------------------------------------------------------

type Old f =
  f (NonEmpty (Old.FunctionLike Old.Expression)) Old.Signature Old.Type

type New f =
  f (NonEmpty (New.FunctionLike New.Expression)) New.Signature New.Type

type TransitionMap m =
  (HasState "old" (Old Context.T) m, HasState "new" (New Context.T) m)

type Queryable tag m =
  (HasReader "dispatch" tag m, QueryConstraint tag m, Query tag)

-- The effect of expression and below note that new is not the new map
-- per se, but more instead of where local functions get added...  for
-- a full pass this is indeed the new context, but for a single pass,
-- just a local cache
type Expression tag m =
  (Queryable tag m, HasState "new" (New Context.T) m, HasThrow "error" Error m)

type MinimalEnv a b c m =
  (HasState "env" (Context.T a b c) m)

-- the effect of the pass itself
type WorkingMaps tag m =
  (TransitionMap m, Expression tag m, HasThrow "error" Error m)

-- the effect of the single function down
type SingleMap a b c m =
  (MinimalEnv a b c m, Expression (SingleDispatch a b c) m)

type FinalContext = New Context.T

--------------------------------------------------------------------------------
-- Environment data declarations
--------------------------------------------------------------------------------

-- | the traditional transition between one context and another
data EnvDispatch = EnvDispatch

-- | Used when going trying to transition one definition to the next level
data SingleDispatch a b c = SingleDispatch

data Environment
  = Env
      { old :: Old Context.T,
        new :: New Context.T,
        dispatch :: EnvDispatch
      }
  deriving (Generic)

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

--------------------------------------------------------------------------------
-- Generic Interface Definitions for Environment lookup
--------------------------------------------------------------------------------

class Query a where
  type QueryConstraint a (m :: * -> *) :: Constraint
  queryInfo' ::
    (SymbLookup sym, QueryConstraint a m) => sym -> a -> m (Maybe [Context.Information])

instance Query (SingleDispatch a b c) where
  type QueryConstraint _ m = MinimalEnv a b c m
  queryInfo' sym SingleDispatch = do
    undefined

instance Query EnvDispatch where
  type QueryConstraint _ m = TransitionMap m
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
  (Queryable a m, SymbLookup sym) => sym -> m (Maybe [Context.Information])
queryInfo s = Juvix.Library.ask @"dispatch" >>= queryInfo' s

runEnv :: Context a -> Old Context.T -> (Either Error a, Environment)
runEnv (Ctx c) old =
  Env old (Context.empty (Context.currentName old)) EnvDispatch
    |> runState (runExceptT c)
