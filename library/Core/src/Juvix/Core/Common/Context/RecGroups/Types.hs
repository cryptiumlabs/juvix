{-# LANGUAGE OverloadedLists #-}

module Juvix.Core.Common.Context.RecGroups.Types
  ( -- * Output types
    Entry (..),
    Group,
    Group',
    Groups,
    Groups',
    Prefix,

    -- * Capabilities
    Env,
    PrefixReader,
    OutputWriter,
    CurGroupState,
    CurGroupWriter,
    run,
    run_,

    -- * Operations
    addDef,
    newGroup,
    withPrefix,
    qualify,
    applyPrefix,
  )
where

import qualified Data.DList as D
import Juvix.Core.Common.Context.Types
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

-- | A definition identified by its fully-qualified name.
data Entry term ty sumRep
  = Entry
      { name :: NameSymbol.T,
        def :: Definition term ty sumRep
      }
  deriving (Eq, Show, Generic, Data)

-- | A recursive group of definitions.
type Group term ty sumRep = NonEmpty (Entry term ty sumRep)

type Group' term ty sumRep = D.DList (Entry term ty sumRep)

-- | All recursive groups in a context, in arbitrary order.
type Groups term ty sumRep = [Group term ty sumRep]

type Groups' term ty sumRep = D.DList (Group term ty sumRep)

-- | Module name prefix
newtype Prefix = P (D.DList Symbol)

data S term ty sumRep
  = S
      { prefix :: Prefix,
        output :: Groups' term ty sumRep,
        curGroup :: Group' term ty sumRep
      }
  deriving (Generic)

type Alias term ty sumRep = State (S term ty sumRep)

newtype Env term ty sumRep a = Env {unEnv :: Alias term ty sumRep a}
  deriving newtype (Functor, Applicative, Monad)
  deriving
    ( HasSource "prefix" Prefix,
      HasReader "prefix" Prefix
    )
    via ReaderField "prefix" (Alias term ty sumRep)
  deriving
    ( HasSink "output" (Groups' term ty sumRep),
      HasWriter "output" (Groups' term ty sumRep)
    )
    via WriterField "output" (Alias term ty sumRep)
  deriving
    ( HasSource "curGroup" (Group' term ty sumRep),
      HasSink "curGroup" (Group' term ty sumRep),
      HasState "curGroup" (Group' term ty sumRep)
    )
    via StateField "curGroup" (Alias term ty sumRep)
  deriving
    (HasWriter "curGroup" (Group' term ty sumRep))
    via WriterField "curGroup" (Alias term ty sumRep)

type PrefixReader = HasReader "prefix" Prefix

type OutputWriter term ty sumRep =
  HasWriter "output" (Groups' term ty sumRep)

type CurGroupState term ty sumRep =
  HasState "curGroup" (Group' term ty sumRep)

type CurGroupWriter term ty sumRep =
  HasWriter "curGroup" (Group' term ty sumRep)

run_ :: Env term ty sumRep a -> Groups term ty sumRep
run_ = snd . run

run :: Env term ty sumRep a -> (a, Groups term ty sumRep)
run act =
  runState (unEnv $ act <* newGroup) initState
    |> second (toList . output)
  where
    initState = S {prefix = P [], output = [], curGroup = []}

-- | Add a definition to the current recursive group.
addDef ::
  (PrefixReader m, CurGroupWriter term ty sumRep m) =>
  Symbol ->
  Definition term ty sumRep ->
  m ()
addDef name def = do
  qname <- qualify name
  tell @"curGroup" [Entry qname def]

-- | Finalise the current group and begin a new empty one.
newGroup ::
  (OutputWriter term ty sumRep m, CurGroupState term ty sumRep m) => m ()
newGroup = do
  addGroup =<< get @"curGroup"
  put @"curGroup" mempty

-- | Add a group to the final output.
addGroup :: OutputWriter term ty sumRep m => Group' term ty sumRep -> m ()
addGroup grp =
  case nonEmpty $ toList grp of
    Just grp -> tell @"output" [grp]
    Nothing -> pure ()

-- | Extend the current module prefix.
--
-- >>> 'fst' $ 'run' $ 'withPrefix' \"A\" $ 'qualify' \"X\"
-- A.X
-- >>> 'fst' $ 'run' $ 'withPrefix' \"A\" $ 'withPrefix' \"B\" $ 'qualify' \"X\"
-- A.B.X
withPrefix :: PrefixReader m => Symbol -> m a -> m a
withPrefix n = local @"prefix" \(P pfx) -> P $ D.snoc pfx n

-- | Qualify a name by the current module prefix.
qualify :: PrefixReader m => Symbol -> m NameSymbol.T
qualify n = asks @"prefix" \pfx -> applyPrefix pfx n

-- | Apply a prefix to a name.
applyPrefix :: Prefix -> Symbol -> NameSymbol.T
applyPrefix (P pfx) = NameSymbol.qualify1 pfx
