module Juvix.FrontendContextualise.Environment where

import Control.Lens hiding ((|>))
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.Core.Common.Open as Open
import Juvix.Library
import Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp

type HasNew t ty s m = HasState "new" (Context.T t ty s) m

type HasOld t ty s m = HasState "old" (Context.T t ty s) m

-- to avoid casting all the time

class SymbLookup a where
  look ::
    a ->
    Context.T term ty sumRep ->
    Maybe (Context.From (Context.Definition term ty sumRep))

  lookCurr ::
    a ->
    Context.T term ty sumRep ->
    Maybe (NameSpace.From (Context.Definition term ty sumRep))

instance SymbLookup Symbol where
  look sym cont = Context.lookup (NameSymbol.fromSymbol sym) cont

  --
  lookCurr sym cont = Context.lookupCurrent (NameSymbol.fromSymbol sym) cont

instance SymbLookup NameSymbol.T where
  look sym cont = Context.lookup sym cont

  lookCurr sym cont = Context.lookupCurrent sym cont

lookup ::
  (HasNew term ty sumRep m, SymbLookup sym) =>
  sym ->
  m (Maybe (Context.From (Context.Definition term ty sumRep)))
lookup sy = do
  get @"new" >>| look sy

lookupCurrent ::
  (HasNew term ty sumRep m, SymbLookup sym) =>
  sym ->
  m (Maybe (NameSpace.From (Context.Definition term ty sumRep)))
lookupCurrent sy = do
  get @"new" >>| lookCurr sy

ask ::
  (HasOld term ty sumRep m, SymbLookup sym) =>
  sym ->
  m (Maybe (Context.From (Context.Definition term ty sumRep)))
ask sy = do
  get @"old" >>| look sy

add ::
  HasNew term ty sumRep m =>
  NameSpace.From Symbol ->
  Context.Definition term ty sumRep ->
  m ()
add sy def = Juvix.Library.modify @"new" (Context.add sy def)

addGlobal ::
  HasNew term ty sumRep m =>
  NameSymbol.T ->
  Context.Definition term ty sumRep ->
  m ()
addGlobal sy def = Juvix.Library.modify @"new" (Context.addGlobal sy def)

remove ::
  HasNew term ty sumRep m => NameSpace.From Symbol -> m ()
remove sy = Juvix.Library.modify @"new" (Context.remove sy)

removeGlobal ::
  HasNew term ty sumRep m => NameSymbol.T -> m ()
removeGlobal sy = Juvix.Library.modify @"new" (Context.removeNameSpace sy)

removeOld ::
  HasOld term ty sumRep m => NameSpace.From Symbol -> m ()
removeOld sy = Juvix.Library.modify @"old" (Context.remove sy)

addUnknown ::
  HasNew term ty sumRep m => NameSpace.From Symbol -> m ()
addUnknown sym =
  Juvix.Library.modify @"new"
    (Context.add sym (Context.Unknown Nothing))

addUnknownGlobal ::
  HasNew term ty sumRep m => Context.From Symbol -> m ()
addUnknownGlobal (Context.Current sym) = addUnknown sym
addUnknownGlobal (Context.Outside sym) =
  Juvix.Library.modify @"new"
    (Context.addGlobal (pure sym) (Context.Unknown Nothing))

------------------------------------------------------------
-- double module setup and dealings
------------------------------------------------------------

setupNewModule ::
  Context.T term1 ty1 sumRep1 -> IO (Context.T term2 ty2 sumRep2)
setupNewModule t = do
  empt <- Context.empty (Context.currentName t)
  empt
    |> set
      (Context._currentNameSpace . Context.qualifiedMap)
      (t ^. Context._currentNameSpace . Context.qualifiedMap)
    |> set Context._reverseLookup (t ^. Context._reverseLookup)
    |> pure

-- | @switchContext@ takes two modules representing the same context
-- and attempts to switch the namespace while keeping the pointers
-- consistent
switchContext ::
  NameSymbol.T ->
  Context.T term ty sumRep ->
  Context.T term2 ty2 sumRep2 ->
  IO
    ( Either
        Context.PathError
        (Context.T term ty sumRep, Context.T term2 ty2 sumRep2)
    )
switchContext sym ctx1 ctx2 = do
  let switched1 = Context.inNameSpace sym ctx1
      switched2 = Context.inNameSpace sym ctx2
  case (switched1, switched2) of
    (Just c1, Just c2) -> pure $ Right (c1, c2)
    (Nothing, Nothing) -> pure $ Left (Context.VariableShared sym)
    (Just c1, Nothing) -> setupFill sym c1 ctx2
    (Nothing, Just c2) -> setupFill sym c2 ctx1 >>| fmap swap

-- | @oneFilled@ takes a namesymbol and filled out parts of a currently
-- added module and creates a new context with that module filled out
oneFilled ::
  NameSymbol.T ->
  ([Open.TName NameSymbol.T], Context.SymbolMap) ->
  Context.T term ty sumRep ->
  IO (Either Context.PathError (Context.T term ty sumRep))
oneFilled sym (openList, qualifiedMap) ctx =
  Context.addPathWithValue
    sym
    (Context.Record (Context.Rec NameSpace.empty Nothing openList qualifiedMap))
    ctx

-- | @setupFilled@ takes two contexts, one that successfully switched
-- modules and another that hasn't and inserts the needed information
-- to keep the pointers consistent
setupFill ::
  NameSymbol.T ->
  Context.T term1 ty1 sumRep1 ->
  Context.T term2 ty2 sumRep2 ->
  IO
    ( Either
        Context.PathError
        (Context.T term1 ty1 sumRep1, Context.T term2 ty2 sumRep2)
    )
setupFill sym cWorks cDoesntWork = do
  let args =
        ( cWorks ^. Context._currentNameSpace . Context.openList,
          cWorks ^. Context._currentNameSpace . Context.qualifiedMap
        )
  newInserted <- oneFilled sym args cDoesntWork
  case newInserted of
    Right cInserted ->
      -- This will always be a just!
      let Just cNowWorks = Context.inNameSpace sym cInserted
       in pure $ Right (cWorks, cNowWorks)
    Left err -> pure $ Left err

----------------------------------------------------------------------
-- Sexp Helpers Code Above this Will likely be deleted
----------------------------------------------------------------------

-- TODO ∷ make this a standard data structure

-- Currently we don't really use the signature however in the future
-- the mSig will be used to detect the types of modules we will have
-- open and any other information we wish to track here!?
data Information
  = Info
      { -- | @mSig@ represents the type of the term in the closure
        mSig :: Maybe Sexp.T,
        -- | @info@ represents all the information we have on the term
        info :: [Context.Information]
      }
  deriving (Show, Eq)

newtype Closure'
  = Closure (Map.T Symbol Information)
  deriving (Show, Eq)

addToClosure :: Symbol -> Information -> Closure' -> Closure'
addToClosure k info (Closure m) =
  Closure $ Map.insert k info m

genericBind :: Symbol -> Closure' -> Closure'
genericBind name (Closure m) =
  Closure $ Map.insert name (Info Nothing []) m

passContext ctx f g h =
  Context.mapWithContext
    ctx
    Context.CtxForm
      { sumF = f,
        termF = g,
        tyF = h
      }
  where
    pass =
      undefined

bindingForms :: (Eq a, IsString a) => a -> Bool
bindingForms x =
  x `elem` ["type", ":open-in", ":let-type", ":let-match", "case", ":lambda-case", "declaim"]

searchAndClosure a as op
  | named "case" = undefined
  where
    named = Sexp.isAtomNamed (Sexp.Atom a)
