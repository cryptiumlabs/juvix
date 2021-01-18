-- |
-- - Serves as the context for lower level programs of the =Juvix=
--   Programming Language
-- - This is parameterized per phase which may store the type and
--   term in slightly different ways
module Juvix.Core.Common.Context
  ( module Juvix.Core.Common.Context.Types,
    module Juvix.Core.Common.Context.Precedence,
    -- leave the entire module for now, so lenses can be exported
    module Juvix.Core.Common.Context,
  )
where

import Juvix.Core.Common.Context.Precedence
import Juvix.Core.Common.Context.Types
import qualified Juvix.Core.Common.NameSpace as NameSpace
import Juvix.Library hiding (modify)
import qualified Juvix.Library as Lib
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import Prelude (error)

--------------------------------------------------------------------------------
-- In lieu of not being able to export namespaces
--------------------------------------------------------------------------------
type NameSymbol = NameSymbol.T

nameSymbolToSymbol :: NameSymbol.T -> Symbol
nameSymbolToSymbol = NameSymbol.toSymbol

nameSymbolFromSymbol :: Symbol -> NameSymbol.T
nameSymbolFromSymbol = NameSymbol.fromSymbol

--------------------------------------------------------------------------------
-- Body
--------------------------------------------------------------------------------

empty :: NameSymbol.T -> T term ty sumRep
empty sym =
  case addPathWithValue (pure topLevelName <> sym') CurrentNameSpace fullyEmpty of
    Lib.Left _ -> error "impossible"
    Lib.Right x -> x
  where
    fullyEmpty =
      ( T
          { currentNameSpace = NameSpace.empty,
            currentName = sym',
            topLevelMap = HashMap.empty
          }
      )
    sym' = removeTopName sym

qualifyName :: NameSymbol.T -> T term ty sumRep -> NameSymbol.T
qualifyName sym T {currentName} = currentName <> sym

--------------------------------------------------------------------------------
-- Functions on the Current NameSpace
--------------------------------------------------------------------------------

lookupCurrent ::
  NameSymbol.T -> T term ty sumRep -> Maybe (NameSpace.From (Definition term ty sumRep))
lookupCurrent =
  lookupGen (\_ currentLookup -> currentLookup)

-- TODO ∷ Maybe change
-- By default add adds it to the public map by default!
add ::
  NameSpace.From Symbol ->
  Definition term ty sumRep ->
  T term ty sumRep ->
  T term ty sumRep
add sy term t =
  t {currentNameSpace = NameSpace.insert sy term (currentNameSpace t)}

remove ::
  NameSpace.From Symbol -> T term ty sumRep -> T term ty sumRep
remove sy t = t {currentNameSpace = NameSpace.remove sy (currentNameSpace t)}

publicNames :: T term ty sumRep -> [Symbol]
publicNames T {currentNameSpace} =
  let NameSpace.List {publicL} = NameSpace.toList currentNameSpace
   in fst <$> publicL

toList :: T term ty sumRep -> NameSpace.List (Definition term ty sumRep)
toList T {currentNameSpace} = NameSpace.toList currentNameSpace

topList :: T term ty sumRep -> [(Symbol, Definition term ty sumRep)]
topList T {topLevelMap} = HashMap.toList topLevelMap

--------------------------------------------------------------------------------
-- Global Functions
--------------------------------------------------------------------------------

-- we lose some type information here... we should probably reserve it somehow

inNameSpace ::
  NameSymbol.T -> T term ty sumRep -> Maybe (T term ty sumRep)
inNameSpace newNameSpace t@T {currentName}
  | removeTopName newNameSpace == removeTopName currentName =
    pure t
  | otherwise = do
    mdef <- t !? newNameSpace
    let (def, fullName) = resolveName t (mdef, newNameSpace)
    case def of
      Record cont _ ->
        Just (changeCurrentModuleWith t cont fullName)
      _ ->
        Nothing

-- | switchNameSpace works like a mixture of defpackage and in-package from CL
-- creating a namespace if not currently there, and switching to it
-- this function may fail if a path is given like `Foo.Bar.x.f` where x
-- is a non record.
-- This function also keeps the invariant that there is only one CurrentNameSpace
-- tag
switchNameSpace ::
  NameSymbol.T -> T term ty sumRep -> Either PathError (T term ty sumRep)
switchNameSpace newNameSpace t@T {currentName}
  | removeTopName newNameSpace == removeTopName currentName =
    Lib.Right t
  | otherwise =
    let newT =
          case addPathWithValue newNameSpace (Record NameSpace.empty Nothing) t of
            Lib.Right t -> t
            Lib.Left {} -> t
     in case inNameSpace newNameSpace newT of
          Nothing -> Lib.Left (VariableShared newNameSpace)
          Just ct -> Lib.Right ct

lookup ::
  NameSymbol.T -> T term ty sumRep -> Maybe (From (Definition term ty sumRep))
lookup key t@T {topLevelMap} =
  let f x currentLookup =
        fmap Current currentLookup <|> fmap Outside (HashMap.lookup x topLevelMap)
   in lookupGen f key t

(!?) ::
  T term ty sumRep -> NameSymbol.T -> Maybe (From (Definition term ty sumRep))
(!?) = flip lookup

modifyGlobal ::
  NameSymbol.T ->
  (Maybe (Definition term ty sumRep) -> Definition term ty sumRep) ->
  T term ty sumRep ->
  T term ty sumRep
modifyGlobal sym g t =
  case modifySpace f sym t of
    Just tt -> tt
    Nothing -> t
  where
    f (Final x) =
      UpdateNow (g x)
    f (Continue (Just (Record def ty))) =
      GoOn (OnRecord def ty)
    f (Continue _) =
      Abort

addGlobal ::
  NameSymbol.T ->
  Definition term ty sumRep ->
  T term ty sumRep ->
  T term ty sumRep
addGlobal sym def t =
  case modifySpace f sym t of
    Just tt -> tt
    Nothing -> t
  where
    f (Final _) =
      UpdateNow def
    f (Continue (Just (Record def ty))) =
      GoOn (OnRecord def ty)
    f (Continue _) =
      Abort

addPathWithValue ::
  NameSymbol.T ->
  Definition term ty sumRep ->
  T term ty sumRep ->
  Either PathError (T term ty sumRep)
addPathWithValue sym def t =
  case modifySpace f sym t of
    Just tt -> Lib.Right tt
    Nothing -> Lib.Left (VariableShared sym)
  where
    f (Final Nothing) = UpdateNow def
    f (Final (Just _)) = Abort
    f (Continue Nothing) =
      GoOn (OnRecord NameSpace.empty Nothing)
    f (Continue (Just (Record def ty))) =
      GoOn (OnRecord def ty)
    f (Continue _) =
      Abort

removeNameSpace :: NameSymbol -> T term ty sumRep -> T term ty sumRep
removeNameSpace sym t =
  case modifySpace f sym t of
    Just tt -> tt
    Nothing -> t
  where
    f (Final (Just _)) =
      RemoveNow
    f (Final Nothing) =
      Abort
    f (Continue (Just (Record def ty))) =
      GoOn (OnRecord def ty)
    f (Continue Nothing) =
      Abort
    f (Continue (Just _)) =
      Abort

removeTop :: Symbol -> T term ty sumRep -> T term ty sumRep
removeTop sym t@T {topLevelMap} =
  t {topLevelMap = HashMap.delete sym topLevelMap}

------------------------------------------------------------
-- Helper for switch name space
------------------------------------------------------------

changeCurrentModuleWith ::
  T ter ty sr -> NameSpace.T (Definition ter ty sr) -> NameSymbol.T -> T ter ty sr
changeCurrentModuleWith t startingContents newCurrName =
  let queued = queueCurrentModuleBackIn t
   in t
        { currentName = removeTopName newCurrName,
          currentNameSpace = startingContents
        }
        |> queued
        |> addGlobal newCurrName CurrentNameSpace

queueCurrentModuleBackIn ::
  T term ty sumRep -> T term ty sumRep -> T term ty sumRep
queueCurrentModuleBackIn T {currentNameSpace, currentName} =
  -- we have to add top to it, or else if it's a single symbol, then
  -- it'll be added to itself, which is bad!
  addGlobal
    (addTopNameToSngle currentName)
    (Record currentNameSpace Nothing)

putCurrentModuleBackIn :: T term ty sumRep -> T term ty sumRep
putCurrentModuleBackIn t = queueCurrentModuleBackIn t t

addTopNameToSngle :: IsString a => NonEmpty a -> NonEmpty a
addTopNameToSngle (x :| []) = topLevelName :| [x]
addTopNameToSngle xs = xs

removeTopName :: (Eq a, IsString a) => NonEmpty a -> NonEmpty a
removeTopName (top :| x : xs)
  | topLevelName == top = x :| xs
removeTopName (top :| [])
  | top == topLevelName = "" :| []
removeTopName xs = xs

-------------------------------------------------------------------------------
-- Functions on From
-------------------------------------------------------------------------------

extractValue :: From a -> a
extractValue (Outside a) = a
extractValue (Current c) = NameSpace.extractValue c

-------------------------------------------------------------------------------
-- Functions on Information
-------------------------------------------------------------------------------
precedenceOf :: Foldable t => t Information -> Maybe Precedence
precedenceOf = fmap (\(Prec p) -> p) . find f
  where
    f (Prec _) = True

-------------------------------------------------------------------------------
-- Generalized Helpers
-------------------------------------------------------------------------------

----------------------------------------
-- Types for Generalized Helpers
----------------------------------------

data Stage b
  = -- | 'Final' signifies the last symbol that we
    -- pursue in updating a structure
    Final b
  | -- | 'Continue' signifies that there are parts of
    -- the namespace that we can still continue down
    Continue b

data Return b ty
  = -- | 'GoOn' signifies that we should continue
    -- going down records
    GoOn (OnRecord b ty)
  | -- | 'Abort' signifies that we should cancel
    -- the changes on the map and
    Abort
  | -- | 'UpdateNow' signifies that we should
    -- update the context with the current value
    UpdateNow b
  | -- | 'RemoveNow' signifies that we show remove
    -- the definition at this level
    RemoveNow

data OnRecord b ty
  = OnRecord (NameSpace.T b) (Maybe ty)

----------------------------------------
-- Type Class for Genralized Helpers
----------------------------------------

-- Type class for removing repeat code
class MapSym m where
  lookup' :: Symbol -> m a -> Maybe a
  remove' :: Symbol -> m a -> m a
  insert' :: Symbol -> a -> m a -> m a

instance MapSym (HashMap.T Symbol) where
  lookup' = HashMap.lookup
  remove' = HashMap.delete
  insert' = HashMap.insert

instance MapSym NameSpace.T where
  lookup' = NameSpace.lookup
  remove' = NameSpace.removePublic
  insert' x = NameSpace.insert (NameSpace.Pub x)

newtype PrivNameSpace v = Priv {unPriv :: NameSpace.T v}

instance MapSym PrivNameSpace where
  lookup' sym = NameSpace.lookup sym . unPriv
  remove' sym = Priv . NameSpace.removePrivate sym . unPriv
  insert' sym def = Priv . NameSpace.insert (NameSpace.Priv sym) def . unPriv

modifySpace ::
  ( Stage (Maybe (Definition term ty sumRep)) ->
    Return (Definition term ty sumRep) ty
  ) ->
  NameSymbol.T ->
  T term ty sumRep ->
  Maybe (T term ty sumRep)
modifySpace f (s :| ymbol) t@T {currentNameSpace, currentName, topLevelMap} =
  case NameSpace.lookupInternal s currentNameSpace of
    Just (NameSpace.Pub _) ->
      updateCurr t <$> recurse f (s :| ymbol) currentNameSpace
    Just (NameSpace.Priv _) ->
      updateCurr t . unPriv <$> recurse f (s :| ymbol) (Priv currentNameSpace)
    Nothing ->
      case NameSymbol.takePrefixOf currentName (removeTopName (s :| ymbol)) of
        Just subPath ->
          updateCurr t <$> recurse f subPath currentNameSpace
        Nothing ->
          -- we do one more match as we have to make sure
          -- if we are adding foo we add it to the right place
          case ymbol of
            [] ->
              updateCurr t <$> recurse f (s :| ymbol) currentNameSpace
            (newS : newYmbol) ->
              if  | s == topLevelName ->
                    updateTopLevel t <$> recurse f (newS :| newYmbol) topLevelMap
                  | otherwise ->
                    updateTopLevel t <$> recurse f (s :| ymbol) topLevelMap
  where
    updateCurr t newCurrent =
      t {currentNameSpace = newCurrent}
    updateTopLevel t newTop =
      t {topLevelMap = newTop}

recurse ::
  MapSym m =>
  ( Stage (Maybe (Definition term ty sumRep)) ->
    Return (Definition term ty sumRep) ty
  ) ->
  (NonEmpty Symbol) ->
  m (Definition term ty sumRep) ->
  Maybe (m (Definition term ty sumRep))
recurse f (x :| y : xs) cont =
  case f (Continue (lookup' x cont)) of
    GoOn (OnRecord nameSpace ty) ->
      let g newRecord =
            insert' x (Record newRecord ty) cont
       in fmap g (recurse f (y :| xs) nameSpace)
    Abort ->
      Nothing
    RemoveNow ->
      Just (remove' x cont)
    UpdateNow newRecord ->
      Just (insert' x newRecord cont)
recurse f (x :| []) cont =
  case f (Final (lookup' x cont)) of
    UpdateNow return ->
      Just (insert' x return cont)
    RemoveNow ->
      Just (remove' x cont)
    -- GoOn makes no sense here, so act as an Abort
    GoOn {} -> Nothing
    Abort -> Nothing

-- couldn't figure out how to fold lenses
-- once we figure out how to do a fold like
-- foldr (\x y -> x . contents . T  . y) identity brokenKey
-- replace the recursive function with that

-- TODO ∷ add something like
-- checkGlobal
--   | NameSymbol.subsetOf currentName nameSymb

-- eventually to check if we are referencing an inner module via the top
-- This will break code where you've added local

lookupGen ::
  Traversable t =>
  ( Symbol ->
    Maybe (NameSpace.From (Definition term ty sumRep)) ->
    Maybe (t (Definition term ty sumRep))
  ) ->
  NameSymbol.T ->
  Cont (Definition term ty sumRep) ->
  Maybe (t (Definition term ty sumRep))
lookupGen extraLookup nameSymb T {currentNameSpace} =
  let recurse _ Nothing =
        Nothing
      recurse [] x =
        x
      recurse (x : xs) (Just (Record namespace _)) =
        recurse xs (NameSpace.lookup x namespace)
      -- This can only happen when we hit from the global
      -- a precondition is that the current module
      -- will never have a currentNamespace inside
      recurse (x : xs) (Just CurrentNameSpace) =
        recurse xs (NameSpace.lookup x currentNameSpace)
      recurse (_ : _) _ =
        Nothing
      first (x :| xs) =
        NameSpace.lookupInternal x currentNameSpace
          |> second (x :| xs)
      second (x :| xs) looked =
        extraLookup x looked
          |> \case
            Just x -> traverse (recurse xs . Just) x
            Nothing -> Nothing
   in case nameSymb of
        -- we skip the local lookup if we get a top level
        top :| x : xs
          | topLevelName == top -> second (x :| xs) Nothing
        top :| []
          | topLevelName == top -> Nothing
        x :| xs -> first (x :| xs)

-- TODO/ISSUE ∷ what if we resolve a private module
-- the path and lookup doesn't make sense much
-- we need to change namesymbol to something along
-- the lines of every symbol saying if it's public
-- or private
resolveName ::
  T a b c ->
  (From (Definition a b c), NameSymbol.T) ->
  (Definition a b c, NameSymbol.T)
resolveName ctx (def, name) =
  case def of
    -- TODO ∷ update this case
    Current (NameSpace.Priv x) ->
      (x, fullyQualified)
    Current (NameSpace.Pub x) ->
      (x, fullyQualified)
    Outside x ->
      (x, nameAlreadyFully)
  where
    nameAlreadyFully =
      pure topLevelName <> removeTopName name
    fullyQualified =
      pure topLevelName <> currentName ctx <> name

-- | qualifyLookup fully qualiifes a name in the current context.
qualifyLookup :: NameSymbol.T -> T a b c -> Maybe NameSymbol.T
qualifyLookup name ctx =
  case lookup name ctx of
    Nothing -> Nothing
    Just (Outside _) -> Just (NameSymbol.cons topLevelName (removeTopName name))
    Just (Current _) -> Just (pure topLevelName <> currentName ctx <> name)
