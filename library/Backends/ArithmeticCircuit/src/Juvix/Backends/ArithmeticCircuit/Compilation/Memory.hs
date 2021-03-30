module Juvix.Backends.ArithmeticCircuit.Compilation.Memory where

import qualified Data.List.NonEmpty as NonEmpty
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol

data Ele a
  = Ele
      { externalNumber :: Maybe Int,
        ele :: a
      }
  deriving (Generic, Show)

data T a
  = T
      { bindings :: Map.Map NameSymbol.T (Ele a),
        free' :: NonEmpty Int,
        size :: Int
      }
  deriving (Generic, Show)

instance Semigroup (T a) where
  t1 <> T bindings _ _ =
    -- hopefully renaming handles this for us and so numberOverriden should be 0
    Map.foldrWithKey
      ( \key val acc ->
          case val of
            Ele (Just _) v ->
              allocExternal key v acc
            Ele Nothing v ->
              alloc key v acc
      )
      t1
      bindings

instance Monoid (T a) where
  mempty = initial

initial :: T a
initial = T mempty (0 :| []) 0

-- alloc
allocExternal :: NameSymbol.T -> a -> T a -> T a
allocExternal symbol value (T bindings free size) =
  let (curr, next) =
        case free of
          x :| [] -> (x, x + 1 :| [])
          x :| y : xs -> (x, y :| xs)
   in case bindings Map.!? symbol of
        Just (Ele Nothing _) ->
          T (Map.insert symbol (Ele (Just curr) value) bindings) next size
        Just (Ele (Just i) _) ->
          T (Map.insert symbol (Ele (Just i) value) bindings) free size
        Nothing ->
          T (Map.insert symbol (Ele (Just curr) value) bindings) next (succ size)

alloc :: NameSymbol.T -> a -> T a -> T a
alloc symbol value (T bindings free size) =
  T (Map.insert symbol (Ele Nothing value) bindings) free size

free :: NameSymbol.T -> T a -> T a
free symbol (T bindings freeNames size) =
  case bindings Map.!? symbol of
    Just (Ele Nothing _) ->
      T (Map.delete symbol bindings) freeNames (pred size)
    Just (Ele (Just i) _) ->
      T (Map.delete symbol bindings) (NonEmpty.cons i freeNames) (pred size)
    Nothing -> T bindings freeNames size

lookup :: NameSymbol.T -> T a -> Maybe (Ele a)
lookup symbol (T bindings _ _) =
  Map.lookup symbol bindings
