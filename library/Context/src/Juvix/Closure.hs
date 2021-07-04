{-# LANGUAGE DeriveGeneric #-}

-- | Closure.T serves as the data structure in which we will store
-- temporary lexical bindings as our code encounters binders.
module Juvix.Closure where

import qualified Data.HashSet as Set
import Data.Hashable (Hashable (..), hash)
import qualified Juvix.Context as Context
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp

-- Currently we don't really use the signature however in the future
-- the mSig will be used to detect the types of modules we will have
-- open and any other information we wish to track here!?
data Information = Info
  { -- | @mSig@ represents the type of the term in the closure
    mSig :: Maybe Sexp.T,
    -- | @info@ represents all the information we have on the term
    info :: [Context.Information],
    -- | @mOpen@ represents a place where the term may have come
    -- from
    mOpen :: Maybe NameSymbol.T
  }
  deriving (Show, Eq, Generic)

newtype T
  = T (Map.T Symbol Information)
  deriving (Show, Eq, Generic)

instance Hashable Information where
  hash (Info {mSig, info, mOpen}) = hash (hash mSig, hash info, hash mOpen)

instance Hashable T where
  hash (T m) = hash m

insert :: Symbol -> Information -> T -> T
insert k info (T m) =
  T $ Map.insert k info m

insertGeneric :: Symbol -> T -> T
insertGeneric name (T m) =
  T $ Map.insert name (Info Nothing [] Nothing) m

keys :: T -> Set.HashSet Symbol
keys (T m) = Map.keysSet m

lookup :: Symbol -> T -> Maybe Information
lookup k (T m) = Map.lookup k m

empty :: T
empty = T Map.empty

insertHash :: T -> (Symbol, T)
insertHash info =
  let name = intern . show $ hash info
   in (name, insertGeneric name info)
