module Juvix.Core.Common.NameSpace where

import Juvix.Library hiding (modify)
import qualified Juvix.Library.HashMap as HashMap

-- TODO :: Put protected here
data T b
  = T
    { public :: HashMap.T Symbol b
    , private :: HashMap.T Symbol b
    }
  deriving (Show)

empty :: T b
empty = T {public = HashMap.empty, private = HashMap.empty}

lookup :: Symbol -> T v -> Maybe v
lookup s T {public, private} =
  HashMap.lookup s public <|> HashMap.lookup s private

insPublic :: Symbol -> v -> T v -> T v
insPublic sym val t =
  t {public = HashMap.insert sym val (public t)}

insPrivate :: Symbol -> v -> T v -> T v
insPrivate sym val t =
  t {private = HashMap.insert sym val (private t)}
