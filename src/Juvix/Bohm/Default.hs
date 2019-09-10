-- | Gives the default execution environment for netToAst
-- Can be added to via core translation
module Juvix.Bohm.Default where

import           Juvix.Bohm.Shared
import           Juvix.Library            hiding (empty, link)
import qualified Juvix.Bohm.Type          as BT

import qualified Data.Map.Strict          as Map

plus :: Primitive → Primitive → Maybe Primitive
plus (PInt i1) (PInt i2) = Just (PInt $ i1 + i2)
plus (PBool _) _         = Nothing
plus _         (PBool _) = Nothing

defaultEnv :: Map SomeSymbol BT.Fn
defaultEnv = Map.fromList [(someSymbolVal "plus", BT.Arg2 plus)]
