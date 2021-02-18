{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Juvix.Library.LineNum where

import Control.Lens
import Juvix.Library

data T = T {tLine :: Int, tCol :: Int} deriving (Show, Eq, Ord)

makeLensesWith camelCaseFields ''T
