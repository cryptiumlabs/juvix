module Juvix.Frontend where

import qualified Control.Arrow as Arrow
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Juvix.Core.Common.NameSymbol as NameSymbol
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Types as Types
import Juvix.Library
import qualified System.FilePath as FilePath
import Prelude (String)

ofPath :: [FilePath] -> IO (Either String [(NameSymbol.T, [Types.TopLevel])])
ofPath files = do
  read <- ByteString.readFile `traverse` files
  case traverse Parser.parseOnly read of
    Left x ->
      pure (Left x)
    Right xs ->
      zip files xs
        |> fmap
          ( Arrow.first
              (NameSymbol.fromSymbol . intern . toUpper . FilePath.takeBaseName)
          )
        |> Right
        |> pure

toUpper :: String -> String
toUpper (x : xs) = Char.toUpper x : xs
toUpper [] = []
