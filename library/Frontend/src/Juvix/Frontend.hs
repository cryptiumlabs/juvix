module Juvix.Frontend where

import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Types as Types
import Juvix.Library hiding (toUpper)
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Parser (ParserError)
import qualified System.FilePath as FilePath
import Prelude (String)

-- we abuse laziness here
-- TODO ∷ add directory option
-- this will add top level to the thing, and properly handle paths

-- | Parse multiple files into ML AST
parseFiles :: [FilePath] -> IO (Either ParserError [(NameSymbol.T, [Types.TopLevel])])
parseFiles =
  -- fmap gets through the IO, so that sequenceA flips the either and list
  fmap sequenceA . traverse parseSingleFile

-- | Parse single file into ML AST
parseSingleFile :: FilePath -> IO (Either ParserError (NameSymbol.T, [Types.TopLevel]))
parseSingleFile file = do
  read <- ByteString.readFile file
  case Parser.parse read of
    Left x ->
      pure (Left x)
    Right (Types.Header name xs) ->
      pure (Right (name, xs))
    Right (Types.NoHeader xs) ->
      let toName =
            NameSymbol.fromSymbol . intern . toUpper . FilePath.takeBaseName
       in pure (Right (toName file, xs))

toUpper :: String -> String
toUpper (x : xs) = Char.toUpper x : xs
toUpper [] = []
