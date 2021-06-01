{-# LANGUAGE ViewPatterns #-}
module Test.Golden where

import qualified Data.ByteString as ByteString (readFile, writeFile)
import Data.ByteString.Char8 (pack)
import qualified Data.Text as Text
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Frontend.Types (TopLevel)
import Juvix.Frontend.Types.Base (Header)
import Juvix.Library
import qualified Test.Tasty.Silver as T
import qualified Test.Tasty.Silver.Advanced as T
import Text.Pretty.Simple (pShowNoColor)
import           Data.String                (String)
import qualified System.FilePath as FP
import           Test.Tasty
import System.Directory (createDirectoryIfMissing)
--------------------------------------------------------------------------------
-- Contracts as a file (Golden tests)
--------------------------------------------------------------------------------


resultToText :: Show a => a -> Text
resultToText = Text.pack . show

toByteString :: Show a => a -> ByteString
toByteString = Data.ByteString.Char8.pack . show

parseContractFile :: FilePath -> IO (Either [Char] (Header TopLevel))
parseContractFile file = do
  Parser.prettyParse  <$> ByteString.readFile file

parsedContract :: FilePath -> IO (Either [Char] (Header TopLevel))
parsedContract file = do
  rawContract <- ByteString.readFile file
  pure $ Parser.prettyParse rawContract 

getGolden :: FilePath -> IO (Maybe (Either [Char] (Header TopLevel)))
getGolden file = do
  createDirectoryIfMissing True $ FP.takeDirectory file
  maybeBS <- T.readFileMaybe file
  return $ do
    bs <- maybeBS
    readMaybe $ Text.unpack $ decodeUtf8 bs

compareParsedGolden :: (Eq a, Show a) => a -> a -> T.GDiff
compareParsedGolden golden parsed
  | parsed == golden =
    T.Equal
  | otherwise =
    T.DiffText
      { T.gReason =
          Just $
            "Parsed output doesn't match golden file."
              <> "The parsed result is \n"
              <> show parsed
              <> "\n but the expected result is \n"
              <> show golden,
        T.gActual = resultToText parsed,
        T.gExpected = resultToText golden
      }


type FileExtension = String

parseTests :: IO TestTree
parseTests = testGroup "parse" <$> sequence
    [ discoverGoldenTestsParse "../../test/examples/positive" compareParsedGolden
    -- , discoverGoldenTestsJuvix "test/examples/negative" parseTestNegative
    ]
-- | Discover golden tests for input files with extension @.ju@ and output
-- files with extension @.parsed@.
discoverGoldenTestsParse
  :: FilePath                 -- ^ the directory in which to recursively look for golden tests
  -> (forall a. (Eq a, Show a) => a -> a -> T.GDiff)  -- ^ the IO action to run on the input file which produces the test output
  -> IO TestTree
discoverGoldenTestsParse = discoverGoldenTests [".ju"] ".parsed" 

-- | Discover golden tests.
discoverGoldenTests
  :: [FileExtension]                -- ^ the input file extensions
  -> FileExtension                  -- ^ the output file extension
  -> FilePath                       -- ^ the directory in which to recursively look for golden tests
  -> (forall a. (Eq a, Show a) => a -> a -> T.GDiff)        -- ^ the IO action to run on the input file which produces the test output
  -> IO TestTree
discoverGoldenTests exts_in ext_out path compare =
    testGroup path
    . map (mkGoldenTest compare ext_out)
    <$> T.findByExtension exts_in path

toGolden :: (ConvertText a Text, ConvertText Text c) => a -> c
toGolden = toS . Text.replace "examples" "examples-golden" .toS 

-- | Make a single golden test.
mkGoldenTest
  :: (forall a. (Eq a, Show a) => a -> a -> T.GDiff)        -- ^ comparison function
  -> FileExtension                  -- ^ the extension of the outfile, e.g. @".parsed"@
  -> FilePath                       -- ^ the file path of the input file
  -> TestTree
mkGoldenTest compare ext pathToFile = T.goldenTest1
    outFilename
    (getGolden outfile)
    (parsedContract pathToFile)
    compare
    -- show the golden/actual value
    ( T.ShowText . toS . pShowNoColor )
    createOutput
  where
    directory = FP.dropFileName pathToFile
    goldenBase = FP.takeBaseName pathToFile 
    outFilename = FP.replaceExtension (FP.takeFileName pathToFile) ext
    outfile = toGolden directory FP.</> goldenBase FP.</> outFilename 
    createOutput = ByteString.writeFile outfile
        . (encodeUtf8 . toS . pShowNoColor)
