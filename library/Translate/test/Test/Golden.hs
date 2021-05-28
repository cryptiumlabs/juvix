module Test.Golden where

import qualified Data.ByteString as ByteString (readFile, writeFile)
import Data.ByteString.Char8 (pack)
import qualified Data.Text as Text
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Frontend.Sexp (transTopLevel)
import Juvix.Frontend.Types (TopLevel, extractTopLevel)
import Juvix.Frontend.Types.Base (Header (NoHeader))
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.Silver as T
import qualified Test.Tasty.Silver.Advanced as T
import Text.Pretty.Simple (pShowNoColor)
import           Data.String                (String)
import           System.FilePath            (replaceExtension)
import qualified System.IO.Strict
import           Test.Tasty
-- import        qualified   Test.Tasty.Golden as T
-- import       qualified    Test.Tasty.Golden.Advanced as T
--------------------------------------------------------------------------------
-- Contracts as a file (Golden tests)
--------------------------------------------------------------------------------
contractFiles :: T.TestTree
contractFiles =
  T.testGroup
    "Contract Files"
    [ T.testGroup
        "Contract Files Tests - Golden"
        [ idString,
          addition,
          token
        ]
    ]

resultToText :: Show a => a -> Text
resultToText = Text.pack . show

toByteString :: Show a => a -> ByteString
toByteString = Data.ByteString.Char8.pack . show

parseContractFile :: FilePath -> IO (Either [Char] (Header TopLevel))
parseContractFile file = do
  Parser.prettyParse  <$> ByteString.readFile file

parsedContract :: FilePath -> IO (Header TopLevel)
parsedContract file = do
  rawContract <- ByteString.readFile file
  case Parser.prettyParse rawContract of
    Left err -> writeFile (file <> ".parsed") (toS err) *> pure (NoHeader [])
    Right x -> do
      -- generate/update the golden file as the parsed file
      writeFile (file <> ".golden") (show x)
      -- human readable version of the golden file for debugging
      writeFile
        (file <> ".HRGolden")
        ( toStrict
            ( pShowNoColor $
                map transTopLevel (extractTopLevel x)
            )
        )
      pure x

getGolden :: FilePath -> IO (Maybe (Header TopLevel))
getGolden file = do
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

goldenTest :: T.TestName -> FilePath -> T.TestTree
goldenTest name file =
  let goldenFileName = file <> ".golden"
   in T.goldenTest1
        name
        (getGolden goldenFileName)
        (parsedContract file)
        compareParsedGolden
        -- show the golden/actual value, not working atm
        ( T.ShowText . Text.pack
            . const "this isn't doing anything?" -- (Prelude.unlines . map show))
            -- update the golden file, not working atm
        )
        ( ByteString.writeFile goldenFileName
            . const "this isn't either" -- ((encodeUtf8 . Text.pack) . ppShowList))
        )

idString :: T.TestTree
idString = goldenTest "Id-String" "../../test/examples/Id-Strings.ju"

addition :: T.TestTree
addition = goldenTest "Addition" "../../test/examples/Addition.ju"

token :: T.TestTree
token = goldenTest "Token" "../../test/examples/Token.ju"


type FileExtension = String

parseTestPositive :: FilePath -> IO String
parseTestPositive file = either
  notImplemented
  notImplemented
  <$> parseContractFile file

parseTests :: IO TestTree
parseTests = testGroup "parse" <$> sequence
    [ discoverGoldenTestsParse "../../test/examples" parseTestPositive
    -- , discoverGoldenTestsJuvix "test/negative" parseTestNegative
    ]
-- | Discover golden tests for input files with extension @.ju@ and output
-- files with extension @.parsed@.
discoverGoldenTestsParse
  :: FilePath                 -- ^ the directory in which to recursively look for golden tests
  -> (FilePath -> IO String)  -- ^ the IO action to run on the input file which produces the test output
  -> IO TestTree
discoverGoldenTestsParse = discoverGoldenTests [".ju"] ".parsed" (\fp s -> writeFile fp (toS s))

-- | Discover golden tests.
discoverGoldenTests
  :: [FileExtension]                -- ^ the input file extensions
  -> FileExtension                  -- ^ the output file extension
  -> (FilePath -> String -> IO ())  -- ^ the IO action to run when creating (or updating, in the case of @--accept@) the golden file
  -> FilePath                       -- ^ the directory in which to recursively look for golden tests
  -> (FilePath -> IO String)        -- ^ the IO action to run on the input file which produces the test output
  -> IO TestTree
discoverGoldenTests exts_in ext_out createGolden path mkOutput = pure
    . testGroup path
    . map (mkGoldenTest mkOutput ext_out createGolden)
    =<< T.findByExtension exts_in path

-- | Make a single golden test.
mkGoldenTest
  :: (FilePath -> IO String)        -- ^ the action to test
  -> FileExtension                  -- ^ the extension of the outfile, e.g. @".out"@
  -> (FilePath -> String -> IO ())  -- ^ the function which updates the output file (gets the path to it and the test result)
  -> FilePath                       -- ^ the file path of the input file
  -> TestTree
mkGoldenTest mkOutput ext createGolden file = T.goldenTest
    file
    (System.IO.Strict.readFile outfile)
    (mkOutput file)
    chkDiffIO
    (createGolden outfile)
  where
    outfile = replaceExtension file ext

    chkDiffIO :: String -> String -> IO (Maybe String)
    chkDiffIO lhs rhs = return $ case checkDifference lhs rhs of
      Nothing   -> Nothing
      Just diff -> Just $ toS $ unlines
        [ "The expected output (<) and actual output (>) differ:"
        , toS diff
        ]

    -- | Compares two strings, returns @Nothing@ if they are the same,
    -- returns @Just@ the diff if they are different.
    checkDifference :: String -> String -> Maybe String
    checkDifference exp act = if exp == act
      then Nothing
      else Just "Different"