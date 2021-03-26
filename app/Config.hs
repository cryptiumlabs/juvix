module Config (T(..)) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Char (isLower, toLower)
import qualified Data.Yaml as Y
import Juvix.Library

data T
  = T
      { configTezosNodeHost :: Text,
        configTezosNodePort :: Int
      }
  deriving (Generic)

defaultT :: T
defaultT =
  T
    { configTezosNodeHost = "127.0.0.1",
      configTezosNodePort = 8732
    }

loadT :: FilePath -> IO (Maybe T)
loadT path = do
  config <- Y.decodeFileEither path
  return $ case config of
    Right parsed -> pure parsed
    Left _ -> Nothing

instance Y.FromJSON T where
  parseJSON = customParseJSON

jsonOptions :: A.Options
jsonOptions =
  A.defaultOptions
    { A.fieldLabelModifier = (\l -> case head l of
        Just h -> toLower h : drop 1 l
        Nothing -> []
        ) . dropWhile isLower,
      A.omitNothingFields = True,
      A.sumEncoding = A.ObjectWithSingleField
    }

customParseJSON :: (A.GFromJSON A.Zero (Rep a), Generic a) => A.Value -> A.Parser a
customParseJSON = A.genericParseJSON jsonOptions
