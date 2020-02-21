module Config where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Char (isLower, toLower)
import qualified Data.Yaml as Y
import Juvix.Library

data Config
  = Config
      { configTezosNodeHost ∷ Text,
        configTezosNodePort ∷ Int
      }
  deriving (Generic)

defaultConfig ∷ Config
defaultConfig = Config
  { configTezosNodeHost = "127.0.0.1",
    configTezosNodePort = 8732
  }

loadConfig ∷ FilePath → IO (Maybe Config)
loadConfig path = do
  config ← Y.decodeFileEither path
  return $ case config of
    Right parsed → pure parsed
    Left _ → Nothing

instance Y.FromJSON Config where
  parseJSON = customParseJSON

jsonOptions ∷ A.Options
jsonOptions =
  A.defaultOptions
    { A.fieldLabelModifier = (\(h : t) → toLower h : t) . dropWhile isLower,
      A.omitNothingFields = True,
      A.sumEncoding = A.ObjectWithSingleField
    }

customParseJSON ∷ (A.GFromJSON A.Zero (Rep a), Generic a) ⇒ A.Value → A.Parser a
customParseJSON = A.genericParseJSON jsonOptions
