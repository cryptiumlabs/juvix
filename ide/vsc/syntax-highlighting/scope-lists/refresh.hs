#!/usr/bin/env stack
-- stack script --resolver lts-15.12 --package aeson --package yaml --package filepath --package directory --package unordered-containers --package text

{-
This file is used to generate a database of scope keys used in
-}

{-# LANGUAGE LambdaCase, OverloadedStrings, GeneralizedNewtypeDeriving, RecordWildCards #-}

import Data.Yaml
import System.Environment
import System.FilePath()
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Foldable (toList)
import Data.Maybe
import Data.List (sort)

newtype Names = Names { names :: S.HashSet T.Text }
    deriving (Monoid, Semigroup)

add :: T.Text -> Names -> Names
add n = Names . S.insert n . names

instance FromJSON Names where
    parseJSON = \case
        Object o -> do
            new <- o .:? "name"
            recursed <- mconcat <$> mapM parseJSON (M.elems o)
            pure $ maybe id add new recursed
        Array vs -> mconcat . toList <$> traverse parseJSON vs
        _ -> pure mempty

data Info = Info { iname :: T.Text, idesc :: Maybe T.Text, iexample :: Maybe T.Text, ihide :: Bool }

instance FromJSON Info where
    parseJSON = withObject "Info must be an object" $ \o ->
        Info <$> o .: "scope" <*> o.:? "description" <*> o .:? "example" <*> o .:? "hide" .!= False

instance ToJSON Info where
    toJSON Info{..} = object $
         "description" .?= idesc $ "example" .?= iexample $ "hide" .?= (if ihide then Just ihide else Nothing) $ ["scope" .= iname]
      where
        _ .?= Nothing = id
        a .?= Just b = (a .= b : )

toInfo :: Names -> [Info]
toInfo = map (\name -> Info name Nothing Nothing False) . sort . toList . names

toNames :: [Info] -> Names
toNames = Names . S.fromList . map iname

mergeInfo :: [Info] -> Names -> [Info]
mergeInfo ext (Names n) = ext <> toInfo (Names $ n `S.difference` names (toNames ext))

toMarkdown :: [Info] -> T.Text
toMarkdown info = T.unlines $
    "| Scope Name | Description | Example |"
    : "|-|-|-|"
    : map (\Info{..} -> "| " <> iname <> " | " <> fromMaybe "" idesc <> " | " <> fromMaybe "" iexample <> " |") ( filter (not . ihide) info)

main =
    getArgs >>= \case
        ["db",target, out] -> refreshDb target out
        ["md", target, out] -> makeMarkdown target out

  where
    refreshDb syntax outFile = do
        fromGrammar <- decodeFileThrow syntax
        old <-
            doesFileExist outFile >>= \case
                True -> Just <$> decodeFileThrow outFile
                _ -> pure Nothing

        encodeFile outFile $ maybe toInfo mergeInfo old fromGrammar
    makeMarkdown db md = decodeFileThrow db >>= T.writeFile md . toMarkdown
