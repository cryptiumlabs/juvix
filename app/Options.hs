{-# LANGUAGE DeriveDataTypeable #-}

module Options
  ( Context (..),
    Options (..),
    Backend (..),
    Command (..),
    options,
  )
where

import Data.Curve.Weierstrass.BLS12381 (Fr)
import qualified Juvix.Backends.Michelson as Michelson
import qualified Juvix.Backends.Plonk as Plonk
import Juvix.Library hiding (option)
import Options.Applicative

data Context = Context
  { contextWorkingDirectory :: FilePath,
    contextHomeDirectory :: FilePath
  }

data Options = Options
  { optionsCommand :: Command,
    optionsConfigPath :: FilePath
  }

data Backend
  = Plonk (Plonk.BPlonk Fr)
  | Michelson Michelson.BMichelson
  deriving (Eq, Show)

data Command
  = Version
  | Config
  | Interactive
  | Parse FilePath Backend
  | Typecheck FilePath Backend
  | Compile FilePath FilePath Backend
  | Init
  | Plan
  | Apply

options :: Context -> Parser Options
options ctx = Options <$> commandOptions <*> configOptions ctx

configOptions :: Context -> Parser FilePath
configOptions ctx =
  strOption
    ( short 'c'
        <> long "config"
        <> metavar "PATH"
        <> value (contextWorkingDirectory ctx <> "/juvix.yaml")
        <> showDefault
        <> help "Path to YAML configuration file"
    )

commandOptions :: Parser Command
commandOptions =
  subparser
    ( command "version" (info versionOptions (progDesc "Display version information"))
        <> command
          "config"
          ( info
              configurationOptions
              (progDesc "Adjust runtime configuration or generate an example config file")
          )
        <> command "parse" (info parseOptions (progDesc "Parse a Juvix source file"))
        <> command "typecheck" (info typecheckOptions (progDesc "Typecheck a Juvix source file"))
        <> command "compile" (info compileOptions (progDesc "Compile a Juvix source file"))
    )

versionOptions :: Parser Command
versionOptions = pure Version

configurationOptions :: Parser Command
configurationOptions = pure Config

parseOptions :: Parser Command
parseOptions = Parse <$> inputFileOptions <*> backendOptions

typecheckOptions :: Parser Command
typecheckOptions = Typecheck <$> inputFileOptions <*> backendOptions

compileOptions :: Parser Command
compileOptions = Compile <$> inputFileOptions <*> outputFileOptions <*> backendOptions

inputFileOptions :: Parser FilePath
inputFileOptions = argument str (metavar "INPUTFILE")

outputFileOptions :: Parser FilePath
outputFileOptions = argument str (metavar "OUTPUTFILE")

backendOptions :: Parser Backend
backendOptions =
  option
    ( maybeReader
        ( \case
            "plonk" -> pure $ Plonk Plonk.BPlonk
            "michelson" -> pure $ Michelson Michelson.BMichelson
            _ -> Nothing
        )
    )
    (long "backend" <> short 'b' <> metavar "BACKEND" <> help "Target backend" <> showDefault)
