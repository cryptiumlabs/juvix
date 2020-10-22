module Options where

import Options.Applicative
import Juvix.Library hiding (option)

data Context
  = Context
      { contextWorkingDirectory :: FilePath,
        contextHomeDirectory :: FilePath
      }

data Options
  = Options
      { optionsCommand :: Command,
        optionsConfigPath :: FilePath
      }

data Backend
  = Unit
  | Naturals
  | Michelson
  deriving (Eq, Show)

data Command
  = Version
  | Config
  | Interactive
  | Parse FilePath
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
        --        <> command "interactive" (info interactiveOptions (progDesc "Launch interactive mode"))
        --        <> command "init" (info initOptions (progDesc "Initialise deployment configuration"))
        --        <> command "plan" (info planOptions (progDesc "Plan deployment"))
        --        <> command "apply" (info applyOptions (progDesc "Execute deployment"))
        <> command "parse" (info parseOptions (progDesc "Parse a Juvix source file"))
        <> command "typecheck" (info typecheckOptions (progDesc "Typecheck a Juvix source file"))
        <> command "compile" (info compileOptions (progDesc "Compile a Juvix source file"))
    )

versionOptions :: Parser Command
versionOptions = pure Version

configurationOptions :: Parser Command
configurationOptions = pure Config

interactiveOptions :: Parser Command
interactiveOptions = pure Interactive

initOptions :: Parser Command
initOptions = pure Init

planOptions :: Parser Command
planOptions = pure Plan

applyOptions :: Parser Command
applyOptions = pure Apply

parseOptions :: Parser Command
parseOptions = Parse <$> inputFileOptions

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
            "unit" -> pure Unit
            "naturals" -> pure Naturals
            "michelson" -> pure Michelson
            _ -> Nothing
        )
    )
    (long "backend" <> short 'b' <> metavar "BACKEND" <> help "Target backend" <> value Michelson <> showDefault)
