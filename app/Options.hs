module Options where

import           Options.Applicative
import           Protolude

data Context = Context {
  contextWorkingDirectory ∷ FilePath,
  contextHomeDirectory    ∷ FilePath
}

data Options = Options {
  optionsCommand    ∷ Command,
  optionsConfigPath ∷ FilePath
}

data Command
  = Version
  | Config
  | Interactive
  | Solve

options ∷ Context → Parser Options
options ctx = Options <$> commandOptions <*> configOptions ctx

configOptions ∷ Context → Parser FilePath
configOptions ctx = strOption (short 'c' <> long "config" <> metavar "PATH" <> value (contextWorkingDirectory ctx <> "/juvix.yaml") <> showDefault <> help "Path to YAML configuration file")

commandOptions ∷ Parser Command
commandOptions = subparser (
      command "version" (info versionOptions (progDesc "Display version information"))
  <>  command "config" (info configurationOptions (progDesc "Adjust runtime configuration or generate an example config file"))
  <>  command "interactive" (info interactiveOptions (progDesc "Launch interactive mode"))
  <>  command "solve" (info solveOptions (progDesc "Solve some equations"))
  )

versionOptions ∷ Parser Command
versionOptions = pure Version

configurationOptions ∷ Parser Command
configurationOptions = pure Config

interactiveOptions ∷ Parser Command
interactiveOptions = pure Interactive

solveOptions ∷ Parser Command
solveOptions = pure Solve
