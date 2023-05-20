module Main.CLI (main) where

import Prelude

import Data.Argonaut.Core (stringify) as A
import Data.Argonaut.Encode (encodeJson) as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Random (randomInt)
import MLogo.Program as Program
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Options.Applicative
  ( Parser
  , ParserInfo
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , progDesc
  , short
  , strOption
  , (<**>)
  )

data Config = Config { filePath ∷ String }

derive instance Generic Config _

instance Show Config where
  show = genericShow

configParser ∷ Parser Config
configParser = map Config $ ({ filePath: _ })
  <$> strOption
    ( long "file"
        <> short 'f'
        <> metavar "FILE"
        <> help "Path to a file containing a source code."
    )

main ∷ Effect Unit
main = run =<< execParser opts

opts ∷ ParserInfo Config
opts = info (configParser <**> helper)
  ( fullDesc
      <> progDesc "Print a greeting for TARGET"
      <> header "mlogo"
  )

run ∷ Config → Effect Unit
run (Config { filePath }) = do
  source ← FS.readTextFile UTF8 filePath
  randomNumberSeed ← randomInt 0 top
  case Program.run randomNumberSeed source of
    Left errorMessage → do
      Console.error errorMessage
    Right state →
      Console.info $ A.stringify $ A.encodeJson state

