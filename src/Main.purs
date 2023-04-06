module Main where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console as Console
import MLogo.Interpretation (ExecutionState, PointerState, ScreenState)
import MLogo.Interpretation as Interpretation
import MLogo.Lexing as Lexing
import MLogo.Parsing as Parsing
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, header, help, helper, info, long, metavar, progDesc, short, strOption, (<**>))
import Parsing as P
import StringParser as SP

data Config = Config { filePath :: String }

derive instance Generic Config _

instance Show Config where
  show = genericShow

configParser :: Parser Config
configParser = map Config $ ({ filePath: _ })
  <$> strOption
    ( long "file"
        <> short 'f'
        <> metavar "FILE"
        <> help "Path to a file containing a source code."
    )

main :: Effect Unit
main = run =<< execParser opts

opts :: ParserInfo Config
opts = info (configParser <**> helper)
  ( fullDesc
      <> progDesc "Print a greeting for TARGET"
      <> header "mlogo"
  )

run :: Config -> Effect Unit
run (Config { filePath }) = do
  source <- FS.readTextFile UTF8 filePath
  case runProgram source of
    Left errorMessage -> do
      Console.error errorMessage
    Right state ->
      Console.info $ show state

runProgram :: String -> String \/ { pointer :: PointerState, screen :: ScreenState }
runProgram source = do
  tokens <- case Lexing.run source of
    Left parseError ->
      Left $ "Lexing error: " <> SP.printParserError parseError
    Right tokens ->
      Right tokens

  statements <- case Parsing.run tokens of
    Left parseError ->
      Left $ "Parsing error: " <> P.parseErrorMessage parseError
    Right statements ->
      Right statements

  { pointer, screen } <- case Interpretation.run statements of
    Left interpretationError ->
      Left $ "Interpretation error: " <> interpretationError
    Right state ->
      Right state

  pure { pointer, screen }
