module MLogo.Program (run) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import MLogo.Interpretation as Interpretation
import MLogo.Interpretation.State
  ( PointerState
  , ScreenState
  , VisibleState
  )
import MLogo.Lexing as Lexing
import MLogo.Parsing as Parsing
import Parsing as P
import StringParser as SP

run
  ∷ String → String \/ VisibleState
run source = do
  tokens ← case Lexing.run source of
    Left parseError →
      Left $ show source
        <> "\n\nLexing error: "
        <> SP.printParserError parseError
    Right tokens →
      Right tokens

  statements ← case Parsing.run tokens of
    Left parseError →
      Left $ show tokens
        <> "\n\nParsing error: "
        <> P.parseErrorMessage parseError
    Right statements →
      Right statements

  { pointer, screen } ← case Interpretation.run statements of
    Left interpretationError →
      Left $ show statements
        <> "\n\nInterpretation error: "
        <> interpretationError
    Right state →
      Right state

  pure { pointer, screen }
