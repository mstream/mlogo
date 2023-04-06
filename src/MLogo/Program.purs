module MLogo.Program (run) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import MLogo.Interpretation as Interpretation
import MLogo.Interpretation.State (PointerState, ScreenState)
import MLogo.Lexing as Lexing
import MLogo.Parsing as Parsing
import Parsing as P
import StringParser as SP

run
  ∷ String → String \/ { pointer ∷ PointerState, screen ∷ ScreenState }
run source = do
  tokens ← case Lexing.run source of
    Left parseError →
      Left $ "Lexing error: " <> SP.printParserError parseError
    Right tokens →
      Right tokens

  statements ← case Parsing.run tokens of
    Left parseError →
      Left $ "Parsing error: " <> P.parseErrorMessage parseError
    Right statements →
      Right statements

  { pointer, screen } ← case Interpretation.run statements of
    Left interpretationError →
      Left $ "Interpretation error: " <> interpretationError
    Right state →
      Right state

  pure { pointer, screen }
