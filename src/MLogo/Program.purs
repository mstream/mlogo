module MLogo.Program (run) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation as Interpretation
import MLogo.Interpretation.Command.Commands as Commands
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (VisibleState)
import MLogo.Interpretation.State as State
import MLogo.Parsing (Expression, ParsingContext)
import MLogo.Parsing as Parsing
import Parsing (ParseError)
import Parsing as P

run ∷ String → String \/ VisibleState
run source = do
  expressions ← case parseExpressions source of
    Left parseError →
      Left $ "Syntax error:\n" <> show parseError
    Right expressions →
      Right expressions

  let
    result = Interpret.runInterpret
      Interpretation.interpretExpressions
      State.initialExecutionState
      expressions

  { callStack, pointer, screen } ← unwrap <$> case result of
    Left interpretationError →
      Left $ "\n\nRuntime error: "
        <> interpretationError
    Right (_ /\ state) →
      Right state

  if List.null callStack then
    Right { pointer, screen }
  else Left "call stack not cleared"

parseExpressions ∷ String → ParseError \/ List Expression
parseExpressions source = do
  procedureSignatures ← P.runParser source Parsing.procedureSignatures

  let
    parsingContext ∷ ParsingContext
    parsingContext = Map.union
      (Parsing.procedureSignaturesToParsingContext procedureSignatures)
      Commands.parsingContext

  P.runParser source (Parsing.expressions parsingContext)

