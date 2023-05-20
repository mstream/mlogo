module MLogo.Program (run, interpretAst, parseExpressions) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation as Interpretation
import MLogo.Interpretation.Command.Commands as Commands
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (VisibleState)
import MLogo.Interpretation.State as State
import MLogo.Parsing (ParsingContext)
import MLogo.Parsing as Parsing
import MLogo.Parsing.Expression (Expression)
import Parsing (ParseError)
import Parsing as P

run ∷ Int → String → String \/ VisibleState
run randomNumberSeed = parseExpressions >>> case _ of
  Left parseError →
    Left $ "Syntax error:\n" <> show parseError
  Right expressions →
    interpretAst randomNumberSeed expressions

parseExpressions ∷ String → ParseError \/ List Expression
parseExpressions source = do
  procedureSignatures ← P.runParser source Parsing.procedureSignatures

  let
    parsingContext ∷ ParsingContext
    parsingContext = Map.union
      (Parsing.procedureSignaturesToParsingContext procedureSignatures)
      Commands.parsingContext

  P.runParser source (Parsing.expressions parsingContext)

interpretAst ∷ Int → List Expression → String \/ VisibleState
interpretAst randomNumberSeed ast = do
  let
    result = Interpret.runInterpret
      Interpretation.interpretExpressions
      (State.initialExecutionState randomNumberSeed)
      ast

  { callStack, pointer, screen } ← case result of
    Left interpretationError →
      Left interpretationError
    Right (_ /\ state) →
      Right state

  if List.null callStack then
    Right { pointer, screen }
  else Left "call stack not cleared"

