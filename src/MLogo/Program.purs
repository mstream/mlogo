module MLogo.Program (run) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List as List
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation as Interpretation
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (VisibleState)
import MLogo.Interpretation.State as State
import MLogo.Parsing as Parsing
import Parsing as P

run ∷ String → String \/ VisibleState
run source = do
  expressions ← case P.runParser source Parsing.expressions of
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

