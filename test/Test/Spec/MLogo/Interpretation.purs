module Test.Spec.MLogo.Interpretation (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import MLogo.Interpretation as Interpretation
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState, Value(..))
import MLogo.Interpretation.State as State
import MLogo.Parsing (Expression(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

spec ∷ Spec Unit
spec = describe "Interpretation" do
  describe "Addition" do
    expressionTestCase
      "two numeric literals"
      State.initialExecutionState
      (Addition (FloatLiteral 1.0) (FloatLiteral 2.0))
      ( Right
          $ (Just $ FloatValue $ 1.0 + 2.0) /\
              State.initialExecutionState
      )

expressionTestCase
  ∷ String
  → ExecutionState
  → Expression
  → String \/ (Maybe Value /\ ExecutionState)
  → Spec Unit
expressionTestCase title state expression expected = it title
  do
    let
      actual = Interpret.runInterpret
        Interpretation.interpretExpression
        state
        expression

    if actual == expected then pure unit
    else
      fail $
        "--- error >>> ---\n"
          <> show actual
          <> "\nis not equal to\n"
          <> show expected
          <> "\n--- expression >>> ---\n"
          <> show expression
          <> "\n--- <<< expression ---"
          <> "\n--- <<< error ---"

