module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Quotient
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Arithmetic.Quotient as Quotient
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "Quotient" do
  describe "interpret" do
    generativeTestCase Short "divides numbers" do
      executionState ← StateGen.genExecutionState
      let
        actual = Interpret.runInterpret
          Quotient.interpret
          executionState
          { dividend: 6.0, divisor: 2.0 }
        expected = Right $ (Just $ FloatValue 3.0) /\ executionState

      pure $ actual === expected

    generativeTestCase Short "divides numbers - division by zero" do
      executionState ← StateGen.genExecutionState
      let
        actual = Interpret.runInterpret
          Quotient.interpret
          executionState
          { dividend: 6.0, divisor: zero }
        expected = Left "division by zero"

      pure $ actual === expected

