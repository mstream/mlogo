module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Power
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Arithmetic.Power as Power
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "Power" do
  describe "interpret" do
    generativeTestCase Short "raises numbers to exponents" do
      executionState ← StateGen.genExecutionState
      let
        actual = Interpret.runInterpret
          Power.interpret
          executionState
          { base: 2.0, exponent: 3.0 }
        expected = Right $ (Just $ FloatValue 8.0) /\ executionState

      pure $ actual === expected

