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
import Test.QuickCheck (arbitrary, (===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "Power" do
  describe "interpret" do
    generativeTestCase "raises numbers to exponents" do
      executionState ← arbitrary
      let
        actual = Interpret.runInterpret
          Power.interpret
          executionState
          { base: 2.0, exponent: 3.0 }
        expected = Right $ (Just $ FloatValue 8.0) /\ executionState

      pure $ actual === expected

