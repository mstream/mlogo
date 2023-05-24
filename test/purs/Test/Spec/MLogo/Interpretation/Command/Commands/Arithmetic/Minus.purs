module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Minus
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Arithmetic.Minus as Minus
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "Minus" do
  describe "interpret" do
    generativeTestCase Short "negates numbers" do
      executionState ← StateGen.genExecutionState

      let
        actual = Interpret.runInterpret
          Minus.interpret
          executionState
          1.0
        expected = Right $ (Just $ FloatValue (-1.0)) /\ executionState

      pure $ actual === expected

