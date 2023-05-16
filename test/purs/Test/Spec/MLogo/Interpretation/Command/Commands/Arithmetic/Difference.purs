module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Difference
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Arithmetic.Difference as Difference
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "Difference" do
  describe "interpret" do
    generativeTestCase "subtracts numbers" do
      executionState ← StateGen.genExecutionState
      let
        actual = Interpret.runInterpret
          Difference.interpret
          executionState
          { minuend: 3.0, subtrahend: 2.0 }
        expected = Right $ (Just $ FloatValue 1.0) /\ executionState

      pure $ actual === expected

