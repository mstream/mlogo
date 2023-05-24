module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Difference
  ( spec
  ) where

import Prelude

import Control.Monad.Gen as Gen
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Arithmetic.Difference as Difference
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck (Result(..), (<?), (===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "Difference" do
  describe "interpret" do
    generativeTestCase Short "subtracts numbers" do
      executionState ← StateGen.genExecutionState
      let
        actual = Interpret.runInterpret
          Difference.interpret
          executionState
          { minuend: 3.0, subtrahend: 2.0 }
        expected = Right $ (Just $ FloatValue 1.0) /\ executionState

      pure $ actual === expected

    generativeTestCase
      Long
      "produces negative result when the subtrahend is larger that the minuend"
      do
        executionState ← StateGen.genExecutionState
        minuend ← Gen.chooseFloat (-100.0) 100.0
        difference ← Gen.chooseFloat 1.0 100.0
        let
          actual = Interpret.runInterpret
            Difference.interpret
            executionState
            { minuend, subtrahend: minuend + difference }

        pure case actual of
          Left errorMessage →
            Failed $ "the command has failed: " <> errorMessage
          Right (Just (FloatValue x) /\ _) →
            x <? 0.0
          Right (Just _ /\ _) →
            Failed "the command has not outputted a float"
          Right (Nothing /\ _) →
            Failed "the command has not outputted any value"

