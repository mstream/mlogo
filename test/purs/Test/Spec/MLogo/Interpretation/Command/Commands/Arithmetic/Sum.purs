module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Sum
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Arithmetic.Sum as Sum
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "Sum" do
  describe "interpret" do
    generativeTestCase "sums up addends - zero arguments" do
      executionState ← StateGen.genExecutionState
      let
        actual = Interpret.runInterpret
          Sum.interpret
          executionState
          Nil
        expected = Right $ (Just $ FloatValue 0.0) /\ executionState

      pure $ actual === expected

    generativeTestCase "sums up addends - one argument" do
      executionState ← StateGen.genExecutionState
      let
        actual = Interpret.runInterpret
          Sum.interpret
          executionState
          (List.fromFoldable [ 1.0 ])
        expected = Right $ (Just $ FloatValue 1.0) /\ executionState
      pure $ actual === expected

    generativeTestCase "sums up addends - two arguments" do
      executionState ← StateGen.genExecutionState
      let
        actual = Interpret.runInterpret
          Sum.interpret
          executionState
          (List.fromFoldable [ 1.0, 2.0 ])
        expected = Right $ (Just $ FloatValue 3.0) /\ executionState
      pure $ actual === expected

