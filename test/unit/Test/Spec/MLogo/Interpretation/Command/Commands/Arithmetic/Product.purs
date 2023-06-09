module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Product
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Arithmetic.Product as Product
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "Product" do
  describe "interpret" do
    generativeTestCase Short "multiplies factors - zero arguments" do
      executionState ← StateGen.genExecutionState
      let
        actual = Interpret.runInterpret
          Product.interpret
          executionState
          Nil
        expected = Right $ (Just $ FloatValue 1.0) /\ executionState

      pure $ actual === expected

    generativeTestCase Short "multiplies factors - one argument" do
      executionState ← StateGen.genExecutionState
      let
        actual = Interpret.runInterpret
          Product.interpret
          executionState
          (List.fromFoldable [ 2.0 ])
        expected = Right $ (Just $ FloatValue 2.0) /\ executionState
      pure $ actual === expected

    generativeTestCase Short "multiplies factors - two arguments" do
      executionState ← StateGen.genExecutionState
      let
        actual = Interpret.runInterpret
          Product.interpret
          executionState
          (List.fromFoldable [ 2.0, 3.0 ])
        expected = Right $ (Just $ FloatValue 6.0) /\ executionState
      pure $ actual === expected

