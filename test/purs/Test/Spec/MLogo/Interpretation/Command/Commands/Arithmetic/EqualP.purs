module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.EqualP
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Arithmetic.EqualP as EqualP
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (Value(..))
import Test.QuickCheck (arbitrary, (===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "EqualP" do
  describe "interpret" do

    generativeTestCase "checks values equality - zero arguments" do
      executionState ← arbitrary
      let
        actual = Interpret.runInterpret
          EqualP.interpret
          executionState
          Nil
        expected = Right $ (Just $ BooleanValue true) /\ executionState

      pure $ actual === expected

    generativeTestCase "checks values equality - one argument" do
      executionState ← arbitrary
      let
        actual = Interpret.runInterpret
          EqualP.interpret
          executionState
          (List.fromFoldable [ FloatValue 1.0 ])
        expected = Right $ (Just $ BooleanValue true) /\ executionState

      pure $ actual === expected

    generativeTestCase "checks values equality - two same arguments" do
      executionState ← arbitrary
      let
        actual = Interpret.runInterpret
          EqualP.interpret
          executionState
          (List.fromFoldable [ FloatValue 1.0, FloatValue 1.0 ])
        expected = Right $ (Just $ BooleanValue true) /\ executionState

      pure $ actual === expected

    generativeTestCase
      "checks values equality - two different arguments"
      do
        executionState ← arbitrary
        let
          actual = Interpret.runInterpret
            EqualP.interpret
            executionState
            (List.fromFoldable [ FloatValue 1.0, FloatValue 2.0 ])
          expected = Right $ (Just $ BooleanValue false) /\
            executionState

        pure $ actual === expected

