module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Sum
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Arithmetic.Sum as Sum
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState(..), Value(..))
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "Sum" do
  describe "interpret" do
    it "sums up addends - zero arguments" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            Sum.interpret
            (wrap state)
            Nil
          expected = Right $ (Just $ FloatValue 0.0) /\ (wrap state)
        in
          actual === expected

    it "sums up addends - one argument" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            Sum.interpret
            (wrap state)
            (List.fromFoldable [ 1.0 ])
          expected = Right $ (Just $ FloatValue 1.0) /\ (wrap state)
        in
          actual === expected

    it "sums up addends - two arguments" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            Sum.interpret
            (wrap state)
            (List.fromFoldable [ 1.0, 2.0 ])
          expected = Right $ (Just $ FloatValue 3.0) /\ (wrap state)
        in
          actual === expected

