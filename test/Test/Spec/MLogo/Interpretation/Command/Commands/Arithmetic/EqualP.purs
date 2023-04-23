module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.EqualP
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Arithmetic.EqualP as EqualP
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState(..), Value(..))
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "EqualP" do
  describe "interpret" do
    it "checks values equality - zero arguments" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            EqualP.interpret
            (wrap state)
            Nil
          expected = Right $ (Just $ BooleanValue true) /\ (wrap state)
        in
          actual === expected

    it "checks values equality - one argument" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            EqualP.interpret
            (wrap state)
            (List.fromFoldable [ FloatValue 1.0 ])
          expected = Right $ (Just $ BooleanValue true) /\ (wrap state)
        in
          actual === expected

    it "checks values equality - two same arguments" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            EqualP.interpret
            (wrap state)
            (List.fromFoldable [ FloatValue 1.0, FloatValue 1.0 ])
          expected = Right $ (Just $ BooleanValue true) /\ (wrap state)
        in
          actual === expected

    it "checks values equality - two different arguments" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            EqualP.interpret
            (wrap state)
            (List.fromFoldable [ FloatValue 1.0, FloatValue 2.0 ])
          expected = Right $ (Just $ BooleanValue false) /\ (wrap state)
        in
          actual === expected

