module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Quotient
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Arithmetic.Quotient as Quotient
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState(..), Value(..))
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "Quotient" do
  describe "interpret" do
    it "divides numbers" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            Quotient.interpret
            (wrap state)
            { dividend: 6.0, divisor: 2.0 }
          expected = Right $ (Just $ FloatValue 3.0) /\ (wrap state)
        in
          actual === expected

    it "divides numbers - division by zero" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            Quotient.interpret
            (wrap state)
            { dividend: 6.0, divisor: zero }
          expected = Left "division by zero"
        in
          actual === expected

