module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Power
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Arithmetic.Power as Power
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState(..), Value(..))
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "Power" do
  describe "interpret" do
    it "raises numbers to exponents" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            Power.interpret
            (wrap state)
            { base: 2.0, exponent: 3.0 }
          expected = Right $ (Just $ FloatValue 8.0) /\ (wrap state)
        in
          actual === expected

