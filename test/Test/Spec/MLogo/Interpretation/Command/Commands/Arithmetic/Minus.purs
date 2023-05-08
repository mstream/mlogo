module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Minus
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Arithmetic.Minus as Minus
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState(..), Value(..))
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "Minus" do
  describe "interpret" do
    it "negates numbers" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            Minus.interpret
            (wrap state)
            1.0
          expected = Right $ (Just $ FloatValue (-1.0)) /\ (wrap state)
        in
          actual === expected

