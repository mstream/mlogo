module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.PenDown
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.PenDown as PenDown
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "PenDown" do
  describe "interpret" do
    generativeTestCase Short "puts the pen down" do
      state ← StateGen.genExecutionState

      let
        actual = Interpret.runInterpret PenDown.interpret state unit

        expected = Right $ Nothing /\ state
          { pointer = state.pointer { isDown = true } }

      pure $ actual === expected

