module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.HideTurtle
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.HideTurtle as HideTurtle
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "HideTurtle" do
  describe "interpret" do
    generativeTestCase "lift the pen up" do
      state ← StateGen.genExecutionState

      let
        actual = Interpret.runInterpret HideTurtle.interpret state unit

        expected = Right $ Nothing /\ state
          { pointer = state.pointer { isVisible = false } }

      pure $ actual === expected

