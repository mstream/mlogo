module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.ShowTurtle
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.ShowTurtle as ShowTurtle
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "ShowTurtle" do
  describe "interpret" do
    generativeTestCase Short "makes the pointer visible" do
      state ← StateGen.genExecutionState

      let
        actual = Interpret.runInterpret ShowTurtle.interpret state unit

        expected = Right $ Nothing /\ state
          { pointer = state.pointer { isVisible = true } }

      pure $ actual === expected

