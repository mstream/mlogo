module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.SetPenColor
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.SetPenColor as SetPenColor
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "SetPenColor" do
  describe "interpret" do
    generativeTestCase "sets pointer's color - existing color" do
      state ← StateGen.genExecutionState
      color ← StateGen.genColor
      let
        actual = Interpret.runInterpret
          SetPenColor.interpret
          (state { colorPalette = Map.singleton 0 color })
          0
        expected = Right $ Nothing /\ state
          { colorPalette = Map.singleton 0 color
          , pointer = state.pointer { color = color }
          }

      pure $ actual === expected

    generativeTestCase
      "sets pointer's x coordinate - non-existing color"
      do
        state ← StateGen.genExecutionState
        let
          actual = Interpret.runInterpret
            SetPenColor.interpret
            (state { colorPalette = Map.empty })
            0
          expected = Left
            "selected color does not exist in the color palette"
        pure $ actual === expected

