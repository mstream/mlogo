module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.YCor
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.YCor as YCor
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "YCor" do
  describe "interpret" do
    generativeTestCase Short "outputs pointer's y coordinate" do
      state ← StateGen.genExecutionState

      let
        actual = Interpret.runInterpret YCor.interpret state unit
        expected = Right
          $ (Just $ FloatValue $ state.pointer.position.y) /\ state

      pure $ actual === expected

