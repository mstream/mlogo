module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.YCor
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.YCor as YCor
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (Position(..), Value(..))
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "YCor" do
  describe "interpret" do
    generativeTestCase "outputs pointer's y coordinate" do
      state ← StateGen.genExecutionState

      let
        actual = Interpret.runInterpret YCor.interpret state unit
        (Position { y }) = state.pointer.position
        expected = Right $ (Just $ FloatValue y) /\ state

      pure $ actual === expected

