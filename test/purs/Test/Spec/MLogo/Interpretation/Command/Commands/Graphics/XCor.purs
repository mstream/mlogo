module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.XCor
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.XCor as XCor
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "XCor" do
  describe "interpret" do
    generativeTestCase Short "outputs pointer's x coordinate" do
      state ← StateGen.genExecutionState

      let
        actual = Interpret.runInterpret XCor.interpret state unit
        expected = Right
          $ (Just $ FloatValue state.pointer.position.x) /\ state

      pure $ actual === expected

