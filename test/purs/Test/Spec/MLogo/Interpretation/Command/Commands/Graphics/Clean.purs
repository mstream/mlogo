module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.Clean
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.Clean as Clean
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "Clean" do
  describe "interpret" do
    generativeTestCase "makes drawings disappear" do
      state ← StateGen.genExecutionState

      let
        actual = Interpret.runInterpret Clean.interpret state unit
        expected = Right $ Nothing /\ state { screen = Nil }

      pure $ actual === expected

