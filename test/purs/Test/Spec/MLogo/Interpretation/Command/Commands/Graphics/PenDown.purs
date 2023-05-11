module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.PenDown
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.PenDown as PenDown
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState(..))
import Test.QuickCheck (arbitrary, (===))
import Test.Spec (Spec, describe, it)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "PenDown" do
  describe "interpret" do
    generativeTestCase "puts the pen down" do
      (ExecutionState state) ← arbitrary
      let
        actual = Interpret.runInterpret
          PenDown.interpret
          (wrap state)
          unit
        expected = Right $ Nothing /\
          ( ExecutionState $ state
              { pointer = state.pointer { isDown = true } }
          )
      pure $ actual === expected

