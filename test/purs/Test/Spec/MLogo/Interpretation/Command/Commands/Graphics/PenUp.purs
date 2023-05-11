module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.PenUp
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.PenUp as PenUp
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState(..))
import Test.QuickCheck (arbitrary, (===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "PenUp" do
  describe "interpret" do
    generativeTestCase "lift the pen up" do
      (ExecutionState state) ← arbitrary
      let
        actual = Interpret.runInterpret
          PenUp.interpret
          (wrap state)
          unit
        expected = Right $ Nothing /\
          ( wrap state
              { pointer = state.pointer { isDown = false } }
          )

      pure $ actual === expected

