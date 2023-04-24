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
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "PenDown" do
  describe "interpret" do
    it "puts the pen down" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            PenDown.interpret
            (wrap state)
            unit
          expected = Right $ Nothing /\
            ( ExecutionState $ state
                { pointer = state.pointer { isDown = true } }
            )
        in
          actual === expected

