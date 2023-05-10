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
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "PenUp" do
  describe "interpret" do
    it "lift the pen up" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            PenUp.interpret
            (wrap state)
            unit
          expected = Right $ Nothing /\
            ( ExecutionState $ state
                { pointer = state.pointer { isDown = false } }
            )
        in
          actual === expected

