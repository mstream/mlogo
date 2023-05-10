module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.SetXY
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.SetXY as SetXY
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState(..))
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "SetXY" do
  describe "interpret" do
    it
      "sets pointer's x and y coordinates - with pen up"
      do
        quickCheck \(ExecutionState state) targetPosition →
          let
            actual = Interpret.runInterpret
              SetXY.interpret
              ( wrap state
                  { pointer = state.pointer { isDown = false } }
              )
              targetPosition
            expected = Right $ Nothing /\
              ( ExecutionState $ state
                  { pointer = state.pointer
                      { isDown = false, position = targetPosition }
                  }
              )
          in
            actual === expected

    it
      "sets pointer's x and y coordinates - with pen down"
      do
        quickCheck \(ExecutionState state) targetPosition →
          let
            actual = Interpret.runInterpret
              SetXY.interpret
              (wrap state { pointer = state.pointer { isDown = true } })
              targetPosition
            expected = Right $ Nothing /\
              ( ExecutionState $ state
                  { pointer = state.pointer
                      { isDown = true, position = targetPosition }
                  , screen =
                      { p1: state.pointer.position, p2: targetPosition }
                        :
                          state.screen
                  }
              )
          in
            actual === expected

