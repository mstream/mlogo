module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.SetXY
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.SetXY as SetXY
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck (arbitrary, (===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "SetXY" do
  describe "interpret" do
    generativeTestCase
      Short
      "sets pointer's x and y coordinates - with pen up"
      do
        state ← StateGen.genExecutionState
        targetPosition ← arbitrary
        let
          actual = Interpret.runInterpret
            SetXY.interpret
            ( state
                { pointer = state.pointer { isDown = false } }
            )
            targetPosition

          expected = Right $ Nothing /\ state
            { pointer = state.pointer
                { isDown = false, position = targetPosition }
            }

        pure $ actual === expected

    generativeTestCase
      Short
      "sets pointer's x and y coordinates - with pen down"
      do
        state ← StateGen.genExecutionState
        targetPosition ← arbitrary
        let
          actual = Interpret.runInterpret
            SetXY.interpret
            (state { pointer = state.pointer { isDown = true } })
            targetPosition
          expected = Right $ Nothing /\ state
            { pointer = state.pointer
                { isDown = true, position = targetPosition }
            , screen =
                { color: state.pointer.color
                , p1: state.pointer.position
                , p2: targetPosition
                }
                  :
                    state.screen
            }

        pure $ actual === expected

