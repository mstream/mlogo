module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.SetY
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.SetY as SetY
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck (arbitrary, (===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "SetY" do
  describe "interpret" do
    generativeTestCase
      Short
      "sets pointer's y coordinate - with pen up"
      do
        state ← StateGen.genExecutionState
        y ← arbitrary
        let
          actual = Interpret.runInterpret
            SetY.interpret
            ( state
                { pointer = state.pointer { isDown = false } }
            )
            y
          expected = Right $ Nothing /\ state
            { pointer = state.pointer
                { isDown = false
                , position = state.pointer.position { y = y }
                }
            }

        pure $ actual === expected

    generativeTestCase
      Short
      "sets pointer's y coordinate - with pen down"
      do
        state ← StateGen.genExecutionState
        y ← arbitrary
        let
          actual = Interpret.runInterpret
            SetY.interpret
            (state { pointer = state.pointer { isDown = true } })
            y
          expected = Right $ Nothing /\ state
            { pointer = state.pointer
                { isDown = true
                , position = state.pointer.position { y = y }
                }
            , screen =
                { color: state.pointer.color
                , p1: state.pointer.position
                , p2: state.pointer.position { y = y }
                } :
                  state.screen
            }

        pure $ actual === expected

