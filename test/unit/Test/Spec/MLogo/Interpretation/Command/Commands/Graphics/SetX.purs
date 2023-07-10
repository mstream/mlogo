module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.SetX
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.SetX as SetX
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck (arbitrary, (===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "SetX" do
  describe "interpret" do
    generativeTestCase
      Short
      "sets pointer's x coordinate - with pen up"
      do
        state ← StateGen.genExecutionState
        x ← arbitrary
        let
          actual = Interpret.runInterpret
            SetX.interpret
            ( state
                { pointer = state.pointer { isDown = false } }
            )
            x
          expected = Right $ Nothing /\ state
            { pointer = state.pointer
                { isDown = false
                , position = state.pointer.position { x = x }
                }
            }

        pure $ actual === expected

    generativeTestCase
      Short
      "sets pointer's x coordinate - with pen down"
      do
        state ← StateGen.genExecutionState
        x ← arbitrary
        let
          actual = Interpret.runInterpret
            SetX.interpret
            (state { pointer = state.pointer { isDown = true } })
            x
          expected = Right $ Nothing /\ state
            { pointer = state.pointer
                { isDown = true
                , position = state.pointer.position { x = x }
                }
            , screen =
                { color: state.pointer.color
                , p1: state.pointer.position
                , p2: state.pointer.position { x = x }
                } :
                  state.screen
            }

        pure $ actual === expected

