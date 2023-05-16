module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.SetY
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Newtype (modify, wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.SetY as SetY
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState(..))
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck (arbitrary, (===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "SetY" do
  describe "interpret" do
    generativeTestCase "sets pointer's y coordinate - with pen up" do
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
              , position = modify
                  (_ { y = y })
                  state.pointer.position
              }
          }

      pure $ actual === expected

    generativeTestCase "sets pointer's y coordinate - with pen down" do
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
              , position = modify
                  (_ { y = y })
                  state.pointer.position
              }
          , screen =
              { p1: state.pointer.position
              , p2: modify
                  (_ { y = y })
                  state.pointer.position
              } :
                state.screen
          }

      pure $ actual === expected

