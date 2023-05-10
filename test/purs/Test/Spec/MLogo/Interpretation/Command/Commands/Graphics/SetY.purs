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
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "SetY" do
  describe "interpret" do
    it
      "sets pointer's y coordinate - with pen up"
      do
        quickCheck \(ExecutionState state) y →
          let
            actual = Interpret.runInterpret
              SetY.interpret
              ( wrap state
                  { pointer = state.pointer { isDown = false } }
              )
              y
            expected = Right $ Nothing /\
              ( ExecutionState $ state
                  { pointer = state.pointer
                      { isDown = false
                      , position = modify
                          (_ { y = y })
                          state.pointer.position
                      }
                  }
              )
          in
            actual === expected

    it
      "sets pointer's y coordinate - with pen down"
      do
        quickCheck \(ExecutionState state) y →
          let
            actual = Interpret.runInterpret
              SetY.interpret
              (wrap state { pointer = state.pointer { isDown = true } })
              y
            expected = Right $ Nothing /\
              ( ExecutionState $ state
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
              )
          in
            actual === expected

