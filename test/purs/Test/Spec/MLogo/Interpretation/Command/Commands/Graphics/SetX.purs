module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.SetX
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Newtype (modify, wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.SetX as SetX
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState(..))
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "SetX" do
  describe "interpret" do
    it
      "sets pointer's x coordinate - with pen up"
      do
        quickCheck \(ExecutionState state) x →
          let
            actual = Interpret.runInterpret
              SetX.interpret
              ( wrap state
                  { pointer = state.pointer { isDown = false } }
              )
              x
            expected = Right $ Nothing /\
              ( ExecutionState $ state
                  { pointer = state.pointer
                      { isDown = false
                      , position = modify
                          (_ { x = x })
                          state.pointer.position
                      }
                  }
              )
          in
            actual === expected

    it
      "sets pointer's x coordinate - with pen down"
      do
        quickCheck \(ExecutionState state) x →
          let
            actual = Interpret.runInterpret
              SetX.interpret
              (wrap state { pointer = state.pointer { isDown = true } })
              x
            expected = Right $ Nothing /\
              ( ExecutionState $ state
                  { pointer = state.pointer
                      { isDown = true
                      , position = modify
                          (_ { x = x })
                          state.pointer.position
                      }
                  , screen =
                      { p1: state.pointer.position
                      , p2: modify
                          (_ { x = x })
                          state.pointer.position
                      } :
                        state.screen
                  }
              )
          in
            actual === expected

