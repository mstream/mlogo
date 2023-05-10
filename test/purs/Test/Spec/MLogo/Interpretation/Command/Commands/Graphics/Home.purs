module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.Home
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.Home as Home
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState(..))
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "Home" do
  describe "interpret" do
    it
      "moves the pointer back to the origin - with pen up"
      do
        quickCheck \(ExecutionState state) →
          let
            actual = Interpret.runInterpret
              Home.interpret
              ( wrap state
                  { pointer = state.pointer { isDown = false } }
              )
              unit
            expected = Right $ Nothing /\
              ( ExecutionState $ state
                  { pointer = state.pointer
                      { isDown = false, position = zero }
                  }
              )
          in
            actual === expected

    it
      "moves the pointer back to the origin - with pen down"
      do
        quickCheck \(ExecutionState state) →
          let
            actual = Interpret.runInterpret
              Home.interpret
              (wrap state { pointer = state.pointer { isDown = true } })
              unit
            expected = Right $ Nothing /\
              ( ExecutionState $ state
                  { pointer = state.pointer
                      { isDown = true, position = zero }
                  , screen = { p1: state.pointer.position, p2: zero } :
                      state.screen
                  }
              )
          in
            actual === expected

