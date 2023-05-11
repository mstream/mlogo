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
import Test.QuickCheck (arbitrary, (===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "Home" do
  describe "interpret" do
    generativeTestCase
      "moves the pointer back to the origin - with pen up"
      do
        (ExecutionState state) ← arbitrary
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

        pure $ actual === expected

    generativeTestCase
      "moves the pointer back to the origin - with pen down"
      do
        (ExecutionState state) ← arbitrary
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
        pure $ actual === expected

