module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.ClearScreen
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.ClearScreen as ClearScreen
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (Position)
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "ClearScreen" do
  describe "interpret" do
    generativeTestCase
      Short
      "makes drawings disappear and moves the pointer back to the origin"
      do
        state ← StateGen.genExecutionState
        let
          actual = Interpret.runInterpret
            ClearScreen.interpret
            state
            unit

          expected = Right $ Nothing /\ state
            { pointer = state.pointer { position = (zero ∷ Position) }
            , screen = Nil
            }

        pure $ actual === expected

