module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.ClearScreen
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.ClearScreen as ClearScreen
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState(..))
import Test.QuickCheck (arbitrary, (===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "ClearScreen" do
  describe "interpret" do
    generativeTestCase
      "makes drawings disappear and moves the pointer back to the origin"
      do
        (ExecutionState state) ← arbitrary
        let
          actual = Interpret.runInterpret
            ClearScreen.interpret
            (wrap state)
            unit
          expected = Right $ Nothing /\
            ( ExecutionState $ state
                { pointer = state.pointer { position = zero }
                , screen = Nil
                }
            )

        pure $ actual === expected

