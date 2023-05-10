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
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "ClearScreen" do
  describe "interpret" do
    it
      "makes drawings disappear and moves the pointer back to the origin"
      do
        quickCheck \(ExecutionState state) →
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
          in
            actual === expected

