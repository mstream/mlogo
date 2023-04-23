module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.Clean
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.Clean as Clean
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState(..))
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "Clean" do
  describe "interpret" do
    it "makes drawings disappear" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            Clean.interpret
            (wrap state)
            unit
          expected = Right $ Nothing /\
            (ExecutionState $ state { screen = Nil })
        in
          actual === expected

