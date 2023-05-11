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
import Test.QuickCheck (arbitrary, (===))
import Test.Spec (Spec, describe, it)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "Clean" do
  describe "interpret" do
    generativeTestCase "makes drawings disappear" do
      (ExecutionState state) ← arbitrary
      let
        actual = Interpret.runInterpret
          Clean.interpret
          (wrap state)
          unit
        expected = Right $ Nothing /\
          (ExecutionState $ state { screen = Nil })

      pure $ actual === expected

