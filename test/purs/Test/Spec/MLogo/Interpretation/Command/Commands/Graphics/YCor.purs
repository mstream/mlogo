module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.YCor
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.YCor as YCor
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State
  ( ExecutionState(..)
  , Position(..)
  , Value(..)
  )
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "YCor" do
  describe "interpret" do
    it "outputs pointer's y coordinate" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            YCor.interpret
            (wrap state)
            unit

          (Position { y }) = state.pointer.position

          expected = Right $ (Just $ FloatValue y) /\ (wrap state)
        in
          actual === expected

