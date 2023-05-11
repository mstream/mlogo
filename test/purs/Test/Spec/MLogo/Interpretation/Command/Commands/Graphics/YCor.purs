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
import Test.QuickCheck (arbitrary, (===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "YCor" do
  describe "interpret" do
    generativeTestCase "outputs pointer's y coordinate" do
      (ExecutionState state) ← arbitrary
      let
        actual = Interpret.runInterpret
          YCor.interpret
          (wrap state)
          unit

        (Position { y }) = state.pointer.position

        expected = Right $ (Just $ FloatValue y) /\ (wrap state)

      pure $ actual === expected

