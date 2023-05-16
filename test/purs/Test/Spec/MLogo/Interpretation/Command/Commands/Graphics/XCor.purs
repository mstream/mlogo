module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.XCor
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.XCor as XCor
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State
  ( ExecutionState(..)
  , Position(..)
  , Value(..)
  )
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck (arbitrary, (===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "XCor" do
  describe "interpret" do
    generativeTestCase "outputs pointer's x coordinate" do
      state ← StateGen.genExecutionState

      let
        actual = Interpret.runInterpret XCor.interpret state unit
        (Position { x }) = state.pointer.position
        expected = Right $ (Just $ FloatValue x) /\ state

      pure $ actual === expected

