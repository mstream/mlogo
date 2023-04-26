module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.XCor
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (modify, wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Graphics.XCor as XCor
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
spec = describe "XCor" do
  describe "interpret" do
    it "outputs pointer's x coordinate" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            XCor.interpret
            (wrap state)
            unit

          (Position { x }) = state.pointer.position

          expected = Right $ (Just $ FloatValue x) /\ (wrap state)
        in
          actual === expected

