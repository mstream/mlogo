module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.ReRandom
  ( spec
  ) where

import Prelude

import Control.Monad.Gen as Gen
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Arithmetic.ReRandom as ReRandom
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "ReRandom" do
  describe "interpret" do

    generativeTestCase Short "sets random number seed" do
      executionState ← StateGen.genExecutionState
      seed ← Gen.chooseInt bottom top

      let
        actual = Interpret.runInterpret
          ReRandom.interpret
          executionState
          seed

        expected = Right $ Nothing /\ executionState
          { randomNumberSeed = seed }

      pure $ actual === expected

