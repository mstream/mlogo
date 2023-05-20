module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Abs
  ( spec
  ) where

import Prelude

import Control.Monad.Gen as Gen
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Arithmetic.Abs as Abs
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck (Result(..), (===), (>=?))
import Test.QuickCheck.Gen (Gen)
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "Abs" do
  describe "interpret" do
    testCase
      "outputs a non-negative value for any input"
      (Gen.chooseFloat bottom top)
      identity
      (\{ output } → output >=? 0.0)

    testCase
      "for any negated, non-negative input, output the original input"
      (Gen.chooseFloat 0.0 top)
      negate
      (\{ input, output } → output === input)

testCase
  ∷ String
  → Gen Number
  → (Number → Number)
  → ({ input ∷ Number, output ∷ Number } → Result)
  → TestSpec
testCase title genInput inputModifier assertion =
  generativeTestCase title do
    executionState ← StateGen.genExecutionState
    input ← genInput

    let
      actual = Interpret.runInterpret
        Abs.interpret
        executionState
        (inputModifier input)

    pure case actual of
      Left errorMessage →
        Failed $ "command execution has failed: " <> errorMessage
      Right (mbValue /\ newExecutionState) →
        if newExecutionState /= executionState then Failed
          "command alters execution states"
        else case mbValue of
          Just (FloatValue output) →
            assertion { input, output }
          Just otherValue →
            Failed $ "command outputs wrong type of a value: "
              <> show otherValue
          Nothing →
            Failed "command does not output any value"

