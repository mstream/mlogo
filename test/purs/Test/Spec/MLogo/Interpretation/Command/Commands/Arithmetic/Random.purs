module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Random
  ( spec
  ) where

import Prelude

import Control.Monad.Gen as Gen
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (all)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as ListNonEmpty
import Data.Maybe (Maybe(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicate1A)
import MLogo.Interpretation.Command.Commands.Arithmetic.Random as Random
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState, Value(..))
import MLogo.Interpretation.State.Gen as StateGen
import Test.QuickCheck (Result(..), (<?), (>=?))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)

spec ∷ TestSpec
spec = describe "Random" do
  describe "interpret" do
    testCase
      "random number is always non-negative"
      (\{ number } → number >=? 0)

    testCase
      "random number is always smaller than the upper limit"
      (\{ number, upperLimit } → number <? upperLimit)

    generativeTestCase "same state produces same numbers" do
      executionState ← StateGen.genExecutionState

      let
        execute ∷ String \/ (Maybe Value /\ ExecutionState)
        execute = Interpret.runInterpret
          Random.interpret
          executionState
          top

        valuesResult ∷ String \/ NonEmptyList (Maybe Value)
        valuesResult = map Tuple.fst <$> replicate1A 100 execute

      pure case valuesResult of
        Left errorMessage →
          Failed $ "execution of commands has failed: " <> errorMessage
        Right values →
          let
            isSameAsFirstValue ∷ Maybe Value → Boolean
            isSameAsFirstValue = eq (ListNonEmpty.head values)
          in
            if all isSameAsFirstValue (ListNonEmpty.tail values) then
              Success
            else Failed $ "not all random values are same: " <> show
              values

    generativeTestCase "updated state produces different numbers" do
      executionState ← StateGen.genExecutionState

      let
        go
          ∷ Int
          → ExecutionState
          → List (Maybe Value)
          → String \/ NonEmptyList (Maybe Value)
        go times previousState acc = do
          mbValue /\ newExecutionState ← Interpret.runInterpret
            Random.interpret
            previousState
            top

          if times > 0 then go (times - 1) newExecutionState
            (mbValue : acc)
          else Right $ ListNonEmpty.cons' mbValue acc

        valuesResult ∷ String \/ NonEmptyList (Maybe Value)
        valuesResult = go 100 executionState Nil

      pure case valuesResult of
        Left errorMessage →
          Failed $ "execution of commands has failed: " <> errorMessage
        Right values →
          let
            isSameAsFirstValue ∷ Maybe Value → Boolean
            isSameAsFirstValue = eq (ListNonEmpty.head values)
          in
            if all isSameAsFirstValue (ListNonEmpty.tail values) then
              Failed $ "all random values are same: " <> show values
            else
              Success

testCase
  ∷ String → ({ number ∷ Int, upperLimit ∷ Int } → Result) → TestSpec
testCase title assertion = generativeTestCase title do
  executionState ← StateGen.genExecutionState
  upperLimit ← Gen.chooseInt 1 top
  let
    actual = Interpret.runInterpret
      Random.interpret
      executionState
      upperLimit

  pure case actual of
    Left errorMessage →
      Failed $ "execution of the command failed: " <> errorMessage
    Right (mbReturnedValue /\ _) →
      case mbReturnedValue of
        Just (IntegerValue number) →
          assertion { number, upperLimit }
        Just otherValue →
          Failed $ "a non integer value has been returned: "
            <> show otherValue
        Nothing →
          Failed "no value has been returned"

