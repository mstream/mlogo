module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands.Arithmetic as Arithmetic
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.State as State
import Test.Spec (Spec, describe)
import Test.Spec.MLogo.Interpretation.Command (commandTestCase)

spec ∷ Spec Unit
spec = describe "Arithmetic" do
  describe "isEqual" do
    isEqualTestCase
      "zero arguments"
      []
      (Right true)

    isEqualTestCase
      "one argument"
      [ FloatValue 1.0 ]
      (Right true)

    isEqualTestCase
      "two equal arguments"
      [ FloatValue 1.0, FloatValue 1.0 ]
      (Right true)

    isEqualTestCase
      "three equal arguments"
      [ FloatValue 1.0, FloatValue 1.0, FloatValue 1.0 ]
      (Right true)

    isEqualTestCase
      "two unequal arguments"
      [ FloatValue 1.0, FloatValue 2.0 ]
      (Right false)

    isEqualTestCase
      "three unequal arguments"
      [ FloatValue 1.0, FloatValue 1.0, FloatValue 2.0 ]
      (Right false)

  describe "sum" do
    sumTestCase
      "zero arguments"
      []
      (Right 0.0)

    sumTestCase
      "one argument"
      [ FloatValue 1.0 ]
      (Right 1.0)

    sumTestCase
      "two arguments"
      [ FloatValue 1.0, FloatValue 2.0 ]
      (Right 3.0)

    sumTestCase
      "three arguments"
      [ FloatValue 1.0, FloatValue 2.0, FloatValue 3.0 ]
      (Right 6.0)

isEqualTestCase
  ∷ String
  → Array Value
  → String \/ Boolean
  → Spec Unit
isEqualTestCase title arguments expected =
  commandTestCase
    Arithmetic.isEqual
    title
    State.initialExecutionState
    arguments
    ( ((_ /\ State.initialExecutionState) <<< Just <<< BooleanValue) <$>
        expected
    )

sumTestCase
  ∷ String
  → Array Value
  → String \/ Number
  → Spec Unit
sumTestCase title arguments expected =
  commandTestCase
    Arithmetic.sum
    title
    State.initialExecutionState
    arguments
    ( ((_ /\ State.initialExecutionState) <<< Just <<< FloatValue) <$>
        expected
    )

