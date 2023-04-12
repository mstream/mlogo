module Test.Spec.MLogo.Interpretation.Command (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import MLogo.Interpretation.Command (Command(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.State (ExecutionState, Value(..))
import MLogo.Interpretation.State as State
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Command" do
  describe "isEqual" do

    isEqualTestCase
      "zero arguments"
      []
      (Right true)

    isEqualTestCase
      "one argument"
      [ NumberValue 1.0 ]
      (Right true)

    isEqualTestCase
      "two equal arguments"
      [ NumberValue 1.0, NumberValue 1.0 ]
      (Right true)

    isEqualTestCase
      "three equal arguments"
      [ NumberValue 1.0, NumberValue 1.0, NumberValue 1.0 ]
      (Right true)

    isEqualTestCase
      "two unequal arguments"
      [ NumberValue 1.0, NumberValue 2.0 ]
      (Right false)

    isEqualTestCase
      "three unequal arguments"
      [ NumberValue 1.0, NumberValue 1.0, NumberValue 2.0 ]
      (Right false)

    sumTestCase
      "zero arguments"
      []
      (Right 0.0)

    sumTestCase
      "one argument"
      [ NumberValue 1.0 ]
      (Right 1.0)

    sumTestCase
      "two arguments"
      [ NumberValue 1.0, NumberValue 2.0 ]
      (Right 3.0)

    sumTestCase
      "three arguments"
      [ NumberValue 1.0, NumberValue 2.0, NumberValue 3.0 ]
      (Right 6.0)

isEqualTestCase
  ∷ String
  → Array Value
  → String \/ Boolean
  → Spec Unit
isEqualTestCase title arguments expected =
  commandTestCase
    Command.isEqual
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
    Command.sum
    title
    State.initialExecutionState
    arguments
    ( ((_ /\ State.initialExecutionState) <<< Just <<< NumberValue) <$>
        expected
    )

commandTestCase
  ∷ Command
  → String
  → ExecutionState
  → Array Value
  → String \/ (Maybe Value /\ ExecutionState)
  → Spec Unit
commandTestCase
  (Command { interpret, name })
  title
  state
  arguments
  expected =
  it
    ("interprets \"" <> name <> "\" command: " <> title)
    ( (interpret state (List.fromFoldable arguments))
        `shouldEqual` expected
    )

