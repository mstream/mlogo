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
    let testCase = commandTestCase Command.isEqual

    testCase
      "zero arguments"
      State.initialExecutionState
      []
      ( Right $ (Just $ BooleanValue true) /\
          State.initialExecutionState
      )

    testCase
      "one argument"
      State.initialExecutionState
      [ NumberValue 1.0 ]
      ( Right $ (Just $ BooleanValue true) /\
          State.initialExecutionState
      )

    testCase
      "two equal arguments"
      State.initialExecutionState
      [ NumberValue 1.0, NumberValue 1.0 ]
      ( Right $ (Just $ BooleanValue true) /\
          State.initialExecutionState
      )

    testCase
      "three equal arguments"
      State.initialExecutionState
      [ NumberValue 1.0, NumberValue 1.0, NumberValue 1.0 ]
      ( Right $ (Just $ BooleanValue true) /\
          State.initialExecutionState
      )

    testCase
      "two unequal arguments"
      State.initialExecutionState
      [ NumberValue 1.0, NumberValue 2.0 ]
      ( Right $ (Just $ BooleanValue false) /\
          State.initialExecutionState
      )

    testCase
      "three unequal arguments"
      State.initialExecutionState
      [ NumberValue 1.0, NumberValue 1.0, NumberValue 2.0 ]
      ( Right $ (Just $ BooleanValue false) /\
          State.initialExecutionState
      )

{-
    testCase
      "two non-equal arguments"
      Command.commandsByAlias."equal?"

    testCase
      "three non-equal arguments"
      Command.commandsByAlias."equal?"
-}

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

