module Test.Spec.MLogo.Interpretation (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Examples (Example(..))
import Examples as Examples
import MLogo.Interpretation as Interpretation
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState, Value(..))
import MLogo.Interpretation.State as State
import MLogo.Parsing.Expression
  ( BinaryOperationType(..)
  , Expression(..)
  )
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.MLogo.Interpretation.Command as Command
import Test.Spec.MLogo.Interpretation.Types as Types
import Test.Types (TestSpec)

spec ∷ TestSpec
spec = describe "Interpretation" do
  Command.spec
  Types.spec
  describe "interpretExpression" do
    expressionTestCase
      "addition of two numeric literals"
      State.initialExecutionState
      ( BinaryOperation
          Addition
          (FloatLiteral 1.0)
          (FloatLiteral 2.0)
      )
      ( Right
          $ (Just $ FloatValue 3.0) /\
              State.initialExecutionState
      )

    expressionTestCase
      "subtraction of two numeric literals"
      State.initialExecutionState
      ( BinaryOperation
          Subtraction
          (FloatLiteral 3.0)
          (FloatLiteral 2.0)
      )
      ( Right
          $ (Just $ FloatValue 1.0) /\
              State.initialExecutionState
      )

    expressionTestCase
      "multiplication of two numeric literals"
      State.initialExecutionState
      ( BinaryOperation
          Multiplication
          (FloatLiteral 3.0)
          (FloatLiteral 2.0)
      )
      ( Right
          $ (Just $ FloatValue 6.0) /\
              State.initialExecutionState
      )

    expressionTestCase
      "division of two numeric literals"
      State.initialExecutionState
      ( BinaryOperation
          Division
          (FloatLiteral 6.0)
          (FloatLiteral 3.0)
      )
      ( Right
          $ (Just $ FloatValue 2.0) /\
              State.initialExecutionState
      )

    expressionTestCase
      "exponentiation of two numeric literals"
      State.initialExecutionState
      ( BinaryOperation
          Exponentiation
          (FloatLiteral 2.0)
          (FloatLiteral 3.0)
      )
      ( Right
          $ (Just $ FloatValue 8.0) /\
              State.initialExecutionState
      )

    expressionTestCase
      "a very long repeat block"
      State.initialExecutionState
      (RepeatBlock (IntegerLiteral 1000000) Nil)
      (Right $ Nothing /\ State.initialExecutionState)

    expressionTestCase
      "a very long for block"
      State.initialExecutionState
      ( ForBlock
          { binder: "i"
          , initialValue: 1
          , step: 1
          , terminalValue: 1000000
          }
          Nil
      )
      (Right $ Nothing /\ State.initialExecutionState)

  describe "interpretExpressions" do
    traverseWithIndex_
      ( \title (Example { ast }) →
          expressionsTestCase title ast
      )
      Examples.examplesByTitle

expressionTestCase
  ∷ String
  → ExecutionState
  → Expression
  → String \/ (Maybe Value /\ ExecutionState)
  → TestSpec
expressionTestCase title state expression expected = it title
  do
    let
      actual = Interpret.runInterpret
        Interpretation.interpretExpression
        state
        expression

    if actual == expected then pure unit
    else
      fail $
        "--- error >>> ---\n"
          <> show actual
          <> "\nis not equal to\n"
          <> show expected
          <> "\n--- expression >>> ---\n"
          <> show expression
          <> "\n--- <<< expression ---"
          <> "\n--- <<< error ---"

expressionsTestCase
  ∷ ∀ f
  . Foldable f
  ⇒ String
  → f Expression
  → TestSpec
expressionsTestCase title expressions = it
  ("terminates execution of \"" <> title <> "\"")
  do
    let
      _ = Interpret.runInterpret
        Interpretation.interpretExpressions
        State.initialExecutionState
        expressions

    pure unit

