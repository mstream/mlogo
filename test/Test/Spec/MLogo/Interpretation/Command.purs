module Test.Spec.MLogo.Interpretation.Command (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Data.Tuple.Nested (type (/\), (/\))
import MLogo.Interpretation.Command (Command(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.State
  ( ExecutionState(..)
  , Line
  , Position(..)
  , Value(..)
  )
import MLogo.Interpretation.State as State
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "Command" do
  describe "clean" do
    it "interprets \"clean\" command" do
      quickCheck \(ExecutionState state) →
        let
          (Command { interpret }) = Command.clean
          actual = Command.runInterpret
            interpret
            (ExecutionState state)
            Nil
          expected = Right $ Nothing /\
            (ExecutionState $ state { screen = Nil })
        in
          actual === expected

    it "interprets \"clearscreen\" command" do
      quickCheck \(ExecutionState state) →
        let
          (Command { interpret }) = Command.clearScreen
          actual = Command.runInterpret
            interpret
            (ExecutionState state)
            Nil
          expected = Right $ Nothing /\
            ( ExecutionState $ state
                { pointer = state.pointer { position = zero }
                , screen = Nil
                }
            )
        in
          actual === expected

    it "interprets \"home\" command" do
      quickCheck \(ExecutionState state) →
        let
          (Command { interpret }) = Command.goHome
          actual = Command.runInterpret
            interpret
            (ExecutionState state)
            Nil
          expected = Right $ Nothing /\
            ( ExecutionState $ state
                { pointer = state.pointer { position = zero } }
            )
        in
          actual === expected

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

  describe "moveBackward" do
    moveBackwardTestCase
      "by a positive steps number"
      [ NumberValue 10.0 ]
      ( Right
          { lines:
              [ { p1: Position { x: 0.0, y: 0.0 }
                , p2: Position { x: 0.0, y: (-10.0) }
                }
              ]
          , pointerPosition: Position { x: 0.0, y: (-10.0) }
          }
      )

    moveBackwardTestCase
      "by a negative steps number"
      [ NumberValue (-10.0) ]
      ( Right
          { lines:
              [ { p1: Position { x: 0.0, y: 0.0 }
                , p2: Position { x: 0.0, y: 10.0 }
                }
              ]
          , pointerPosition: Position { x: 0.0, y: 10.0 }
          }
      )

  describe "moveForward" do
    moveForwardTestCase
      "by a positive steps number"
      [ NumberValue 10.0 ]
      ( Right
          { lines:
              [ { p1: Position { x: 0.0, y: 0.0 }
                , p2: Position { x: 0.0, y: 10.0 }
                }
              ]
          , pointerPosition: Position { x: 0.0, y: 10.0 }
          }
      )

    moveForwardTestCase
      "by a negative steps number"
      [ NumberValue (-10.0) ]
      ( Right
          { lines:
              [ { p1: Position { x: 0.0, y: 0.0 }
                , p2: Position { x: 0.0, y: (-10.0) }
                }
              ]
          , pointerPosition: Position { x: 0.0, y: (-10.0) }
          }
      )

  describe "penDown" do
    penDownTestCase
      "pen previously up"
      false
      true

    penDownTestCase
      "pen previously down"
      true
      true

  describe "penUp" do
    penUpTestCase
      "pen previously down"
      true
      false

    penUpTestCase
      "pen previously up"
      false
      false

  describe "sum" do
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

moveBackwardTestCase
  ∷ String
  → Array Value
  → String \/ { lines ∷ Array Line, pointerPosition ∷ Position }
  → Spec Unit
moveBackwardTestCase title arguments expected =
  commandTestCase
    Command.moveBackward
    title
    State.initialExecutionState
    arguments
    ( ( \exp → Nothing /\ (Newtype.over ExecutionState (f exp))
          State.initialExecutionState
      ) <$> expected
    )
  where
  f exp state = state
    { pointer = state.pointer
        { position = exp.pointerPosition }
    , screen = List.fromFoldable exp.lines
    }

moveForwardTestCase
  ∷ String
  → Array Value
  → String \/ { lines ∷ Array Line, pointerPosition ∷ Position }
  → Spec Unit
moveForwardTestCase title arguments expected =
  commandTestCase
    Command.moveForward
    title
    State.initialExecutionState
    arguments
    ( ( \exp → Nothing /\ (Newtype.over ExecutionState (f exp))
          State.initialExecutionState
      ) <$> expected
    )
  where
  f exp state = state
    { pointer = state.pointer
        { position = exp.pointerPosition }
    , screen = List.fromFoldable exp.lines
    }

penDownTestCase
  ∷ String
  → Boolean
  → Boolean
  → Spec Unit
penDownTestCase title isDown expected =
  commandTestCase
    Command.penDown
    title
    ( ( Newtype.over ExecutionState \state → state
          { pointer = state.pointer
              { isDown = isDown }
          }
      ) State.initialExecutionState
    )
    []
    ( Right $ Nothing /\
        ( Newtype.over ExecutionState \state → state
            { pointer = state.pointer
                { isDown = expected }
            }
        ) State.initialExecutionState
    )

penUpTestCase
  ∷ String
  → Boolean
  → Boolean
  → Spec Unit
penUpTestCase title isDown expected =
  commandTestCase
    Command.penUp
    title
    ( ( Newtype.over ExecutionState \state → state
          { pointer = state.pointer
              { isDown = isDown }
          }
      ) State.initialExecutionState
    )
    []
    ( Right $ Nothing /\
        ( Newtype.over ExecutionState \state → state
            { pointer = state.pointer
                { isDown = expected }
            }
        ) State.initialExecutionState
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
    ( ( Command.runInterpret
          interpret
          state
          (List.fromFoldable arguments)
      )
        `shouldEqual` expected
    )

