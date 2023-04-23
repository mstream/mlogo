module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics (spec) where

import Prelude

import Control.Monad.Except (class MonadError)
import Control.Monad.State (class MonadState)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (over, wrap)
import Data.Tuple.Nested (type (/\), (/\))
import MLogo.Interpretation.Command (Command(..))
import MLogo.Interpretation.Command.Commands.Graphics as Graphics
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.Interpret as Interpret
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
import Test.Spec.MLogo.Interpretation.Command
  ( commandTestCase
  , interpretCommand
  )
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "Graphics" do
  describe "clean" do
    it "interprets \"clean\" command" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            interpretCommand
            (wrap state)
            { arguments: Nil, command: Graphics.clean }
          expected = Right $ Nothing /\
            (ExecutionState $ state { screen = Nil })
        in
          actual === expected

    it "interprets \"clearscreen\" command" do
      quickCheck \(ExecutionState state) →
        let
          actual = Interpret.runInterpret
            interpretCommand
            (wrap state)
            { arguments: Nil, command: Graphics.clearScreen }
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
          actual = Interpret.runInterpret
            interpretCommand
            (wrap state)
            { arguments: Nil, command: Graphics.goHome }
          expected = Right $ Nothing /\
            ( ExecutionState $ state
                { pointer = state.pointer { position = zero } }
            )
        in
          actual === expected

  describe "moveBackward" do
    moveBackwardTestCase
      "by a positive steps number"
      [ FloatValue 10.0 ]
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
      [ FloatValue (-10.0) ]
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
      [ FloatValue 10.0 ]
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
      [ FloatValue (-10.0) ]
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

moveBackwardTestCase
  ∷ String
  → Array Value
  → String \/ { lines ∷ Array Line, pointerPosition ∷ Position }
  → Spec Unit
moveBackwardTestCase title arguments expected =
  commandTestCase
    Graphics.moveBackward
    title
    State.initialExecutionState
    arguments
    ( ( \exp → Nothing /\ (over ExecutionState (f exp))
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
    Graphics.moveForward
    title
    State.initialExecutionState
    arguments
    ( ( \exp → Nothing /\ (over ExecutionState (f exp))
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
    Graphics.penDown
    title
    ( ( over ExecutionState \state → state
          { pointer = state.pointer
              { isDown = isDown }
          }
      ) State.initialExecutionState
    )
    []
    ( Right $ Nothing /\
        ( over ExecutionState \state → state
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
    Graphics.penUp
    title
    ( ( over ExecutionState \state → state
          { pointer = state.pointer
              { isDown = isDown }
          }
      ) State.initialExecutionState
    )
    []
    ( Right $ Nothing /\
        ( over ExecutionState \state → state
            { pointer = state.pointer
                { isDown = expected }
            }
        ) State.initialExecutionState
    )

