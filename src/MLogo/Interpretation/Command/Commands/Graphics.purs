module MLogo.Interpretation.Command.Commands.Graphics
  ( clean
  , clearScreen
  , commandsByAlias
  , goHome
  , moveBackward
  , moveForward
  , penDown
  , penUp
  , turnLeft
  , turnRight
  ) where

import Prelude

import Control.Monad.State (get, modify_)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Data.Newtype as Newtype
import Data.Number as Number
import Heterogeneous.Folding as Heterogeneous
import MLogo.Interpretation.Command (Command(..), ToMap(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.State
  ( Angle(..)
  , ExecutionState(..)
  , Position(..)
  )
import MLogo.Interpretation.State as State
import MLogo.Interpretation.Types as Types

commandsByAlias ∷ Map String Command
commandsByAlias = Heterogeneous.hfoldlWithIndex
  ToMap
  (Map.empty ∷ Map String Command)
  { back: moveBackward
  , bk: moveBackward
  , clean: clean
  , clearscreen: clearScreen
  , cs: clearScreen
  , fd: moveForward
  , forward: moveForward
  , home: goHome
  , left: turnLeft
  , lt: turnLeft
  , pd: penDown
  , pendown: penDown
  , penup: penUp
  , pu: penUp
  , right: turnRight
  , rt: turnRight
  }

moveBackward ∷ Command
moveBackward =
  let
    inputParser = Types.fixedNumberInputParser "steps"
  in
    Command
      { description:
          "Move the cursor backward by the given amount of steps."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpretMoveBackward
      , name: "back"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

moveForward ∷ Command
moveForward =
  let
    inputParser = Types.fixedNumberInputParser "steps"
  in
    Command
      { description:
          "Move the cursor forward by the given amount of steps."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpretMoveForward
      , name: "forward"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

clean ∷ Command
clean =
  let
    inputParser = Types.fixedNoInputParser
  in
    Command
      { description: "Clear the drawing area."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpretClean
      , name: "clean"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

clearScreen ∷ Command
clearScreen =
  let
    inputParser = Types.fixedNoInputParser
  in
    Command
      { description:
          "Clear the drawing area and move the cursor to the initial position."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpretClearScreen
      , name: "clearscreen"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

goHome ∷ Command
goHome =
  let
    inputParser = Types.fixedNoInputParser
  in
    Command
      { description: "Move the cursor to the initial position."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpretGoHome
      , name: "home"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

penDown ∷ Command
penDown =
  let
    inputParser = Types.fixedNoInputParser
  in
    Command
      { description: "Make the cursor resume leaving a trail."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          (interpretSetPenState true)
      , name: "pendown"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

penUp ∷ Command
penUp =
  let
    inputParser = Types.fixedNoInputParser
  in
    Command
      { description: "Make the cursor stop leaving a trail."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          (interpretSetPenState false)
      , name: "penup"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpretSetPenState ∷ ∀ m. Boolean → Interpret m Unit
interpretSetPenState isDown _ = do
  modify_ $ over
    ExecutionState
    (\st → st { pointer = st.pointer { isDown = isDown } })

  pure Nothing

turnLeft ∷ Command
turnLeft =
  let
    inputParser = Types.fixedNumberInputParser "angle"
  in
    Command
      { description:
          "Rotate the cursor by the given angle counterclockwise."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpretTurnLeft
      , name: "left"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

turnRight ∷ Command
turnRight =
  let
    inputParser = Types.fixedNumberInputParser "angle"
  in
    Command
      { description: "Rotate the cursor by the given angle clockwise."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpretTurnRight
      , name: "right"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpretMoveForward ∷ ∀ m. Interpret m Number
interpretMoveForward steps = do
  (ExecutionState state) ← get

  let
    rads = State.toRadians state.pointer.angle
    target = state.pointer.position + Position
      { x: steps * Number.sin rads, y: steps * Number.cos rads }

  moveTo target

interpretMoveBackward ∷ ∀ m. Interpret m Number
interpretMoveBackward steps = interpretMoveForward (-steps)

interpretTurnRight ∷ ∀ m. Interpret m Number
interpretTurnRight angle = do
  modify_ \(ExecutionState state) →
    ExecutionState $ state
      { pointer = state.pointer
          { angle = state.pointer.angle + Angle angle }
      }
  pure Nothing

interpretTurnLeft ∷ ∀ m. Interpret m Number
interpretTurnLeft angle = interpretTurnRight (-angle)

interpretGoHome ∷ ∀ m. Interpret m Unit
interpretGoHome _ = do
  modify_ \(ExecutionState state) → ExecutionState $ state
    { pointer = state.pointer { position = (zero ∷ Position) } }
  pure Nothing

interpretClean ∷ ∀ m. Interpret m Unit
interpretClean _ = do
  modify_ $ Newtype.over ExecutionState _ { screen = Nil }
  pure Nothing

interpretClearScreen ∷ ∀ m. Interpret m Unit
interpretClearScreen _ = do
  void $ interpretGoHome unit
  interpretClean unit

moveTo ∷ ∀ m. Interpret m Position
moveTo target = do
  modify_ \(ExecutionState state) → ExecutionState state
    { pointer = state.pointer { position = target }
    , screen =
        if state.pointer.isDown then
          { p1: state.pointer.position
          , p2: target
          } : state.screen
        else state.screen
    }
  pure Nothing

