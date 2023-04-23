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
import Data.Newtype (over, unwrap)
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
          (\steps → interpretMoveForward (-steps))
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
          (\angle → interpretTurnRight (-angle))
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

interpretSetPenState ∷ ∀ m. Boolean → Interpret m Unit
interpretSetPenState isDown _ = pure Nothing <* do
  modify_ $ over ExecutionState
    (\st → st { pointer = st.pointer { isDown = isDown } })

interpretMoveForward ∷ ∀ m. Interpret m Number
interpretMoveForward steps = do
  state ← unwrap <$> get

  let
    radians = State.toRadians state.pointer.angle
    target = state.pointer.position + Position
      { x: steps * Number.sin radians, y: steps * Number.cos radians }

  interpretMoveTo target

interpretTurnRight ∷ ∀ m. Interpret m Number
interpretTurnRight angle = pure Nothing <* do
  modify_ $ over ExecutionState
    ( \st → st
        { pointer = st.pointer
            { angle = st.pointer.angle + Angle angle }
        }
    )

interpretGoHome ∷ ∀ m. Interpret m Unit
interpretGoHome _ = pure Nothing <* do
  modify_ $ over ExecutionState
    ( \st → st
        { pointer = st.pointer { position = (zero ∷ Position) }
        }
    )

interpretClean ∷ ∀ m. Interpret m Unit
interpretClean _ = pure Nothing <* do
  modify_ $ over ExecutionState _ { screen = Nil }

interpretClearScreen ∷ ∀ m. Interpret m Unit
interpretClearScreen _ = do
  void $ interpretGoHome unit
  interpretClean unit

interpretMoveTo ∷ ∀ m. Interpret m Position
interpretMoveTo target = pure Nothing <* do
  modify_ $ over ExecutionState
    ( \st → st
        { pointer = st.pointer { position = target }
        , screen =
            if st.pointer.isDown then
              { p1: st.pointer.position
              , p2: target
              } : st.screen
            else st.screen
        }
    )

