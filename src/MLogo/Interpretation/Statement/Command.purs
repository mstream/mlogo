module MLogo.Interpretation.Statement.Command
  ( Command
  , Parameter
  , Parameters(..)
  , ValueType(..)
  , clean
  , clearScreen
  , goHome
  , moveForward
  , moveBackward
  , penDown
  , penUp
  , turnLeft
  , turnRight
  , variableAssignment
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Number as Number
import MLogo.Interpretation.State
  ( Angle(..)
  , ExecutionState
  , Position(..)
  , Value(..)
  )
import MLogo.Interpretation.State as State

type Command =
  { description ∷ String
  , interpret ∷ ExecutionState → List Value → String \/ ExecutionState
  , parameters ∷ Parameters
  }

data Parameters
  = FixedParameters (Array Parameter)
  | VariableParameters Parameter

type Parameter =
  { name ∷ String
  , valueType ∷ ValueType
  }

data ValueType
  = AnyType
  | IntegerType
  | NumberType
  | WordType

moveBackward ∷ Command
moveBackward =
  { description:
      "Move the cursor backward by the given amount of steps."
  , interpret: interpretMoveBackward
  , parameters: FixedParameters
      [ { name: "steps", valueType: NumberType } ]
  }

moveForward ∷ Command
moveForward =
  { description: "Move the cursor forward by the given amount of steps."
  , interpret: interpretMoveForward
  , parameters: FixedParameters
      [ { name: "steps", valueType: NumberType } ]
  }

clean ∷ Command
clean =
  { description: "Clear the drawing area."
  , interpret: interpretClean
  , parameters: FixedParameters []
  }

clearScreen ∷ Command
clearScreen =
  { description:
      "Clear the drawing area and move the cursor to the initial position."
  , interpret: interpretClearScreen
  , parameters: FixedParameters []
  }

goHome ∷ Command
goHome =
  { description: "Move the cursor to the initial position."
  , interpret: interpretGoHome
  , parameters: FixedParameters []
  }

turnLeft ∷ Command
turnLeft =
  { description:
      "Rotate the cursor by the given angle counterclockwise."
  , interpret: interpretTurnLeft
  , parameters: FixedParameters
      [ { name: "angle", valueType: NumberType } ]
  }

turnRight ∷ Command
turnRight =
  { description: "Rotate the cursor by the given angle clockwise."
  , interpret: interpretTurnRight
  , parameters: FixedParameters
      [ { name: "angle", valueType: NumberType } ]
  }

variableAssignment ∷ Command
variableAssignment =
  { description: "Set a global variable value."
  , interpret: interpretVariableAssignment
  , parameters: FixedParameters
      [ { name: "name", valueType: WordType }
      , { name: "value", valueType: AnyType }
      ]
  }

penDown ∷ Command
penDown =
  { description: "Make the cursor resume leaving a trail."
  , interpret: interpretPenDown
  , parameters: FixedParameters []
  }

penUp ∷ Command
penUp =
  { description: "Make the cursor stop leaving a trail."
  , interpret: interpretPenUp
  , parameters: FixedParameters []
  }

interpretVariableAssignment
  ∷ ExecutionState → List Value → String \/ ExecutionState
interpretVariableAssignment state = case _ of
  _ : Nil →
    Left errorMessage
  WordValue name : value : Nil →
    Right $ state { variables = Map.insert name value state.variables }
  _ →
    Left errorMessage
  where
  errorMessage =
    "variable takes two arguments: a variable name and variable value"

interpretMoveForward
  ∷ ExecutionState → List Value → String \/ ExecutionState
interpretMoveForward state = case _ of
  value : Nil → do
    x ← State.extractNumber value
    let
      rads = State.toRadians state.pointer.angle
      target = state.pointer.position + Position
        { x: x * Number.sin rads, y: x * Number.cos rads }

    Right $ moveTo state target
  _ →
    Left "FORWARD takes exactly one parameter"

interpretMoveBackward
  ∷ ExecutionState → List Value → String \/ ExecutionState
interpretMoveBackward state = case _ of
  value : Nil → do
    x ← State.extractNumber value
    interpretMoveForward
      state
      (List.fromFoldable [ NumberValue (-x) ])
  _ →
    Left "BACKWARD takes exactly one parameter"

interpretTurnRight
  ∷ ExecutionState → List Value → String \/ ExecutionState
interpretTurnRight state = case _ of
  value : Nil → do
    x ← State.extractNumber value
    Right $ state
      { pointer = state.pointer
          { angle = state.pointer.angle + Angle x }
      }
  _ →
    Left "RIGHT takes exactly one parameter"

interpretTurnLeft
  ∷ ExecutionState → List Value → String \/ ExecutionState
interpretTurnLeft state = case _ of
  value : Nil → do
    x ← State.extractNumber value
    interpretTurnRight
      state
      (List.fromFoldable [ NumberValue (-x) ])
  _ →
    Left "RIGHT takes exactly one argument"

interpretPenDown
  ∷ ExecutionState → List Value → String \/ ExecutionState
interpretPenDown state = case _ of
  Nil →
    Right $ state { pointer = state.pointer { isDown = true } }
  _ →
    Left "PENDOWN takes no arguments"

interpretPenUp
  ∷ ExecutionState → List Value → String \/ ExecutionState
interpretPenUp state = case _ of
  Nil →
    Right $ state { pointer = state.pointer { isDown = false } }
  _ →
    Left "PENUP takes no arguments"

interpretGoHome
  ∷ ExecutionState → List Value → String \/ ExecutionState
interpretGoHome state = case _ of
  Nil →
    Right $ state
      { pointer = state.pointer { position = (zero ∷ Position) } }
  _ →
    Left "HOME takes no arguments"

interpretClean
  ∷ ExecutionState → List Value → String \/ ExecutionState
interpretClean state = case _ of
  Nil →
    Right $ state { screen = Nil }
  _ →
    Left "CLEAN takes no arguments"

interpretClearScreen
  ∷ ExecutionState → List Value → String \/ ExecutionState
interpretClearScreen state = case _ of
  Nil → do
    newState ← interpretGoHome state Nil
    interpretClean newState Nil
  _ →
    Left "CLEAN takes no arguments"

moveTo ∷ ExecutionState → Position → ExecutionState
moveTo state target = state
  { pointer = state.pointer { position = target }
  , screen =
      if state.pointer.isDown then
        { p1: state.pointer.position
        , p2: target
        } : state.screen
      else state.screen
  }

