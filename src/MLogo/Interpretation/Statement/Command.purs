module MLogo.Interpretation.Statement.Command
  ( Command
  , Interpret
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
  , sum
  , turnLeft
  , turnRight
  , variableAssignment
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import MLogo.Interpretation.State
  ( Angle(..)
  , ExecutionState
  , Position(..)
  , Value(..)
  )
import MLogo.Interpretation.State as State

type Interpret =
  ExecutionState
  → List Value
  → String \/ (Maybe Value /\ ExecutionState)

type Command =
  { description ∷ String
  , interpret ∷ Interpret
  , outputValueType ∷ Maybe ValueType
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
  , outputValueType: Nothing
  , parameters: FixedParameters
      [ { name: "steps", valueType: NumberType } ]
  }

moveForward ∷ Command
moveForward =
  { description: "Move the cursor forward by the given amount of steps."
  , interpret: interpretMoveForward
  , outputValueType: Nothing
  , parameters: FixedParameters
      [ { name: "steps", valueType: NumberType } ]
  }

clean ∷ Command
clean =
  { description: "Clear the drawing area."
  , interpret: interpretClean
  , outputValueType: Nothing
  , parameters: FixedParameters []
  }

clearScreen ∷ Command
clearScreen =
  { description:
      "Clear the drawing area and move the cursor to the initial position."
  , interpret: interpretClearScreen
  , outputValueType: Nothing
  , parameters: FixedParameters []
  }

goHome ∷ Command
goHome =
  { description: "Move the cursor to the initial position."
  , interpret: interpretGoHome
  , outputValueType: Nothing
  , parameters: FixedParameters []
  }

turnLeft ∷ Command
turnLeft =
  { description:
      "Rotate the cursor by the given angle counterclockwise."
  , interpret: interpretTurnLeft
  , outputValueType: Nothing
  , parameters: FixedParameters
      [ { name: "angle", valueType: NumberType } ]
  }

turnRight ∷ Command
turnRight =
  { description: "Rotate the cursor by the given angle clockwise."
  , interpret: interpretTurnRight
  , outputValueType: Nothing
  , parameters: FixedParameters
      [ { name: "angle", valueType: NumberType } ]
  }

variableAssignment ∷ Command
variableAssignment =
  { description: "Set a global variable value."
  , interpret: interpretVariableAssignment
  , outputValueType: Nothing
  , parameters: FixedParameters
      [ { name: "name", valueType: WordType }
      , { name: "value", valueType: AnyType }
      ]
  }

penDown ∷ Command
penDown =
  { description: "Make the cursor resume leaving a trail."
  , interpret: interpretPenDown
  , outputValueType: Nothing
  , parameters: FixedParameters []
  }

penUp ∷ Command
penUp =
  { description: "Make the cursor stop leaving a trail."
  , interpret: interpretPenUp
  , outputValueType: Nothing
  , parameters: FixedParameters []
  }

sum ∷ Command
sum =
  { description: "Sums up given numbers."
  , interpret: interpretSum
  , outputValueType: Just NumberType
  , parameters: VariableParameters
      { name: "addends", valueType: NumberType }
  }

interpretSum ∷ Interpret
interpretSum state values = do
  numbers ← traverse State.extractNumber values
  Right $ (Just $ NumberValue $ foldl (+) zero numbers) /\ state

interpretVariableAssignment ∷ Interpret
interpretVariableAssignment state = case _ of
  _ : Nil →
    Left errorMessage
  WordValue name : value : Nil →
    Right $ Nothing /\ state
      { variables = Map.insert name value state.variables }
  _ →
    Left errorMessage
  where
  errorMessage =
    "variable takes two arguments: a variable name and variable value"

interpretMoveForward ∷ Interpret
interpretMoveForward state = case _ of
  value : Nil → do
    x ← State.extractNumber value
    let
      rads = State.toRadians state.pointer.angle
      target = state.pointer.position + Position
        { x: x * Number.sin rads, y: x * Number.cos rads }

    Right $ Nothing /\ moveTo state target
  _ →
    Left "FORWARD takes exactly one parameter"

interpretMoveBackward ∷ Interpret
interpretMoveBackward state = case _ of
  value : Nil → do
    x ← State.extractNumber value
    interpretMoveForward
      state
      (List.fromFoldable [ NumberValue (-x) ])
  _ →
    Left "BACKWARD takes exactly one parameter"

interpretTurnRight ∷ Interpret
interpretTurnRight state = case _ of
  value : Nil → do
    x ← State.extractNumber value
    Right $ Nothing /\ state
      { pointer = state.pointer
          { angle = state.pointer.angle + Angle x }
      }
  _ →
    Left "RIGHT takes exactly one parameter"

interpretTurnLeft ∷ Interpret
interpretTurnLeft state = case _ of
  value : Nil → do
    x ← State.extractNumber value
    interpretTurnRight
      state
      (List.fromFoldable [ NumberValue (-x) ])
  _ →
    Left "RIGHT takes exactly one argument"

interpretPenDown ∷ Interpret
interpretPenDown state = case _ of
  Nil →
    Right $ Nothing /\ state
      { pointer = state.pointer { isDown = true } }
  _ →
    Left "PENDOWN takes no arguments"

interpretPenUp ∷ Interpret
interpretPenUp state = case _ of
  Nil →
    Right $ Nothing /\ state
      { pointer = state.pointer { isDown = false } }
  _ →
    Left "PENUP takes no arguments"

interpretGoHome ∷ Interpret
interpretGoHome state = case _ of
  Nil →
    Right $ Nothing /\ state
      { pointer = state.pointer { position = (zero ∷ Position) } }
  _ →
    Left "HOME takes no arguments"

interpretClean ∷ Interpret
interpretClean state = case _ of
  Nil →
    Right $ Nothing /\ state { screen = Nil }
  _ →
    Left "CLEAN takes no arguments"

interpretClearScreen ∷ Interpret
interpretClearScreen state = case _ of
  Nil → do
    _ /\ newState ← interpretGoHome state Nil
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

