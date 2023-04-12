module MLogo.Interpretation.Command
  ( Command(..)
  , Interpret
  , commandsByAlias
  , isEqual
  , sum
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Heterogeneous.Folding (class FoldingWithIndex)
import Heterogeneous.Folding as Heterogeneous
import MLogo.Interpretation.Command.Input (Parameters, ValueType(..))
import MLogo.Interpretation.Command.Input as Input
import MLogo.Interpretation.State
  ( Angle(..)
  , ExecutionState
  , Position(..)
  , Value(..)
  )
import MLogo.Interpretation.State as State
import Type.Proxy (Proxy)

type Interpret i =
  ExecutionState → i → String \/ (Maybe Value /\ ExecutionState)

newtype Command = Command
  { description ∷ String
  , interpret ∷ Interpret (List Value)
  , name ∷ String
  , outputValueType ∷ Maybe ValueType
  , parameters ∷ Parameters
  }

moveBackward ∷ Command
moveBackward =
  let
    inputParser = Input.fixedNumberInputParser "steps"
  in
    Command
      { description:
          "Move the cursor backward by the given amount of steps."
      , interpret: \state values → do
          input ← Input.runFixedInputParser inputParser values
          interpretMoveBackward state input
      , name: "back"
      , outputValueType: Nothing
      , parameters: Input.parametersFromFixedInputParser inputParser
      }

moveForward ∷ Command
moveForward =
  let
    inputParser = Input.fixedNumberInputParser "steps"
  in
    Command
      { description:
          "Move the cursor forward by the given amount of steps."
      , interpret: \state values → do
          input ← Input.runFixedInputParser inputParser values
          interpretMoveForward state input
      , name: "forward"
      , outputValueType: Nothing
      , parameters: Input.parametersFromFixedInputParser inputParser
      }

clean ∷ Command
clean =
  let
    inputParser = Input.fixedNoInputParser
  in
    Command
      { description: "Clear the drawing area."
      , interpret: \state values → do
          input ← Input.runFixedInputParser inputParser values
          interpretClean state input
      , name: "clean"
      , outputValueType: Nothing
      , parameters: Input.parametersFromFixedInputParser inputParser
      }

clearScreen ∷ Command
clearScreen =
  let
    inputParser = Input.fixedNoInputParser
  in
    Command
      { description:
          "Clear the drawing area and move the cursor to the initial position."
      , interpret: \state values → do
          input ← Input.runFixedInputParser inputParser values
          interpretClearScreen state input
      , name: "clearscreen"
      , outputValueType: Nothing
      , parameters: Input.parametersFromFixedInputParser inputParser
      }

goHome ∷ Command
goHome =
  let
    inputParser = Input.fixedNoInputParser
  in
    Command
      { description: "Move the cursor to the initial position."
      , interpret: \state values → do
          input ← Input.runFixedInputParser inputParser values
          interpretGoHome state input
      , name: "home"
      , outputValueType: Nothing
      , parameters: Input.parametersFromFixedInputParser inputParser
      }

turnLeft ∷ Command
turnLeft =
  let
    inputParser = Input.fixedNumberInputParser "angle"
  in
    Command
      { description:
          "Rotate the cursor by the given angle counterclockwise."
      , interpret: \state values → do
          input ← Input.runFixedInputParser inputParser values
          interpretTurnLeft state input
      , name: "left"
      , outputValueType: Nothing
      , parameters: Input.parametersFromFixedInputParser inputParser
      }

turnRight ∷ Command
turnRight =
  let
    inputParser = Input.fixedNumberInputParser "angle"
  in
    Command
      { description: "Rotate the cursor by the given angle clockwise."
      , interpret: \state values → do
          input ← Input.runFixedInputParser inputParser values
          interpretTurnRight state input
      , name: "right"
      , outputValueType: Nothing
      , parameters: Input.parametersFromFixedInputParser inputParser
      }

variableAssignment ∷ Command
variableAssignment =
  let
    inputParser = ado
      name ← Input.fixedWordInputParser "name"
      value ← Input.fixedAnyInputParser "value"
      in { name, value }
  in
    Command
      { description: "Set a global variable value."
      , interpret: \state values → do
          input ← Input.runFixedInputParser inputParser values
          interpretVariableAssignment state input
      , name: "make"
      , outputValueType: Nothing
      , parameters: Input.parametersFromFixedInputParser inputParser
      }

penDown ∷ Command
penDown =
  let
    inputParser = Input.fixedNoInputParser
  in
    Command
      { description: "Make the cursor resume leaving a trail."
      , interpret: \state values → do
          input ← Input.runFixedInputParser inputParser values
          interpretPenDown state input
      , name: "pendown"
      , outputValueType: Nothing
      , parameters: Input.parametersFromFixedInputParser inputParser
      }

penUp ∷ Command
penUp =
  let
    inputParser = Input.fixedNoInputParser
  in
    Command
      { description: "Make the cursor stop leaving a trail."
      , interpret: \state values → do
          input ← Input.runFixedInputParser inputParser values
          interpretPenUp state input
      , name: "penup"
      , outputValueType: Nothing
      , parameters: Input.parametersFromFixedInputParser inputParser
      }

sum ∷ Command
sum =
  let
    inputParser = Input.variableNumberInputParser "addend"
  in
    Command
      { description: "Sums up given numbers."
      , interpret: \state values → do
          input ← Input.runVariableInputParser inputParser values
          interpretSum state input
      , name: "sum"
      , outputValueType: Just NumberType
      , parameters: Input.parametersFromVariableInputParser inputParser
      }

isEqual ∷ Command
isEqual =
  let
    inputParser = Input.variableAnyInputParser "value"
  in
    Command
      { description: "Tells if values are equal."
      , interpret: \state values → do
          input ← Input.runVariableInputParser inputParser values
          interpretIsEqual state input
      , name: "equal?"
      , outputValueType: Just BooleanType
      , parameters: Input.parametersFromVariableInputParser inputParser
      }

interpretSum ∷ Interpret (List Number)
interpretSum state numbers =
  Right $ (Just $ NumberValue $ foldl (+) zero numbers) /\ state

interpretIsEqual ∷ Interpret (List Value)
interpretIsEqual state values =
  Right $ (Just $ BooleanValue $ go true Nothing values) /\ state
  where
  go ∷ Boolean → (Maybe Value) → (List Value) → Boolean
  go acc mbLast = case _ of
    Nil →
      acc
    v : vs →
      let
        continue = go acc (Just v) vs
      in
        case mbLast of
          Nothing →
            continue
          Just last →
            if v == last then
              continue
            else
              false

interpretVariableAssignment ∷ Interpret { name ∷ String, value ∷ Value }
interpretVariableAssignment state { name, value } =
  Right $ Nothing /\ state
    { variables = Map.insert name value state.variables }

interpretMoveForward ∷ Interpret Number
interpretMoveForward state steps =
  let
    rads = State.toRadians state.pointer.angle
    target = state.pointer.position + Position
      { x: steps * Number.sin rads, y: steps * Number.cos rads }
  in
    Right $ Nothing /\ moveTo state target

interpretMoveBackward ∷ Interpret Number
interpretMoveBackward state steps = interpretMoveForward state (-steps)

interpretTurnRight ∷ Interpret Number
interpretTurnRight state angle =
  Right $ Nothing /\ state
    { pointer = state.pointer
        { angle = state.pointer.angle + Angle angle }
    }

interpretTurnLeft ∷ Interpret Number
interpretTurnLeft state angle = interpretTurnRight state (-angle)

interpretPenDown ∷ Interpret Unit
interpretPenDown state _ =
  Right $ Nothing /\ state
    { pointer = state.pointer { isDown = true } }

interpretPenUp ∷ Interpret Unit
interpretPenUp state _ =
  Right $ Nothing /\ state
    { pointer = state.pointer { isDown = false } }

interpretGoHome ∷ Interpret Unit
interpretGoHome state _ =
  Right $ Nothing /\ state
    { pointer = state.pointer { position = (zero ∷ Position) } }

interpretClean ∷ Interpret Unit
interpretClean state _ =
  Right $ Nothing /\ state { screen = Nil }

interpretClearScreen ∷ Interpret Unit
interpretClearScreen state _ = do
  _ /\ newState ← interpretGoHome state unit
  interpretClean newState unit

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

data ToMap = ToMap

instance
  ( IsSymbol sym
  ) ⇒
  FoldingWithIndex
    ToMap
    (Proxy sym)
    (Map String Command)
    Command
    (Map String Command) where
  foldingWithIndex ToMap prop acc val =
    Map.insert (reflectSymbol prop) val acc

commandsByAlias ∷ Map String Command
commandsByAlias = Heterogeneous.hfoldlWithIndex
  ToMap
  (Map.empty ∷ Map String Command)
  { back: moveBackward
  , bk: moveBackward
  , clean: clean
  , clearscreen: clearScreen
  , cs: clearScreen
  , "equal?": isEqual
  , fd: moveForward
  , forward: moveForward
  , home: goHome
  , left: turnLeft
  , lt: turnLeft
  , make: variableAssignment
  , pd: penDown
  , pendown: penDown
  , penup: penUp
  , pu: penUp
  , right: turnRight
  , rt: turnRight
  , sum
  }

