module MLogo.Interpretation.Command
  ( Command(..)
  , Interpret
  , InterpretCommand
  , commandsByAlias
  , isEqual
  , moveBackward
  , moveForward
  , runInterpret
  , sum
  ) where

import Prelude

import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , throwError
  )
import Control.Monad.Except (Except, runExcept)
import Control.Monad.State (StateT, get, modify_, runStateT)
import Control.Monad.State.Class (class MonadState)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\))
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

type Interpret m i =
  MonadError String m
  ⇒ MonadState ExecutionState m
  ⇒ i
  → m (Maybe Value)

type InterpretCommand =
  List Value → StateT ExecutionState (Except String) (Maybe Value)

runInterpret
  ∷ ∀ i
  . (i → StateT ExecutionState (Except String) (Maybe Value))
  → ExecutionState
  → i
  → String \/ (Maybe Value /\ ExecutionState)
runInterpret computation initialState input =
  runExcept (runStateT (computation input) initialState)

newtype Command = Command
  { description ∷ String
  , interpret ∷ InterpretCommand
  , name ∷ String
  , outputValueType ∷ Maybe ValueType
  , parameters ∷ Parameters
  }

parseAndInterpretInput
  ∷ ∀ i m
  . MonadThrow String m
  ⇒ (List Value → String \/ i)
  → (i → m (Maybe Value))
  → List Value
  → m (Maybe Value)
parseAndInterpretInput parseInput interpretInput values =
  case parseInput values of
    Left errorMessage →
      throwError errorMessage
    Right input →
      interpretInput input

moveBackward ∷ Command
moveBackward =
  let
    inputParser = Input.fixedNumberInputParser "steps"
  in
    Command
      { description:
          "Move the cursor backward by the given amount of steps."
      , interpret: parseAndInterpretInput
          (Input.runFixedInputParser inputParser)
          interpretMoveBackward
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
      , interpret: parseAndInterpretInput
          (Input.runFixedInputParser inputParser)
          interpretMoveForward
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
      , interpret: parseAndInterpretInput
          (Input.runFixedInputParser inputParser)
          interpretClean
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
      , interpret: parseAndInterpretInput
          (Input.runFixedInputParser inputParser)
          interpretClearScreen
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
      , interpret: parseAndInterpretInput
          (Input.runFixedInputParser inputParser)
          interpretGoHome
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
      , interpret: parseAndInterpretInput
          (Input.runFixedInputParser inputParser)
          interpretTurnLeft
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
      , interpret: parseAndInterpretInput
          (Input.runFixedInputParser inputParser)
          interpretTurnRight
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
      , interpret: parseAndInterpretInput
          (Input.runFixedInputParser inputParser)
          interpretVariableAssignment
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
      , interpret: parseAndInterpretInput
          (Input.runFixedInputParser inputParser)
          interpretPenDown
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
      , interpret: parseAndInterpretInput
          (Input.runFixedInputParser inputParser)
          interpretPenUp
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
      , interpret: parseAndInterpretInput
          (Input.runVariableInputParser inputParser)
          interpretSum
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
      , interpret: \values →
          case Input.runVariableInputParser inputParser values of
            Left errorMessage →
              throwError errorMessage
            Right input →
              interpretIsEqual input
      , name: "equal?"
      , outputValueType: Just BooleanType
      , parameters: Input.parametersFromVariableInputParser inputParser
      }

interpretSum ∷ ∀ m. Interpret m (List Number)
interpretSum = pure <<< Just <<< NumberValue <<< foldl (+) zero

interpretIsEqual ∷ ∀ m. Interpret m (List Value)
interpretIsEqual = pure <<< Just <<< BooleanValue <<< go true Nothing
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

interpretVariableAssignment
  ∷ ∀ m. Interpret m { name ∷ String, value ∷ Value }
interpretVariableAssignment { name, value } = do
  modify_ \state →
    state { variables = Map.insert name value state.variables }
  pure Nothing

interpretMoveForward ∷ ∀ m. Interpret m Number
interpretMoveForward steps = do
  state ← get

  let
    rads = State.toRadians state.pointer.angle
    target = state.pointer.position + Position
      { x: steps * Number.sin rads, y: steps * Number.cos rads }

  moveTo target

interpretMoveBackward ∷ ∀ m. Interpret m Number
interpretMoveBackward steps = interpretMoveForward (-steps)

interpretTurnRight ∷ ∀ m. Interpret m Number
interpretTurnRight angle = do
  modify_ \state →
    state
      { pointer = state.pointer
          { angle = state.pointer.angle + Angle angle }
      }
  pure Nothing

interpretTurnLeft ∷ ∀ m. Interpret m Number
interpretTurnLeft angle = interpretTurnRight (-angle)

interpretPenDown ∷ ∀ m. Interpret m Unit
interpretPenDown _ = do
  modify_ \state → state { pointer = state.pointer { isDown = true } }
  pure Nothing

interpretPenUp ∷ ∀ m. Interpret m Unit
interpretPenUp _ = do
  modify_ \state → state { pointer = state.pointer { isDown = false } }
  pure Nothing

interpretGoHome ∷ ∀ m. Interpret m Unit
interpretGoHome _ = do
  modify_ \state → state
    { pointer = state.pointer { position = (zero ∷ Position) } }
  pure Nothing

interpretClean ∷ ∀ m. Interpret m Unit
interpretClean _ = do
  modify_ \state → state { screen = Nil }
  pure Nothing

interpretClearScreen ∷ ∀ m. Interpret m Unit
interpretClearScreen _ = do
  void $ interpretGoHome unit
  interpretClean unit

moveTo ∷ ∀ m. Interpret m Position
moveTo target = do
  modify_ \state → state
    { pointer = state.pointer { position = target }
    , screen =
        if state.pointer.isDown then
          { p1: state.pointer.position
          , p2: target
          } : state.screen
        else state.screen
    }
  pure Nothing

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

