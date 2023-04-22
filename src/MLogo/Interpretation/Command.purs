module MLogo.Interpretation.Command
  ( Command(..)
  , clean
  , clearScreen
  , commandsByAlias
  , goHome
  , isEqual
  , moveBackward
  , moveForward
  , parsingContext
  , penDown
  , penUp
  , sinus
  , sum
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.State (get, modify_)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Data.Number as Number
import Data.Symbol (class IsSymbol, reflectSymbol)
import Heterogeneous.Folding (class FoldingWithIndex)
import Heterogeneous.Folding as Heterogeneous
import MLogo.Interpretation.Command.Input
  ( Parameters(..)
  , ValueType(..)
  )
import MLogo.Interpretation.Command.Input as Input
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.State
  ( Angle(..)
  , ExecutionState(..)
  , Position(..)
  , Value(..)
  )
import MLogo.Interpretation.State as State
import MLogo.Parsing (ParsingContext)
import Type.Proxy (Proxy)

newtype Command = Command
  { description ∷ String
  , interpret ∷ ∀ m. Interpret m (List Value)
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

sinus ∷ Command
sinus =
  let
    inputParser = Input.fixedNumberInputParser "angle"
  in
    Command
      { description:
          "Calculates sinus of a given angle."
      , interpret: parseAndInterpretInput
          (Input.runFixedInputParser inputParser)
          interpretSinus
      , name: "sin"
      , outputValueType: Just NumberType
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

repCount ∷ Command
repCount =
  let
    inputParser = Input.fixedNoInputParser
  in
    Command
      { description:
          "outputs the repetition count of the innermost current REPEAT or FOREVER, starting from 1"
      , interpret: parseAndInterpretInput
          (Input.runFixedInputParser inputParser)
          interpretRepCount
      , name: "repcount"
      , outputValueType: Just IntegerType
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
      , name: "equalp"
      , outputValueType: Just BooleanType
      , parameters: Input.parametersFromVariableInputParser inputParser
      }

interpretSum ∷ ∀ m. Interpret m (List Number)
interpretSum = pure <<< Just <<< FloatValue <<< foldl (+) zero

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
  modify_ \(ExecutionState state) →
    ExecutionState $ state
      { globalVariables = Map.insert name value state.globalVariables }
  pure Nothing

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

interpretSinus ∷ ∀ m. Interpret m Number
interpretSinus =
  pure <<< Just <<< FloatValue <<< Number.sin <<< degreesToRadians

degreesToRadians ∷ Number → Number
degreesToRadians x = x * Number.pi / 180.0

interpretTurnLeft ∷ ∀ m. Interpret m Number
interpretTurnLeft angle = interpretTurnRight (-angle)

interpretPenDown ∷ ∀ m. Interpret m Unit
interpretPenDown _ = do
  modify_ \(ExecutionState state) → ExecutionState $ state
    { pointer = state.pointer { isDown = true } }
  pure Nothing

interpretPenUp ∷ ∀ m. Interpret m Unit
interpretPenUp _ = do
  modify_ \(ExecutionState state) → ExecutionState $ state
    { pointer = state.pointer { isDown = false } }
  pure Nothing

interpretRepCount ∷ ∀ m. Interpret m Unit
interpretRepCount _ = do
  (ExecutionState state) ← get
  pure $ Just $ IntegerValue state.repCount

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

parsingContext ∷ ParsingContext
parsingContext = commandsByAlias
  <#> \(Command { parameters }) → case parameters of
    FixedParameters ps →
      Just $ Array.length ps
    VariableParameters _ →
      Nothing

commandsByAlias ∷ Map String Command
commandsByAlias = Heterogeneous.hfoldlWithIndex
  ToMap
  (Map.empty ∷ Map String Command)
  { back: moveBackward
  , bk: moveBackward
  , clean: clean
  , clearscreen: clearScreen
  , cs: clearScreen
  , equalp: isEqual
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
  , repcount: repCount
  , right: turnRight
  , rt: turnRight
  , sin: sinus
  , sum
  }

