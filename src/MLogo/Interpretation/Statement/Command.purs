module MLogo.Interpretation.Statement.Command
  ( Command(..)
  , InputParser
  , InputParserInfo
  , Interpret
  , Parameter
  , Parameters(..)
  , ValueType(..)
  , clean
  , clearScreen
  , goHome
  , inputParserInfo
  , moveForward
  , moveBackward
  , numberInputParser
  , penDown
  , penUp
  , runInputParser
  , sum
  , turnLeft
  , turnRight
  , variableAssignment
  , wordInputParser
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Exists (Exists)
import Data.Exists as Exists
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Tuple.Nested (type (/\), (/\))
import MLogo.Interpretation.State
  ( Angle(..)
  , ExecutionState
  , Position(..)
  , Value(..)
  )
import MLogo.Interpretation.State as State

type InputParserInfo = List Parameter

inputParserInfo ∷ ∀ a. InputParser a → InputParserInfo
inputParserInfo = go Nil
  where
  go ∷ ∀ b. InputParserInfo → InputParser b → InputParserInfo
  go acc = case _ of
    NilParser _ →
      Nil
    ParameterParser parameter _ →
      parameter : acc
    ChainedParser entry →
      Exists.runExists
        ( \(ChainedParserEntry inputConstructorParser parameterParser) →
            acc
              <> inputParserInfo inputConstructorParser
              <> inputParserInfo parameterParser
        )
        entry

data InputParser a
  = NilParser a
  | ParameterParser Parameter (Value → String \/ a)
  | ChainedParser (Exists (ChainedParserEntry a))

instance Show (InputParser a) where
  show = case _ of
    NilParser _ →
      "NilParser"
    ParameterParser _ _ →
      "ParameterParser"
    ChainedParser entry →
      Exists.runExists
        ( \(ChainedParserEntry inputConstructorParser parameterParser) →
            "ChainedParser("
              <> show inputConstructorParser
              <> "|"
              <> show parameterParser
              <> ")"
        )
        entry

data ChainedParserEntry a x = ChainedParserEntry
  (InputParser (x → a))
  (InputParser x)

instance Functor InputParser where
  map f = case _ of
    ChainedParser entry →
      Exists.runExists
        ( \(ChainedParserEntry inputConstructorParser parameterParser) →
            ChainedParser $ Exists.mkExists $
              ChainedParserEntry
                (map (f <<< _) inputConstructorParser)
                parameterParser
        )
        entry
    NilParser x →
      NilParser $ f x
    ParameterParser parameter parse →
      ParameterParser parameter (\value → f <$> parse value)

instance Apply InputParser where
  apply inputConstructorParser parameterParser =
    ChainedParser $ Exists.mkExists $ ChainedParserEntry
      inputConstructorParser
      parameterParser

instance Applicative InputParser where
  pure = NilParser

runInputParser
  ∷ ∀ a. InputParser a → List Value → String \/ a
runInputParser parser arguments = do
  input /\ _ ← go parser (List.reverse arguments)
  pure input
  where
  go ∷ ∀ b. InputParser b → List Value → String \/ (b /\ List Value)
  go p = case _ of
    Nil →
      case p of
        NilParser x →
          Right $ x /\ Nil
        ParameterParser _ _ →
          Left $ "insufficient number of arguments provided (P)"
        ChainedParser _ →
          Left $ "insufficient number of arguments provided (C)"
    v : vs →
      case p of
        NilParser _ →
          Left $ "too many arguments provided"
        ParameterParser _ parse → do
          x ← parse v
          pure $ x /\ vs
        ChainedParser entry →
          Exists.runExists
            ( \( ChainedParserEntry
                   inputConstructorParser
                   parameterParser
               ) → do
                x /\ _ ← go parameterParser (v : vs)
                f /\ _ ← go inputConstructorParser vs
                pure $ f x /\ vs
            )
            entry

type Interpret i =
  ExecutionState → i → String \/ (Maybe Value /\ ExecutionState)

newtype Command = Command
  { description ∷ String
  , inputParserInfo ∷ InputParserInfo
  , interpret ∷ Interpret (List Value)
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

derive instance Eq ValueType

instance Show ValueType where
  show = case _ of
    AnyType →
      "any"
    IntegerType →
      "integer"
    NumberType →
      "number"
    WordType →
      "word"

anyInputParser ∷ String → InputParser Value
anyInputParser name =
  ParameterParser { name, valueType: AnyType } (pure <<< identity)

numberInputParser ∷ String → InputParser Number
numberInputParser name =
  ParameterParser { name, valueType: NumberType } State.extractNumber

wordInputParser ∷ String → InputParser String
wordInputParser name =
  ParameterParser { name, valueType: WordType } State.extractString

moveBackward ∷ Command
moveBackward =
  let
    inputParser = numberInputParser "steps"
  in
    Command
      { description:
          "Move the cursor backward by the given amount of steps."
      , inputParserInfo: inputParserInfo inputParser
      , interpret: \state values → do
          input ← runInputParser inputParser values
          interpretMoveBackward state input
      , outputValueType: Nothing
      , parameters: FixedParameters
          [ { name: "steps", valueType: NumberType } ]
      }

moveForward ∷ Command
moveForward =
  let
    inputParser = numberInputParser "steps"
  in
    Command
      { description:
          "Move the cursor forward by the given amount of steps."
      , inputParserInfo: inputParserInfo inputParser
      , interpret: \state values → do
          input ← runInputParser inputParser values
          interpretMoveForward state input
      , outputValueType: Nothing
      , parameters: FixedParameters
          [ { name: "steps", valueType: NumberType } ]
      }

clean ∷ Command
clean =
  let
    inputParser = pure unit
  in
    Command
      { description: "Clear the drawing area."
      , inputParserInfo: inputParserInfo inputParser
      , interpret: \state values → do
          input ← runInputParser inputParser values
          interpretClean state input
      , outputValueType: Nothing
      , parameters: FixedParameters []
      }

clearScreen ∷ Command
clearScreen =
  let
    inputParser = pure unit
  in
    Command
      { description:
          "Clear the drawing area and move the cursor to the initial position."
      , inputParserInfo: inputParserInfo inputParser
      , interpret: \state values → do
          input ← runInputParser inputParser values
          interpretClearScreen state input
      , outputValueType: Nothing
      , parameters: FixedParameters []
      }

goHome ∷ Command
goHome =
  let
    inputParser = pure unit
  in
    Command
      { description: "Move the cursor to the initial position."
      , inputParserInfo: inputParserInfo inputParser
      , interpret: \state values → do
          input ← runInputParser inputParser values
          interpretGoHome state input
      , outputValueType: Nothing
      , parameters: FixedParameters []
      }

turnLeft ∷ Command
turnLeft =
  let
    inputParser = numberInputParser "angle"
  in
    Command
      { description:
          "Rotate the cursor by the given angle counterclockwise."
      , inputParserInfo: inputParserInfo inputParser
      , interpret: \state values → do
          input ← runInputParser inputParser values
          interpretTurnLeft state input
      , outputValueType: Nothing
      , parameters: FixedParameters
          [ { name: "angle", valueType: NumberType } ]
      }

turnRight ∷ Command
turnRight =
  let
    inputParser = numberInputParser "angle"
  in
    Command
      { description: "Rotate the cursor by the given angle clockwise."
      , inputParserInfo: inputParserInfo inputParser
      , interpret: \state values → do
          input ← runInputParser inputParser values
          interpretTurnRight state input
      , outputValueType: Nothing
      , parameters: FixedParameters
          [ { name: "angle", valueType: NumberType } ]
      }

variableAssignment ∷ Command
variableAssignment =
  let
    inputParser = ado
      name ← wordInputParser "name"
      value ← anyInputParser "value"
      in { name, value }
  in
    Command
      { description: "Set a global variable value."
      , inputParserInfo: inputParserInfo inputParser
      , interpret: \state values → do
          input ← runInputParser inputParser values
          interpretVariableAssignment state input
      , outputValueType: Nothing
      , parameters: FixedParameters
          [ { name: "name", valueType: WordType }
          , { name: "value", valueType: AnyType }
          ]
      }

penDown ∷ Command
penDown =
  let
    inputParser = pure unit
  in
    Command
      { description: "Make the cursor resume leaving a trail."
      , inputParserInfo: inputParserInfo inputParser
      , interpret: \state values → do
          input ← runInputParser inputParser values
          interpretPenDown state input
      , outputValueType: Nothing
      , parameters: FixedParameters []
      }

penUp ∷ Command
penUp =
  let
    inputParser = pure unit
  in
    Command
      { description: "Make the cursor stop leaving a trail."
      , inputParserInfo: inputParserInfo inputParser
      , interpret: \state values → do
          input ← runInputParser inputParser values
          interpretPenUp state input
      , outputValueType: Nothing
      , parameters: FixedParameters []
      }

sum ∷ Command
sum =
  let
    inputParser = pure Nil
  in
    Command
      { description: "Sums up given numbers."
      , inputParserInfo: inputParserInfo inputParser
      , interpret: \state values → do
          input ← runInputParser inputParser values
          interpretSum state input
      , outputValueType: Just NumberType
      , parameters: VariableParameters
          { name: "addends", valueType: NumberType }
      }

interpretSum ∷ Interpret (List Number)
interpretSum state numbers =
  Right $ (Just $ NumberValue $ foldl (+) zero numbers) /\ state

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

