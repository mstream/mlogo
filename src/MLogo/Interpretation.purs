module MLogo.Interpretation
  ( Angle(..)
  , ExecutionState
  , Line
  , PointerState
  , Position(..)
  , ScreenState
  , Value(..)
  , run
  ) where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Either.Nested (type (\/))
import Data.Foldable (foldM, foldl)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import MLogo.Parsing (Expression(..), Parameter(..), ProcedureCall(..), Statement(..))

data Value
  = NumberValue Int
  | WordValue String

derive instance Generic Value _

derive instance Eq Value

instance Show Value where
  show = genericShow

newtype Angle = Angle Int

derive newtype instance Eq Angle
derive newtype instance Show Angle
derive newtype instance Semiring Angle
derive newtype instance Ring Angle
derive newtype instance EncodeJson Angle

toRadians :: Angle -> Number
toRadians (Angle n) = Int.toNumber n * Number.pi / 180.0

newtype Steps = Steps Int

derive newtype instance Eq Steps
derive newtype instance Show Steps

derive newtype instance Semiring Steps
derive newtype instance Ring Steps

type PointerState =
  { angle :: Angle
  , isDown :: Boolean
  , position :: Position
  }

initialPointerState :: PointerState
initialPointerState =
  { angle: zero
  , isDown: true
  , position: zero
  }

type ScreenState = List Line

type ExecutionState =
  { callStack ::
      List
        { name :: String
        , boundArguments :: Map Parameter Value
        }
  , pointer :: PointerState
  , procedures ::
      Map String
        { body :: List Statement
        , parameters :: List Parameter
        }
  , screen :: ScreenState
  , variables :: Map String Value
  }

initialExecutionState :: ExecutionState
initialExecutionState =
  { callStack: Nil
  , pointer: initialPointerState
  , procedures: Map.empty
  , screen: Nil
  , variables: Map.empty
  }

newtype Position = Position { x :: Number, y :: Number }

derive instance Generic Position _
derive newtype instance Eq Position
derive newtype instance Show Position
derive newtype instance EncodeJson Position

instance Semiring Position where
  add (Position p1) (Position p2) = Position { x: p1.x + p2.x, y: p1.y + p2.y }
  mul (Position p1) (Position p2) = Position { x: p1.x * p2.x, y: p1.y * p2.y }
  one = Position one
  zero = Position zero

type Line = { p1 :: Position, p2 :: Position }

run :: List Statement -> String \/ ExecutionState
run = interpretStatements initialExecutionState

interpretStatements :: ExecutionState -> List Statement -> String \/ ExecutionState
interpretStatements initialState = foldM f initialState
  where
  f state = case _ of
    ProcedureCallStatement pc ->
      interpretProcedureCallStatement state pc
    ProcedureDefinition name parameters body ->
      interpretProcedureDefinition state name parameters body

interpretProcedureCallStatement :: ExecutionState -> ProcedureCall -> String \/ ExecutionState
interpretProcedureCallStatement state (ProcedureCall name arguments) = do
  evaluatedArguments <- traverse (evaluateExpression state) arguments
  case name of
    "forward" ->
      interpretMoveForward state evaluatedArguments
    "make" ->
      interpretVariableAssignment state evaluatedArguments
    otherName -> do
      { body, parameters } <- Either.note ("Unknown procedure name: " <> otherName) (Map.lookup otherName state.procedures)
      let
        boundArguments = Map.fromFoldable $ List.zip parameters evaluatedArguments
      if Map.size boundArguments /= List.length parameters then
        Left $ "Expected "
          <> (show $ List.length parameters)
          <> " arguments but got "
          <> (show $ List.length arguments)
      else do
        newState <- interpretStatements
          ( state
              { callStack =
                  { name
                  , boundArguments
                  } : state.callStack
              }
          )
          body
        Right $ newState { callStack = state.callStack }

interpretProcedureDefinition :: ExecutionState -> String -> List Parameter -> List Statement -> String \/ ExecutionState
interpretProcedureDefinition state name parameters body =
  Right $ state { procedures = Map.insert name { body, parameters } state.procedures }

interpretVariableAssignment :: ExecutionState -> List Value -> String \/ ExecutionState
interpretVariableAssignment state = case _ of
  _ : Nil ->
    Left errorMessage
  WordValue name : value : Nil ->
    Right $ state { variables = Map.insert name value state.variables }
  _ ->
    Left errorMessage
  where
  errorMessage = "variable takes two arguments: a variable name and variable value"

interpretMoveForward :: ExecutionState -> List Value -> String \/ ExecutionState
interpretMoveForward state = case _ of
  value : Nil ->
    case value of
      NumberValue n ->
        let
          d = Int.toNumber n
          rads = toRadians state.pointer.angle
          target = state.pointer.position + Position { x: d * Number.sin rads, y: d * Number.cos rads }
        in
          Right $ moveTo state target
      WordValue s ->
        Left $ "Word \"" <> s <> "\" is not a number"

  _ ->
    Left "FORWARD takes exactly one parameter"

evaluateExpression :: ExecutionState -> Expression -> String \/ Value
evaluateExpression state = case _ of
  ListLiteral _ ->
    Left "TODO"
  NumericLiteral n ->
    Right $ NumberValue n
  ProcedureCallExpression pc ->
    evaluateProcedureCallExpression state pc
  VariableReference s ->
    evaluateVariableReference state s
  WordLiteral s ->
    Right $ WordValue s

evaluateVariableReference :: ExecutionState -> String -> String \/ Value
evaluateVariableReference state name = case findInProcedureParameters of
  Just value ->
    Right value
  Nothing -> case findInVariables of
    Just value ->
      Right value
    Nothing ->
      Left $ "variable \"" <> name <> "\" not found"
  where
  findInProcedureParameters = case List.head state.callStack of
    Just { boundArguments } ->
      Map.lookup (Parameter name) boundArguments
    Nothing ->
      Nothing
  findInVariables = Map.lookup name state.variables

evaluateProcedureCallExpression :: ExecutionState -> ProcedureCall -> String \/ Value
evaluateProcedureCallExpression state (ProcedureCall name arguments) = case name of
  "sum" ->
    evaluateSum state arguments
  _ ->
    Left "Unknown Command"

evaluateSum :: ExecutionState -> List Expression -> String \/ Value
evaluateSum state = case _ of
  Nil ->
    Left "sum requires at least 2 arguments"
  _ : Nil ->
    Left "sum requires at least 2 arguments"
  xs -> do
    values <- traverse (evaluateExpression state) xs
    numbers <- traverse
      ( case _ of
          NumberValue n ->
            Right n
          WordValue s ->
            Left $ "Word \"" <> s <> " is not a number"
      )
      values
    Right $ NumberValue $ foldl (+) zero numbers

moveTo :: ExecutionState -> Position -> ExecutionState
moveTo state target = state
  { pointer = state.pointer { position = target }
  , screen =
      if state.pointer.isDown then
        { p1: state.pointer.position
        , p2: target
        } : state.screen
      else state.screen
  }

{-
run :: List Statement -> String \/ ExecutionState
run = foldl f
  { pointer:
      { angle: zero
      , isDown: true
      , position: zero
      }
  , screen: Nil
  }
  where
  moveTo acc target = acc
    { pointer = acc.pointer { position = target }
    , screen =
        if acc.pointer.isDown then
          { p1: acc.pointer.position
          , p2: target
          } : acc.screen
        else acc.screen
    }

  f acc = case _ of
    Backward steps ->
      f acc $ Forward $ -steps
    ClearScreen ->
      acc { screen = Nil }
    Forward (Steps n) ->
      let
        d = Int.toNumber n
        rads = toRadians acc.pointer.angle
        target = acc.pointer.position
          { x = acc.pointer.position.x + d * Number.sin rads
          , y = acc.pointer.position.y + d * Number.cos rads
          }
      in
        moveTo acc target
    Home ->
      moveTo acc { x: zero, y: zero }
    Left angle ->
      f acc $ Right $ -angle
    PenDown ->
      acc { pointer = acc.pointer { isDown = true } }
    PenUp ->
      acc { pointer = acc.pointer { isDown = false } }
    Right angle ->
      acc { pointer = acc.pointer { angle = acc.pointer.angle + angle } }
-}
