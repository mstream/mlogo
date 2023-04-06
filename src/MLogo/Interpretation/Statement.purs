module MLogo.Interpretation.Statement (interpret) where

import Prelude

import Data.Either (Either(..))
import Data.Either as Either
import Data.Either.Nested (type (\/))
import Data.Foldable (foldM)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Number as Number
import Data.Traversable (traverse)
import MLogo.Interpretation.Expression as Expression
import MLogo.Interpretation.State
  ( Angle(..)
  , ExecutionState
  , Position(..)
  , Value(..)
  )
import MLogo.Interpretation.State as State
import MLogo.Parsing (Parameter, ProcedureCall(..), Statement(..))

interpret ∷ ExecutionState → Statement → String \/ ExecutionState
interpret state = case _ of
  ProcedureCallStatement pc →
    interpretProcedureCall state pc
  ProcedureDefinition name parameters body →
    interpretProcedureDefinition state name parameters body

interpretProcedureCall
  ∷ ExecutionState → ProcedureCall → String \/ ExecutionState
interpretProcedureCall state (ProcedureCall name arguments) = do
  evaluatedArguments ← traverse (Expression.evaluate state) arguments
  case name of
    "back" →
      interpretMoveBackward state evaluatedArguments
    "bk" →
      interpretMoveBackward state evaluatedArguments
    "clean" →
      interpretClean state evaluatedArguments
    "clearscreen" →
      interpretClearScreen state evaluatedArguments
    "cs" →
      interpretClearScreen state evaluatedArguments
    "fd" →
      interpretMoveForward state evaluatedArguments
    "forward" →
      interpretMoveForward state evaluatedArguments
    "home" →
      interpretGoHome state evaluatedArguments
    "left" →
      interpretTurnLeft state evaluatedArguments
    "lt" →
      interpretTurnLeft state evaluatedArguments
    "make" →
      interpretVariableAssignment state evaluatedArguments
    "pd" →
      interpretPenDown state evaluatedArguments
    "pendown" →
      interpretPenDown state evaluatedArguments
    "penup" →
      interpretPenUp state evaluatedArguments
    "pu" →
      interpretPenUp state evaluatedArguments
    "right" →
      interpretTurnRight state evaluatedArguments
    "rt" →
      interpretTurnRight state evaluatedArguments

    otherName → do
      { body, parameters } ← Either.note
        ("Unknown procedure name: " <> otherName)
        (Map.lookup otherName state.procedures)
      let
        boundArguments = Map.fromFoldable $ List.zip parameters
          evaluatedArguments
      if Map.size boundArguments /= List.length parameters then
        Left $ "Expected "
          <> (show $ List.length parameters)
          <> " arguments but got "
          <> (show $ List.length arguments)
      else do
        newState ← foldM
          interpret
          ( state
              { callStack =
                  { name
                  , boundArguments
                  } : state.callStack
              }
          )
          body
        Right $ newState { callStack = state.callStack }

interpretProcedureDefinition
  ∷ ExecutionState
  → String
  → List Parameter
  → List Statement
  → String \/ ExecutionState
interpretProcedureDefinition state name parameters body =
  Right $ state
    { procedures = Map.insert name { body, parameters } state.procedures
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
  value : Nil →
    case value of
      NumberValue n →
        let
          d = Int.toNumber n
          rads = State.toRadians state.pointer.angle
          target = state.pointer.position + Position
            { x: d * Number.sin rads, y: d * Number.cos rads }
        in
          Right $ moveTo state target
      WordValue s →
        Left $ "Word \"" <> s <> "\" is not a number"

  _ →
    Left "FORWARD takes exactly one parameter"

interpretMoveBackward
  ∷ ExecutionState → List Value → String \/ ExecutionState
interpretMoveBackward state = case _ of
  value : Nil →
    case value of
      NumberValue n →
        interpretMoveForward
          state
          (List.fromFoldable [ NumberValue (-n) ])
      WordValue s →
        Left $ "Word \"" <> s <> "\" is not a number"

  _ →
    Left "BACKWARD takes exactly one parameter"

interpretTurnRight
  ∷ ExecutionState → List Value → String \/ ExecutionState
interpretTurnRight state = case _ of
  value : Nil →
    case value of
      NumberValue n →
        Right $ state
          { pointer = state.pointer
              { angle = state.pointer.angle + Angle (Int.toNumber n) }
          }
      WordValue s →
        Left $ "Word \"" <> s <> "\" is not a number"

  _ →
    Left "RIGHT takes exactly one parameter"

interpretTurnLeft
  ∷ ExecutionState → List Value → String \/ ExecutionState
interpretTurnLeft state = case _ of
  value : Nil →
    case value of
      NumberValue n →
        interpretTurnRight state
          (List.fromFoldable [ NumberValue (-n) ])
      WordValue s →
        Left $ "Word \"" <> s <> "\" is not a number"

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

