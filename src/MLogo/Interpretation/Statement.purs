module MLogo.Interpretation.Statement (commands, interpretMany) where

import Prelude

import Data.Either (Either(..))
import Data.Either as Either
import Data.Either.Nested (type (\/))
import Data.Foldable (foldM)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import MLogo.Interpretation.Expression as Expression
import MLogo.Interpretation.State (ExecutionState, Value)
import MLogo.Interpretation.State as State
import MLogo.Interpretation.Statement.Command (Command)
import MLogo.Interpretation.Statement.Command as Command
import MLogo.Parsing
  ( ControlStructure(..)
  , Expression(..)
  , NumericLiteral(..)
  , Parameter
  , ProcedureCall(..)
  , Statement(..)
  )

interpretMany
  ∷ ExecutionState
  → List Statement
  → String \/ ExecutionState
interpretMany state = foldM (\st s → Tuple.snd <$> interpret st s) state

interpret
  ∷ ExecutionState
  → Statement
  → String \/ (Maybe Value /\ ExecutionState)
interpret state = case _ of
  ControlStructureStatement cs → do
    newState ← interpretControlStructure state cs
    Right $ Nothing /\ newState
  ProcedureCallStatement pc →
    interpretProcedureCall state pc
  ProcedureDefinition name parameters body → do
    newState ← interpretProcedureDefinition state name parameters body
    pure $ Nothing /\ newState

interpretControlStructure
  ∷ ExecutionState → ControlStructure → String \/ ExecutionState
interpretControlStructure state = case _ of
  IfBlock conditionExpression body →
    interpretIfElseBlock
      state
      conditionExpression
      body
      Nil

  IfElseBlock conditionExpression positiveBranch negativeBranch →
    interpretIfElseBlock
      state
      conditionExpression
      positiveBranch
      negativeBranch

  RepeatBlock timesExpression body →
    interpretRepeatBlock state timesExpression body

interpretRepeatBlock
  ∷ ExecutionState
  → Expression
  → List Statement
  → String \/ ExecutionState
interpretRepeatBlock state timesExpression body = do
  times ← State.extractInt =<< Expression.evaluate
    state
    timesExpression

  if times > 0 then do
    newState ← interpretMany state body
    interpretRepeatBlock
      newState
      (NumericLiteralExpression $ IntegerLiteral $ times - 1)
      body
  else Right state

interpretIfElseBlock
  ∷ ExecutionState
  → Expression
  → List Statement
  → List Statement
  → String \/ ExecutionState
interpretIfElseBlock
  state
  conditionExpression
  positiveBranch
  negativeBranch =
  do
    b ← State.extractBoolean =<< Expression.evaluate
      state
      conditionExpression

    if b then interpretMany state positiveBranch
    else interpretMany state negativeBranch

commands ∷ Map String Command
commands = Map.fromFoldable
  [ "back" /\ Command.moveBackward
  , "bk" /\ Command.moveBackward
  , "clean" /\ Command.clean
  , "clearscreen" /\ Command.clearScreen
  , "cs" /\ Command.clearScreen
  , "fd" /\ Command.moveForward
  , "forward" /\ Command.moveForward
  , "home" /\ Command.goHome
  , "left" /\ Command.turnLeft
  , "lt" /\ Command.turnLeft
  , "make" /\ Command.variableAssignment
  , "pd" /\ Command.penDown
  , "pendown" /\ Command.penDown
  , "penup" /\ Command.penUp
  , "pu" /\ Command.penUp
  , "right" /\ Command.turnRight
  , "rt" /\ Command.turnRight
  , "sum" /\ Command.sum
  ]

interpretProcedureCall
  ∷ ExecutionState
  → ProcedureCall
  → String \/ (Maybe Value /\ ExecutionState)
interpretProcedureCall state (ProcedureCall name arguments) = do
  evaluatedArguments ← traverse (Expression.evaluate state) arguments
  case Map.lookup name commands of
    Just command →
      command.interpret state evaluatedArguments
    Nothing → do
      { body, parameters } ← Either.note
        ("Unknown procedure name: " <> name)
        (Map.lookup name state.procedures)
      let
        boundArguments = Map.fromFoldable $ List.zip parameters
          evaluatedArguments
      if Map.size boundArguments /= List.length parameters then
        Left $ "Expected "
          <> (show $ List.length parameters)
          <> " arguments but got "
          <> (show $ List.length arguments)
      else
        do
          newState ← interpretMany
            ( state
                { callStack =
                    { name
                    , boundArguments
                    } : state.callStack
                }
            )
            body

          Right $ Nothing /\ newState { callStack = state.callStack }

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

