module MLogo.Interpretation (run) where

import Prelude

import Data.Either (Either(..))
import Data.Either as Either
import Data.Either.Nested (type (\/))
import Data.Foldable (foldM)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import MLogo.Interpretation.Command (Command(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.State (ExecutionState, Value(..))
import MLogo.Interpretation.State as State
import MLogo.Parsing
  ( ControlStructure(..)
  , Expression(..)
  , NumericLiteral(..)
  , Parameter(..)
  , Statement(..)
  )

run ∷ List Statement → String \/ ExecutionState
run statements = do
  state ← Tuple.snd <$> interpretBlockOfStatements
    State.initialExecutionState
    statements

  if List.null state.callStack then
    Right state
  else Left "the call stack has not been cleared"

interpretBlockOfStatements
  ∷ ExecutionState
  → List Statement
  → String \/ (Maybe Value /\ ExecutionState)
interpretBlockOfStatements initialState =
  foldM f (Nothing /\ initialState)
  where
  f
    ∷ (Maybe Value /\ ExecutionState)
    → Statement
    → String \/ (Maybe Value /\ ExecutionState)
  f (_ /\ state) statement =
    case state.outputtedValue of
      Just value →
        Right $ Just value /\ state
      Nothing → do
        _ /\ newState ← interpretStatement state statement
        Right $ newState.outputtedValue /\ newState

interpretStatement
  ∷ ExecutionState
  → Statement
  → String \/ (Maybe Value /\ ExecutionState)
interpretStatement state = case _ of
  ControlStructureStatement cs → do
    newState ← interpretControlStructure state cs
    Right $ Nothing /\ newState
  ExpressionStatement expression → do
    value /\ newState ← evaluateExpression state expression
    Right $ Just value /\ newState
  ProcedureCall name arguments →
    interpretProcedureCall state name arguments
  ProcedureDefinition name parameters body → do
    newState ← interpretProcedureDefinition state name parameters
      body
    Right $ Nothing /\ newState

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

  OutputCall expression →
    interpretOutputCall state expression

  RepeatBlock timesExpression body →
    interpretRepeatBlock state timesExpression body

interpretRepeatBlock
  ∷ ExecutionState
  → Statement
  → List Statement
  → String \/ ExecutionState
interpretRepeatBlock state timesExpression body = do
  timesValue /\ newState ← interpretStatement state timesExpression
  times ← State.extractInt =<< Either.note
    "repeat counter expression does not evaluate to a value"
    timesValue

  if times > 0 then do
    _ /\ newState' ← interpretBlockOfStatements newState body
    interpretRepeatBlock
      newState'
      ( ExpressionStatement
          $ NumericLiteralExpression
          $ IntegerLiteral
          $ times - 1
      )
      body
  else Right newState

interpretIfElseBlock
  ∷ ExecutionState
  → Statement
  → List Statement
  → List Statement
  → String \/ ExecutionState
interpretIfElseBlock
  state
  conditionExpression
  positiveBranch
  negativeBranch =
  do
    conditionValue /\ newState ← interpretStatement
      state
      conditionExpression

    b ← State.extractBoolean =<< Either.note
      "condition does not evaluate to a value"
      conditionValue

    Tuple.snd <$> interpretBlockOfStatements
      newState
      (if b then positiveBranch else negativeBranch)

interpretOutputCall
  ∷ ExecutionState
  → Statement
  → String \/ ExecutionState
interpretOutputCall state expression =
  if List.null state.callStack then
    Left "value output can be returned only from a procedure"
  else do
    mbValue /\ newState ← interpretStatement state expression
    case mbValue of
      Just value →
        Right $ newState { outputtedValue = Just value }
      Nothing →
        Left "output called with no value"

evaluateArguments
  ∷ ExecutionState
  → List Statement
  → String \/ (List Value /\ ExecutionState)
evaluateArguments initialState arguments = do
  values /\ state ← foldM f (Nil /\ initialState) arguments
  Right $ List.reverse values /\ state
  where
  f (values /\ state) expression = do
    mbValue /\ newState ← interpretStatement state expression
    case mbValue of
      Just value →
        Right $ (value : values) /\ newState
      Nothing →
        Left $ "argument expression does not evaluate to a value: "
          <> show expression

interpretProcedureCall
  ∷ ExecutionState
  → String
  → List Statement
  → String \/ (Maybe Value /\ ExecutionState)
interpretProcedureCall state name arguments = do
  evaluatedArguments /\ newState ← evaluateArguments state arguments
  case Map.lookup name Command.commandsByAlias of
    Just (Command command) →
      command.interpret newState evaluatedArguments
    Nothing → do
      { body, parameters } ← Either.note
        ("Unknown procedure name: " <> name)
        (Map.lookup name newState.procedures)
      let
        boundArguments =
          Map.fromFoldable $ List.zip parameters evaluatedArguments
      if Map.size boundArguments /= List.length parameters then
        Left $ "Expected "
          <> (show $ List.length parameters)
          <> " arguments but got "
          <> (show $ List.length arguments)
      else
        do
          let
            newCallStack =
              { name
              , boundArguments
              } : newState.callStack

          mbValue /\ newState' ← interpretBlockOfStatements
            (newState { callStack = newCallStack })
            body

          Right $ mbValue /\ newState'
            { callStack = newState.callStack
            , outputtedValue = Nothing
            }

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

evaluateExpression
  ∷ ExecutionState → Expression → String \/ (Value /\ ExecutionState)
evaluateExpression state = case _ of
  BooleanLiteral b →
    Right $ BooleanValue b /\ state
  ListLiteral _ →
    Left "TODO"
  NumericLiteralExpression (IntegerLiteral n) →
    Right $ IntegerValue n /\ state
  NumericLiteralExpression (NumberLiteral x) →
    Right $ NumberValue x /\ state
  VariableReference s → do
    value ← evaluateVariableReference state s
    Right $ value /\ state
  WordLiteral s →
    Right $ WordValue s /\ state

evaluateVariableReference ∷ ExecutionState → String → String \/ Value
evaluateVariableReference state name = case findInProcedureParameters of
  Just value →
    Right value
  Nothing → case findInVariables of
    Just value →
      Right value
    Nothing →
      Left $ "variable \"" <> name <> "\" not found"
  where
  findInProcedureParameters = case List.head state.callStack of
    Just { boundArguments } →
      Map.lookup (Parameter name) boundArguments
    Nothing →
      Nothing
  findInVariables = Map.lookup name state.variables

