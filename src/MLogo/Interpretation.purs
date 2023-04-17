module MLogo.Interpretation
  ( interpretExpression
  , interpretExpressions
  ) where

import Prelude

import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Except (class MonadError, runExcept)
import Control.Monad.State
  ( class MonadState
  , get
  , modify_
  , put
  , runStateT
  )
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldM)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap, wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command (Command(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.State (ExecutionState(..), Value(..))
import MLogo.Interpretation.State as State
import MLogo.Parsing (Expression(..), ForBlockSpec, Parameter(..))

interpretExpressions
  ∷ ∀ f m
  . Foldable f
  ⇒ MonadError String m
  ⇒ MonadState ExecutionState m
  ⇒ Interpret m (f Expression)
interpretExpressions =
  foldM f Nothing
  where
  {- TODO: use a more suitable function than foldM -}
  f _ expression = do
    state ← unwrap <$> get
    case state.outputtedValue of
      Just value →
        pure $ Just value
      Nothing → do
        void $ interpretExpression expression
        state' ← unwrap <$> get
        pure state'.outputtedValue

interpretExpression ∷ ∀ m. Interpret m Expression
interpretExpression = case _ of
  Addition leftOperand rightOperand →
    interpretAddition { leftOperand, rightOperand }
  BooleanLiteral b →
    pure $ Just $ BooleanValue b
  Equation leftOperand rightOperand →
    interpretEquation { leftOperand, rightOperand }
  FloatLiteral x →
    pure $ Just $ FloatValue x
  ForBlock spec body →
    interpretForBlock { body, spec }
  IfBlock condition positiveBranch →
    interpretIfElseBlock
      { condition, positiveBranch, negativeBranch: Nil }
  IfElseBlock condition positiveBranch negativeBranch →
    interpretIfElseBlock { condition, positiveBranch, negativeBranch }
  IntegerLiteral n →
    pure $ Just $ IntegerValue n
  Multiplication leftOperand rightOperand →
    interpretMultiplication { leftOperand, rightOperand }
  ProcedureCall name arguments →
    interpretProcedureCall { arguments, name }
  ProcedureDefinition name parameters body →
    interpretProcedureDefinition { body, name, parameters }
  RepeatBlock times body →
    interpretRepeatBlock { body, times }
  StringLiteral s →
    pure $ Just $ WordValue s
  ValueReference name →
    interpretValueReference name
  VariableAssignment name value →
    interpretVariableAssignment { name, value }

interpretAddition
  ∷ ∀ m
  . Interpret m
      { leftOperand ∷ Expression
      , rightOperand ∷ Expression
      }
interpretAddition { leftOperand, rightOperand } =
  interpretProcedureCall
    { arguments: List.fromFoldable [ leftOperand, rightOperand ]
    , name: "sum"
    }

interpretEquation
  ∷ ∀ m
  . Interpret m
      { leftOperand ∷ Expression
      , rightOperand ∷ Expression
      }
interpretEquation { leftOperand, rightOperand } =
  interpretProcedureCall
    { arguments: List.fromFoldable [ leftOperand, rightOperand ]
    , name: "equalp"
    }

interpretMultiplication
  ∷ ∀ m
  . Interpret m
      { leftOperand ∷ Expression
      , rightOperand ∷ Expression
      }
interpretMultiplication { leftOperand, rightOperand } =
  interpretProcedureCall
    { arguments: List.fromFoldable [ leftOperand, rightOperand ]
    , name: "product"
    }

interpretForBlock
  ∷ ∀ m
  . Interpret m
      { body ∷ List Expression
      , spec ∷ ForBlockSpec
      }
interpretForBlock { body, spec } = do
  throwError "TODO"

interpretIfElseBlock
  ∷ ∀ m
  . Interpret m
      { condition ∷ Expression
      , positiveBranch ∷ List Expression
      , negativeBranch ∷ List Expression
      }
interpretIfElseBlock { condition, positiveBranch, negativeBranch } = do
  mbConditionValue ← interpretExpression condition

  case mbConditionValue of
    Nothing →
      throwError "if statement condition does not evaluate to any value"
    Just conditionValue → do
      b ← liftEither $ State.extractBoolean conditionValue
      interpretExpressions
        $ if b then positiveBranch else negativeBranch

interpretProcedureCall
  ∷ ∀ m
  . Interpret m { arguments ∷ List Expression, name ∷ String }
interpretProcedureCall { arguments, name } = do
  state ← get
  case runExcept $ runStateT (evaluateArguments arguments) state of
    Left errorMessage →
      throwError errorMessage
    Right (evaluatedArguments /\ newState) → do
      put newState
      case Map.lookup name Command.commandsByAlias of
        Just (Command command) →
          command.interpret evaluatedArguments
        Nothing → do
          state' ← unwrap <$> get
          case Map.lookup name state'.procedures of
            Nothing →
              throwError $ "Unknown procedure name: " <> name
            Just { body, parameters } →
              interpetUserDefinedProcedureCall
                { body, evaluatedArguments, name, parameters }

interpretRepeatBlock
  ∷ ∀ m
  . Interpret m { body ∷ List Expression, times ∷ Expression }
interpretRepeatBlock { body, times } = do
  mbTimesValue ← interpretExpression times
  case mbTimesValue of
    Just timesValue → do
      n ← liftEither $ State.extractInt timesValue
      if n > 0 then do
        void $ interpretExpressions body
        interpretRepeatBlock { body, times: IntegerLiteral $ n - 1 }
      else pure Nothing
    Nothing →
      throwError
        "repeat counter expression does not evaluate to a value"

interpetUserDefinedProcedureCall
  ∷ ∀ m
  . Interpret m
      { body ∷ List Expression
      , evaluatedArguments ∷ List Value
      , name ∷ String
      , parameters ∷ List Parameter
      }
interpetUserDefinedProcedureCall
  { body, evaluatedArguments, name, parameters } =
  let
    boundArguments =
      Map.fromFoldable $ List.zip parameters evaluatedArguments
  in
    if Map.size boundArguments /= List.length parameters then
      throwError $ "Expected "
        <> (show $ List.length parameters)
        <> " arguments but got "
        <> (show $ Map.size boundArguments)
    else do
      newState ← unwrap <$> get

      when
        (List.length newState.callStack >= maximumCallStackSize)
        (throwError "call stack overflow")

      modify_ $ over
        ExecutionState
        ( \st → st
            { callStack = { boundArguments, name } :
                st.callStack
            }
        )

      mbValue ← interpretExpressions body

      modify_ $ over ExecutionState _
        { callStack = newState.callStack
        , outputtedValue = Nothing
        }

      pure mbValue

evaluateArguments
  ∷ ∀ m
  . MonadError String m
  ⇒ MonadState ExecutionState m
  ⇒ List Expression
  → m (List Value)
evaluateArguments arguments = do
  values ← foldM f Nil arguments
  pure $ List.reverse values
  where
  f ∷ List Value → Expression → m (List Value)
  f values expression = do
    mbValue ← interpretExpression expression
    case mbValue of
      Just value →
        pure (value : values)
      Nothing →
        throwError $
          "argument expression does not evaluate to a value: "
            <> show expression

interpretProcedureDefinition
  ∷ ∀ m
  . Interpret m
      { body ∷ List Expression
      , name ∷ String
      , parameters ∷ List Parameter
      }
interpretProcedureDefinition { body, name, parameters } = do
  modify_ \(ExecutionState state) → wrap $ state
    { procedures = Map.insert name { body, parameters } state.procedures
    }
  pure Nothing

interpretValueReference ∷ ∀ m. Interpret m String
interpretValueReference name = do
  { callStack, variables } ← unwrap <$> get

  let
    findInGlobalVariables = Map.lookup name variables

    findInProcedureParameters = case List.head callStack of
      Just { boundArguments } →
        Map.lookup (Parameter name) boundArguments
      Nothing →
        Nothing

  case findInProcedureParameters of
    Just value →
      pure $ Just value
    Nothing →
      case findInGlobalVariables of
        Just value →
          pure $ Just value
        Nothing →
          throwError $ "value refered by \""
            <> name
            <> "\" name not found"

interpretVariableAssignment
  ∷ ∀ m
  . Interpret m
      { name ∷ String
      , value ∷ Expression
      }
interpretVariableAssignment { name, value } =
  interpretProcedureCall
    { arguments: List.fromFoldable [ StringLiteral name, value ]
    , name: "make"
    }

maximumCallStackSize ∷ Int
maximumCallStackSize = 100

{-
import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Except (class MonadError, runExcept)
import Control.Monad.RWS (put)
import Control.Monad.State (class MonadState, get, modify_, runStateT)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (foldM)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command (Command(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState(..), Value(..))
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
  (ExecutionState state) ← Tuple.snd <$>
    Interpret.runInterpret interpretBlockOfStatements
      State.initialExecutionState
      statements

  if List.null state.callStack then
    Right (ExecutionState state)
  else Left "the call stack has not been cleared"

interpretBlockOfStatements
  ∷ ∀ m
  . MonadError String m
  ⇒ MonadState ExecutionState m
  ⇒ Interpret m (List Statement)
interpretBlockOfStatements =
  foldM f Nothing
  where
  f _ statement = do
    state ← Newtype.unwrap <$> get
    case state.outputtedValue of
      Just value →
        pure $ Just value
      Nothing → do
        void $ interpretStatement statement
        newState ← Newtype.unwrap <$> get
        pure newState.outputtedValue

interpretStatement ∷ ∀ m. Interpret m Statement
interpretStatement = case _ of
  ControlStructureStatement cs →
    interpretControlStructure cs
  ExpressionStatement expression →
    evaluateExpression expression
  ProcedureCall name arguments →
    interpretProcedureCall { arguments, name }
  ProcedureDefinition name parameters body → do
    interpretProcedureDefinition { body, name, parameters }

interpretControlStructure ∷ ∀ m. Interpret m ControlStructure
interpretControlStructure = case _ of
  IfBlock conditionExpr posBranch →
    interpretIfElseBlock { conditionExpr, posBranch, negBranch: Nil }

  IfElseBlock conditionExpr posBranch negBranch →
    interpretIfElseBlock { conditionExpr, posBranch, negBranch }

  OutputCall expression →
    interpretOutputCall expression

  RepeatBlock timesExpression body →
    interpretRepeatBlock { body, timesExpression }

interpretRepeatBlock
  ∷ ∀ m
  . Interpret m { body ∷ List Statement, timesExpression ∷ Statement }
interpretRepeatBlock { body, timesExpression } = do
  mbTimesValue ← interpretStatement timesExpression
  case mbTimesValue of
    Just timesValue → do
      times ← liftEither $ State.extractInt timesValue
      if times > 0 then do
        void $ interpretBlockOfStatements body
        interpretRepeatBlock
          { body
          , timesExpression:
              ( ExpressionStatement
                  $ NumericLiteralExpression
                  $ IntegerLiteral
                  $ times - 1
              )
          }
      else pure Nothing
    Nothing →
      throwError
        "repeat counter expression does not evaluate to a value"

interpretIfElseBlock
  ∷ ∀ m
  . Interpret m
      { conditionExpr ∷ Statement
      , posBranch ∷ List Statement
      , negBranch ∷ List Statement
      }
interpretIfElseBlock { conditionExpr, posBranch, negBranch } = do
  mbConditionValue ← interpretStatement conditionExpr

  case mbConditionValue of
    Nothing →
      throwError "if statement condition does not evaluate to any value"
    Just conditionValue → do
      b ← liftEither $ State.extractBoolean conditionValue

      let
        chosenBranch = if b then posBranch else negBranch

      interpretBlockOfStatements chosenBranch

interpretOutputCall ∷ ∀ m. Interpret m Statement
interpretOutputCall expression = do
  state ← Newtype.unwrap <$> get
  if List.null state.callStack then
    throwError "value output can be returned only from a procedure"
  else do
    mbValue ← interpretStatement expression
    case mbValue of
      Just value → do
        modify_ $ Newtype.over
          ExecutionState
          _ { outputtedValue = Just value }

        pure Nothing

      Nothing →
        throwError "output called with no value"

evaluateArguments
  ∷ ∀ m
  . MonadError String m
  ⇒ MonadState ExecutionState m
  ⇒ List Statement
  → m (List Value)
evaluateArguments arguments = do
  values ← foldM f Nil arguments
  pure $ List.reverse values
  where
  f ∷ List Value → Statement → m (List Value)
  f values expression = do
    mbValue ← interpretStatement expression
    case mbValue of
      Just value →
        pure (value : values)
      Nothing →
        throwError $
          "argument expression does not evaluate to a value: "
            <> show expression

interpretProcedureCall
  ∷ ∀ m. Interpret m { arguments ∷ List Statement, name ∷ String }
interpretProcedureCall { arguments, name } = do
  state ← get
  case runExcept $ runStateT (evaluateArguments arguments) state of
    Left errorMessage →
      throwError errorMessage
    Right (evaluatedArguments /\ newState) → do
      put newState
      case Map.lookup name Command.commandsByAlias of
        Just (Command command) →
          command.interpret evaluatedArguments
        Nothing → do
          state ← Newtype.unwrap <$> get
          case Map.lookup name state.procedures of
            Nothing →
              throwError $ "Unknown procedure name: " <> name
            Just { body, parameters } →
              interpetUserDefinedProcedureCall
                { body, evaluatedArguments, name, parameters }

interpetUserDefinedProcedureCall
  ∷ ∀ m
  . Interpret m
      { body ∷ List Statement
      , evaluatedArguments ∷ List Value
      , name ∷ String
      , parameters ∷ List Parameter
      }
interpetUserDefinedProcedureCall
  { body, evaluatedArguments, name, parameters } =
  let
    boundArguments =
      Map.fromFoldable $ List.zip parameters evaluatedArguments
  in
    if Map.size boundArguments /= List.length parameters then
      throwError $ "Expected "
        <> (show $ List.length parameters)
        <> " arguments but got "
        <> (show $ Map.size boundArguments)
    else do
      newState ← Newtype.unwrap <$> get

      when
        (List.length newState.callStack >= maximumCallStackSize)
        (throwError "call stack overflow")

      modify_ $ Newtype.over
        ExecutionState
        ( \st → st
            { callStack = { boundArguments, name } :
                st.callStack
            }
        )

      mbValue ← interpretBlockOfStatements body

      modify_ $ Newtype.over ExecutionState _
        { callStack = newState.callStack
        , outputtedValue = Nothing
        }

      pure mbValue

interpretProcedureDefinition
  ∷ ∀ m
  . Interpret m
      { body ∷ List Statement
      , name ∷ String
      , parameters ∷ List Parameter
      }
interpretProcedureDefinition { body, name, parameters } = do
  modify_ \(ExecutionState state) → Newtype.wrap $ state
    { procedures = Map.insert name { body, parameters } state.procedures
    }
  pure Nothing

evaluateExpression ∷ ∀ m. Interpret m Expression
evaluateExpression = case _ of
  BooleanLiteral b →
    pure $ Just $ BooleanValue b
  ListLiteral _ →
    throwError "TODO"
  NumericLiteralExpression (IntegerLiteral n) →
    pure $ Just $ IntegerValue n
  NumericLiteralExpression (NumberLiteral x) →
    pure $ Just $ NumberValue x
  VariableReference name →
    evaluateVariableReference name
  WordLiteral s →
    pure $ Just $ WordValue s

evaluateVariableReference ∷ ∀ m. Interpret m String
evaluateVariableReference name = do
  { callStack, variables } ← Newtype.unwrap <$> get

  let
    findInGlobalVariables = Map.lookup name variables

    findInProcedureParameters = case List.head callStack of
      Just { boundArguments } →
        Map.lookup (Parameter name) boundArguments
      Nothing →
        Nothing

  case findInProcedureParameters of
    Just value →
      pure $ Just value
    Nothing →
      case findInGlobalVariables of
        Just value →
          pure $ Just value
        Nothing →
          throwError $ "variable \"" <> name <> "\" not found"

-}
