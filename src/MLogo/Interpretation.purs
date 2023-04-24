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
import Data.Foldable (class Foldable, foldM, foldl)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap, wrap)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command (Command(..))
import MLogo.Interpretation.Command.Commands as Commands
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.State
  ( ExecutionState(..)
  , Value(..)
  , Variables
  )
import MLogo.Interpretation.State as State
import MLogo.Parsing (Expression(..), ForBlockSpec, Parameter)

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
  Division leftOperand rightOperand →
    interpretDivision { leftOperand, rightOperand }
  Equation leftOperand rightOperand →
    interpretEquation { leftOperand, rightOperand }
  Exponentiation leftOperand rightOperand →
    interpretExponentiation { leftOperand, rightOperand }
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
  ProcedureDefinition { name, parameters } body →
    interpretProcedureDefinition { body, name, parameters }
  RepeatBlock times body →
    interpretRepeatBlock { body, times }
  StringLiteral s →
    pure $ Just $ WordValue s
  SubExpression expression →
    interpretExpression expression
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

interpretDivision
  ∷ ∀ m
  . Interpret m
      { leftOperand ∷ Expression
      , rightOperand ∷ Expression
      }
interpretDivision { leftOperand, rightOperand } =
  interpretProcedureCall
    { arguments: List.fromFoldable [ leftOperand, rightOperand ]
    , name: "quotient"
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

interpretExponentiation
  ∷ ∀ m
  . Interpret m
      { leftOperand ∷ Expression
      , rightOperand ∷ Expression
      }
interpretExponentiation { leftOperand, rightOperand } =
  interpretProcedureCall
    { arguments: List.fromFoldable [ leftOperand, rightOperand ]
    , name: "power"
    }

interpretForBlock
  ∷ ∀ m
  . Interpret m
      { body ∷ List Expression
      , spec ∷ ForBlockSpec
      }
interpretForBlock { body, spec } =
  go spec.initialValue
  where
  go ∷ Interpret m Int
  go n = do
    if n <= spec.terminalValue then do
      let
        localVariables ∷ Variables
        localVariables = Map.singleton spec.binder (IntegerValue n)

      state ← unwrap <$> get

      when
        (List.length state.callStack >= maximumCallStackSize)
        (throwError "call stack overflow")

      modify_ $ over
        ExecutionState
        ( \st → st
            { callStack = { localVariables, name: "for loop" } :
                st.callStack
            }
        )

      void $ interpretExpressions body

      modify_ $ over ExecutionState _
        { callStack = state.callStack
        , outputtedValue = Nothing
        }

      go $ n + spec.step
    else pure Nothing

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
      case Map.lookup name Commands.commandsByAlias of
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
      repCountMax ← liftEither $ State.extractInt timesValue
      (ExecutionState state) ← get
      let
        go repCount =
          if repCount <= repCountMax then do
            modify_ $ over
              ExecutionState
              (\st → st { repCount = repCount })

            void $ interpretExpressions body
            go $ repCount + 1
          else do
            modify_ $ over
              ExecutionState
              (\st → st { repCount = state.repCount })

            pure Nothing
      go 1
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
    localVariables ∷ Variables
    localVariables = Map.fromFoldable
      $ List.zip (unwrap <$> parameters) evaluatedArguments
  in
    if Map.size localVariables /= List.length parameters then
      throwError $ "Expected "
        <> (show $ List.length parameters)
        <> " arguments but got "
        <> (show $ Map.size localVariables)
    else do
      state ← unwrap <$> get

      when
        (List.length state.callStack >= maximumCallStackSize)
        (throwError "call stack overflow")

      modify_ $ over
        ExecutionState
        ( \st → st
            { callStack = { localVariables, name } : st.callStack }
        )

      mbValue ← interpretExpressions body

      modify_ $ over ExecutionState _
        { callStack = state.callStack
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
  { callStack, globalVariables } ← unwrap <$> get

  let
    localVariables ∷ Variables
    localVariables = foldl
      Map.union
      Map.empty
      (_.localVariables <$> callStack)

  case Map.lookup name localVariables of
    Just value →
      pure $ Just value
    Nothing →
      case Map.lookup name globalVariables of
        Just value →
          pure $ Just value
        Nothing →
          throwError $ "value referred by \""
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
