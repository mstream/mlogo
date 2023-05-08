module MLogo.Interpretation
  ( interpretExpression
  , interpretExpressions
  ) where

import Prelude

import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Except (class MonadError, runExcept)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
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
import MLogo.Parsing.Expression
  ( BinaryOperationType(..)
  , Expression(..)
  , ForBlockSpec
  , ParameterName
  , UnaryOperationType(..)
  )

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
  BinaryOperation Addition leftOperand rightOperand →
    interpretAddition { leftOperand, rightOperand }
  BinaryOperation Division leftOperand rightOperand →
    interpretDivision { leftOperand, rightOperand }
  BinaryOperation Equation leftOperand rightOperand →
    interpretEquation { leftOperand, rightOperand }
  BinaryOperation Exponentiation leftOperand rightOperand →
    interpretExponentiation { leftOperand, rightOperand }
  BinaryOperation Multiplication leftOperand rightOperand →
    interpretMultiplication { leftOperand, rightOperand }
  BinaryOperation Subtraction leftOperand rightOperand →
    interpretSubtraction { leftOperand, rightOperand }
  BooleanLiteral b →
    pure $ Just $ BooleanValue b
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
  ProcedureCall name arguments →
    interpretProcedureCall { arguments, name }
  ProcedureDefinition { name, parameterNames } body →
    interpretProcedureDefinition { body, name, parameterNames }
  RepeatBlock times body →
    interpretRepeatBlock { body, times }
  StringLiteral s →
    pure $ Just $ WordValue s
  UnaryOperation Negation operand →
    interpretNegation { operand }
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

interpretNegation ∷ ∀ m. Interpret m { operand ∷ Expression }
interpretNegation { operand } =
  interpretProcedureCall
    { arguments: List.fromFoldable [ operand ]
    , name: "minus"
    }

interpretSubtraction
  ∷ ∀ m
  . Interpret m
      { leftOperand ∷ Expression
      , rightOperand ∷ Expression
      }
interpretSubtraction { leftOperand, rightOperand } =
  interpretProcedureCall
    { arguments: List.fromFoldable [ leftOperand, rightOperand ]
    , name: "difference"
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
  tailRecM go spec.initialValue
  where
  go ∷ Int → m (Step Int (Maybe Value))
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

      pure $ Loop $ n + spec.step
    else pure $ Done Nothing

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
            Just { body, parameterNames } →
              interpetUserDefinedProcedureCall
                { body, evaluatedArguments, name, parameterNames }

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
        go ∷ Int → m (Step Int (Maybe Value))
        go repCount =
          if repCount <= repCountMax then do
            modify_ $ over
              ExecutionState
              (\st → st { repCount = repCount })

            void $ interpretExpressions body
            pure $ Loop $ repCount + 1
          else do
            modify_ $ over
              ExecutionState
              (\st → st { repCount = state.repCount })

            pure $ Done Nothing
      tailRecM go 1
    Nothing →
      throwError
        "repeat counter expression does not evaluate to a value"

interpetUserDefinedProcedureCall
  ∷ ∀ m
  . Interpret m
      { body ∷ List Expression
      , evaluatedArguments ∷ List Value
      , name ∷ String
      , parameterNames ∷ List ParameterName
      }
interpetUserDefinedProcedureCall
  { body, evaluatedArguments, name, parameterNames } =
  let
    localVariables ∷ Variables
    localVariables = Map.fromFoldable
      $ List.zip (unwrap <$> parameterNames) evaluatedArguments
  in
    if Map.size localVariables /= List.length parameterNames then
      throwError $ "Expected "
        <> (show $ List.length parameterNames)
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
  ⇒ MonadRec m
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
      , parameterNames ∷ List ParameterName
      }
interpretProcedureDefinition { body, name, parameterNames } = do
  modify_ \(ExecutionState state) → wrap $ state
    { procedures = Map.insert
        name
        { body, parameterNames }
        state.procedures
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
