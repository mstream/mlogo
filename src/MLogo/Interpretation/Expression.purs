module MLogo.Interpretation.Expression (evaluate) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import MLogo.Interpretation.State (ExecutionState, Value(..))
import MLogo.Interpretation.State as State
import MLogo.Parsing (Expression(..), Parameter(..), ProcedureCall(..))

evaluate ∷ ExecutionState → Expression → String \/ Value
evaluate state = case _ of
  BooleanLiteral b →
    Right $ BooleanValue b
  ListLiteral _ →
    Left "TODO"
  NumericLiteral x →
    Right $ NumberValue x
  ProcedureCallExpression pc →
    evaluateProcedureCallExpression state pc
  VariableReference s →
    evaluateVariableReference state s
  WordLiteral s →
    Right $ WordValue s

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

evaluateProcedureCallExpression
  ∷ ExecutionState → ProcedureCall → String \/ Value
evaluateProcedureCallExpression state (ProcedureCall name arguments) =
  case name of
    "sum" →
      evaluateSum state arguments
    otherName →
      Left "Unknown Command"

evaluateSum ∷ ExecutionState → List Expression → String \/ Value
evaluateSum state = case _ of
  Nil →
    Left "sum requires at least 2 arguments"
  _ : Nil →
    Left "sum requires at least 2 arguments"
  xs → do
    values ← traverse (evaluate state) xs
    numbers ← traverse State.extractNumber values
    Right $ NumberValue $ foldl (+) zero numbers

