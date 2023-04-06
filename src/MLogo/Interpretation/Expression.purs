module MLogo.Interpretation.Expression (evaluate) where

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
import MLogo.Interpretation.State (ExecutionState)
import MLogo.Interpretation.State (ExecutionState, Value(..))
import MLogo.Interpretation.State as State
import MLogo.Parsing (Expression(..), Parameter(..), ProcedureCall(..), Statement(..))

evaluate :: ExecutionState -> Expression -> String \/ Value
evaluate state = case _ of
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
    values <- traverse (evaluate state) xs
    numbers <- traverse
      ( case _ of
          NumberValue n ->
            Right n
          WordValue s ->
            Left $ "Word \"" <> s <> " is not a number"
      )
      values
    Right $ NumberValue $ foldl (+) zero numbers

