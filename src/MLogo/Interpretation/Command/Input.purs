module MLogo.Interpretation.Command.Input
  ( FixedInputParser
  , Parameter
  , Parameters(..)
  , ValueType(..)
  , VariableInputParser
  , fixedAnyInputParser
  , fixedNoInputParser
  , fixedNumberInputParser
  , fixedWordInputParser
  , parametersFromFixedInputParser
  , parametersFromVariableInputParser
  , runFixedInputParser
  , runVariableInputParser
  , variableAnyInputParser
  , variableNumberInputParser
  , variableWordInputParser
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Exists (Exists)
import Data.Exists as Exists
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable as Traversable
import Data.Tuple.Nested (type (/\), (/\))
import Heterogeneous.Folding (class FoldingWithIndex)
import Heterogeneous.Folding as Heterogeneous
import MLogo.Interpretation.State
  ( Angle(..)
  , ExecutionState
  , Position(..)
  , Value(..)
  )
import MLogo.Interpretation.State as State
import Type.Proxy (Proxy)

data FixedInputParser a
  = NilParser a
  | ParameterParser Parameter (Value → String \/ a)
  | ChainedParser (Exists (ChainedParserEntry a))

data ChainedParserEntry a x = ChainedParserEntry
  (FixedInputParser (x → a))
  (FixedInputParser x)

instance Functor FixedInputParser where
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

instance Apply FixedInputParser where
  apply inputConstructorParser parameterParser =
    ChainedParser $ Exists.mkExists $ ChainedParserEntry
      inputConstructorParser
      parameterParser

instance Applicative FixedInputParser where
  pure = NilParser

parametersFromFixedInputParser ∷ ∀ a. FixedInputParser a → Parameters
parametersFromFixedInputParser = FixedParameters <<< Array.fromFoldable
  <<< go Nil
  where
  go ∷ ∀ b. List Parameter → FixedInputParser b → List Parameter
  go acc = case _ of
    NilParser _ →
      Nil
    ParameterParser parameter _ →
      parameter : acc
    ChainedParser entry →
      Exists.runExists
        ( \(ChainedParserEntry inputConstructorParser parameterParser) →
            acc
              <> go Nil inputConstructorParser
              <> go Nil parameterParser
        )
        entry

runFixedInputParser
  ∷ ∀ a. FixedInputParser a → List Value → String \/ a
runFixedInputParser parser arguments = do
  input /\ _ ← go parser (List.reverse arguments)
  pure input
  where
  go
    ∷ ∀ b. FixedInputParser b → List Value → String \/ (b /\ List Value)
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

data VariableInputParser a = VariableInputParser
  Parameter
  (Value → String \/ a)

runVariableInputParser
  ∷ ∀ a. VariableInputParser a → List Value → String \/ List a
runVariableInputParser (VariableInputParser _ parse) =
  Traversable.traverse parse

parametersFromVariableInputParser
  ∷ ∀ a. VariableInputParser a → Parameters
parametersFromVariableInputParser (VariableInputParser parameter _) =
  VariableParameters parameter

data Parameters
  = FixedParameters (Array Parameter)
  | VariableParameters Parameter

derive instance Generic Parameters _

derive instance Eq Parameters

instance Show Parameters where
  show = genericShow

type Parameter =
  { name ∷ String
  , valueType ∷ ValueType
  }

anyParameter ∷ String → Parameter
anyParameter name = { name, valueType: AnyType }

wordParameter ∷ String → Parameter
wordParameter name = { name, valueType: WordType }

numberParameter ∷ String → Parameter
numberParameter name = { name, valueType: NumberType }

data ValueType
  = AnyType
  | BooleanType
  | IntegerType
  | NumberType
  | WordType

derive instance Eq ValueType

instance Show ValueType where
  show = case _ of
    AnyType →
      "any"
    BooleanType →
      "boolean"
    IntegerType →
      "integer"
    NumberType →
      "number"
    WordType →
      "word"

fixedNoInputParser ∷ FixedInputParser Unit
fixedNoInputParser = NilParser unit

fixedAnyInputParser ∷ String → FixedInputParser Value
fixedAnyInputParser name =
  ParameterParser (anyParameter name) (pure <<< identity)

variableAnyInputParser ∷ String → VariableInputParser Value
variableAnyInputParser name =
  VariableInputParser (anyParameter name) (pure <<< identity)

fixedNumberInputParser ∷ String → FixedInputParser Number
fixedNumberInputParser name =
  ParameterParser (numberParameter name) State.extractNumber

variableNumberInputParser ∷ String → VariableInputParser Number
variableNumberInputParser name = VariableInputParser
  (numberParameter name)
  State.extractNumber

fixedWordInputParser ∷ String → FixedInputParser String
fixedWordInputParser name =
  ParameterParser (wordParameter name) State.extractString

variableWordInputParser ∷ String → VariableInputParser String
variableWordInputParser name = VariableInputParser
  (wordParameter name)
  State.extractString

