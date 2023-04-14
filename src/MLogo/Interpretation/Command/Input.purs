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
  , generateValuesFromParameters
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
import Data.Array.NonEmpty as NEArray
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Exists (Exists)
import Data.Exists as Exists
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.NonEmpty ((:|))
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Traversable as Traversable
import Data.Tuple.Nested (type (/\), (/\))
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.State as State
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen

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

generateValuesFromParameters ∷ Parameters → Gen (Array Value)
generateValuesFromParameters = case _ of
  FixedParameters parameters →
    sequence $ generateValueFromParameter <$> parameters
  VariableParameters parameter →
    Gen.vectorOf 3 (generateValueFromParameter parameter)

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

generateValueFromParameter ∷ Parameter → Gen Value
generateValueFromParameter { valueType } = generateValueOfType valueType

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

generateValueOfType ∷ ValueType → Gen Value
generateValueOfType = case _ of
  AnyType →
    arbitrary
  BooleanType →
    BooleanValue <$> arbitrary
  IntegerType →
    IntegerValue <$> Gen.chooseInt 1 99
  NumberType → do
    x1 ← Gen.chooseInt 1 99
    x2 ← Gen.chooseInt 1 9
    pure $ NumberValue $ Int.toNumber x1 + (Int.toNumber $ x2 / 10)
  WordType →
    WordValue <$> Gen.oneOf
      (pure <$> (NEArray.fromNonEmpty $ "foo" :| [ "bar", "biz" ]))

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

