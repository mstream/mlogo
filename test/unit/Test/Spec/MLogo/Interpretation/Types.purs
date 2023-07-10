module Test.Spec.MLogo.Interpretation.Types (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List as List
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.Types
  ( FixedInputParser
  , Parameter
  , Parameters(..)
  , ValueType(..)
  , VariableInputParser
  )
import MLogo.Interpretation.Types as Types
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Types (TestSpec)

spec ∷ TestSpec
spec = describe "Input" do

  describe "parametersFromFixedInputParser" do

    parametersFromFixedInputParserTestCase
      "no arguments"
      (Types.fixedNoInputParser)
      []

    parametersFromFixedInputParserTestCase
      "single number argument"
      ( ado
          number ← Types.fixedNumberInputParser "number"
          in { number }
      )
      [ { name: "number", valueType: NumberType } ]

    parametersFromFixedInputParserTestCase
      "single word argument"
      ( ado
          word ← Types.fixedWordInputParser "word"
          in { word }
      )
      [ { name: "word", valueType: WordType } ]

    parametersFromFixedInputParserTestCase
      "two number arguments"
      ( ado
          number1 ← Types.fixedNumberInputParser "number1"
          number2 ← Types.fixedNumberInputParser "number2"
          in { number1, number2 }
      )
      [ { name: "number1", valueType: NumberType }
      , { name: "number2", valueType: NumberType }
      ]

  describe "parametersFromVariableInputParser" do

    parametersFromVariableInputParserTestCase
      "any value"
      (Types.variableAnyInputParser "value")
      { name: "value", valueType: AnyType }

    parametersFromVariableInputParserTestCase
      "number value"
      (Types.variableNumberInputParser "number")
      { name: "number", valueType: NumberType }

    parametersFromVariableInputParserTestCase
      "word value"
      (Types.variableWordInputParser "word")
      { name: "word", valueType: WordType }

  describe "runFixedInputParser" do

    runFixedInputParserTestCase
      "no arguments"
      (pure unit)
      []
      (Right unit)

    runFixedInputParserTestCase
      "single number argument"
      ( ado
          number ← Types.fixedNumberInputParser "number"
          in { number }
      )
      [ FloatValue 10.0 ]
      (Right $ { number: 10.0 })

    runFixedInputParserTestCase
      "two number arguments"
      ( ado
          number1 ← Types.fixedNumberInputParser "number1"
          number2 ← Types.fixedNumberInputParser "number2"
          in { number1, number2 }
      )
      [ FloatValue 10.0, FloatValue 20.0 ]
      (Right $ { number1: 10.0, number2: 20.0 })

    runFixedInputParserTestCase
      "single word argument"
      ( ado
          word ← Types.fixedWordInputParser "word"
          in { word }
      )
      [ WordValue "aaa" ]
      (Right $ { word: "aaa" })

    runFixedInputParserTestCase
      "two word arguments"
      ( ado
          word1 ← Types.fixedWordInputParser "word1"
          word2 ← Types.fixedWordInputParser "word2"
          in { word1, word2 }
      )
      [ WordValue "aaa", WordValue "bbb" ]
      (Right $ { word1: "aaa", word2: "bbb" })

    runFixedInputParserTestCase
      "a number argument followed by a word argument"
      ( ado
          number ← Types.fixedNumberInputParser "number"
          word ← Types.fixedWordInputParser "word"
          in { number, word }
      )
      [ FloatValue 10.0, WordValue "aaa" ]
      (Right $ { number: 10.0, word: "aaa" })

  describe "runVariableInputParser" do

    runVariableInputParserTestCase
      "no arguments"
      (Types.variableAnyInputParser "value")
      []
      (Right [])

    runVariableInputParserTestCase
      "single number argument"
      (Types.variableNumberInputParser "number")
      [ FloatValue 10.0 ]
      (Right [ 10.0 ])

    runVariableInputParserTestCase
      "two number arguments"
      (Types.variableNumberInputParser "number")
      [ FloatValue 10.0, FloatValue 20.0 ]
      (Right [ 10.0, 20.0 ])

    runVariableInputParserTestCase
      "single word argument"
      (Types.variableWordInputParser "word")
      [ WordValue "aaa" ]
      (Right [ "aaa" ])

    runVariableInputParserTestCase
      "two words argument"
      (Types.variableWordInputParser "word")
      [ WordValue "aaa", WordValue "bbb" ]
      (Right [ "aaa", "bbb" ])

    runVariableInputParserTestCase
      "a number follow by a word"
      (Types.variableAnyInputParser "value")
      [ FloatValue 10.0, WordValue "aaa" ]
      (Right [ FloatValue 10.0, WordValue "aaa" ])

parametersFromFixedInputParserTestCase
  ∷ ∀ a
  . String
  → FixedInputParser a
  → Array Parameter
  → TestSpec
parametersFromFixedInputParserTestCase title parser expected = it
  ("extracts parameters from a fixed input parser \"" <> title <> "\"")
  ( (Types.parametersFromFixedInputParser parser)
      `shouldEqual`
        (FixedParameters expected)
  )

parametersFromVariableInputParserTestCase
  ∷ ∀ a
  . String
  → VariableInputParser a
  → Parameter
  → TestSpec
parametersFromVariableInputParserTestCase title parser expected = it
  ( "extracts parameters from a variable input parser \"" <> title <>
      "\""
  )
  ( (Types.parametersFromVariableInputParser parser)
      `shouldEqual`
        (VariableParameters expected)
  )

runFixedInputParserTestCase
  ∷ ∀ a
  . Eq a
  ⇒ Show a
  ⇒ String
  → FixedInputParser a
  → Array Value
  → String \/ a
  → TestSpec
runFixedInputParserTestCase title parser arguments expected = it
  ("parses fixed input \"" <> title <> "\"")
  ( (Types.runFixedInputParser parser $ List.fromFoldable arguments)
      `shouldEqual`
        expected
  )

runVariableInputParserTestCase
  ∷ ∀ a
  . Eq a
  ⇒ Show a
  ⇒ String
  → VariableInputParser a
  → Array Value
  → String \/ Array a
  → TestSpec
runVariableInputParserTestCase title parser arguments expected = it
  ("parses variable input \"" <> title <> "\"")
  ( ( Types.runVariableInputParser parser $ List.fromFoldable
        arguments
    )
      `shouldEqual`
        (List.fromFoldable <$> expected)
  )

