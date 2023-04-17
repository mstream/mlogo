module Test.Spec.MLogo.Interpretation.Command.Input (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List as List
import MLogo.Interpretation.Command.Input
  ( FixedInputParser
  , Parameter
  , Parameters(..)
  , ValueType(..)
  , VariableInputParser
  )
import MLogo.Interpretation.Command.Input as Input
import MLogo.Interpretation.State (Value(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Input" do

  describe "parametersFromFixedInputParser" do

    parametersFromFixedInputParserTestCase
      "no arguments"
      (Input.fixedNoInputParser)
      []

    parametersFromFixedInputParserTestCase
      "single number argument"
      ( ado
          number ← Input.fixedNumberInputParser "number"
          in { number }
      )
      [ { name: "number", valueType: NumberType } ]

    parametersFromFixedInputParserTestCase
      "single word argument"
      ( ado
          word ← Input.fixedWordInputParser "word"
          in { word }
      )
      [ { name: "word", valueType: WordType } ]

    parametersFromFixedInputParserTestCase
      "two number arguments"
      ( ado
          number1 ← Input.fixedNumberInputParser "number1"
          number2 ← Input.fixedNumberInputParser "number2"
          in { number1, number2 }
      )
      [ { name: "number1", valueType: NumberType }
      , { name: "number2", valueType: NumberType }
      ]

  describe "parametersFromVariableInputParser" do

    parametersFromVariableInputParserTestCase
      "any value"
      (Input.variableAnyInputParser "value")
      { name: "value", valueType: AnyType }

    parametersFromVariableInputParserTestCase
      "number value"
      (Input.variableNumberInputParser "number")
      { name: "number", valueType: NumberType }

    parametersFromVariableInputParserTestCase
      "word value"
      (Input.variableWordInputParser "word")
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
          number ← Input.fixedNumberInputParser "number"
          in { number }
      )
      [ FloatValue 10.0 ]
      (Right $ { number: 10.0 })

    runFixedInputParserTestCase
      "two number arguments"
      ( ado
          number1 ← Input.fixedNumberInputParser "number1"
          number2 ← Input.fixedNumberInputParser "number2"
          in { number1, number2 }
      )
      [ FloatValue 10.0, FloatValue 20.0 ]
      (Right $ { number1: 10.0, number2: 20.0 })

    runFixedInputParserTestCase
      "single word argument"
      ( ado
          word ← Input.fixedWordInputParser "word"
          in { word }
      )
      [ WordValue "aaa" ]
      (Right $ { word: "aaa" })

    runFixedInputParserTestCase
      "two word arguments"
      ( ado
          word1 ← Input.fixedWordInputParser "word1"
          word2 ← Input.fixedWordInputParser "word2"
          in { word1, word2 }
      )
      [ WordValue "aaa", WordValue "bbb" ]
      (Right $ { word1: "aaa", word2: "bbb" })

    runFixedInputParserTestCase
      "a number argument followed by a word argument"
      ( ado
          number ← Input.fixedNumberInputParser "number"
          word ← Input.fixedWordInputParser "word"
          in { number, word }
      )
      [ FloatValue 10.0, WordValue "aaa" ]
      (Right $ { number: 10.0, word: "aaa" })

  describe "runVariableInputParser" do

    runVariableInputParserTestCase
      "no arguments"
      (Input.variableAnyInputParser "value")
      []
      (Right [])

    runVariableInputParserTestCase
      "single number argument"
      (Input.variableNumberInputParser "number")
      [ FloatValue 10.0 ]
      (Right [ 10.0 ])

    runVariableInputParserTestCase
      "two number arguments"
      (Input.variableNumberInputParser "number")
      [ FloatValue 10.0, FloatValue 20.0 ]
      (Right [ 10.0, 20.0 ])

    runVariableInputParserTestCase
      "single word argument"
      (Input.variableWordInputParser "word")
      [ WordValue "aaa" ]
      (Right [ "aaa" ])

    runVariableInputParserTestCase
      "two words argument"
      (Input.variableWordInputParser "word")
      [ WordValue "aaa", WordValue "bbb" ]
      (Right [ "aaa", "bbb" ])

    runVariableInputParserTestCase
      "a number follow by a word"
      (Input.variableAnyInputParser "value")
      [ FloatValue 10.0, WordValue "aaa" ]
      (Right [ FloatValue 10.0, WordValue "aaa" ])

parametersFromFixedInputParserTestCase
  ∷ ∀ a
  . String
  → FixedInputParser a
  → Array Parameter
  → Spec Unit
parametersFromFixedInputParserTestCase title parser expected = it
  ("extracts parameters from a fixed input parser \"" <> title <> "\"")
  ( (Input.parametersFromFixedInputParser parser)
      `shouldEqual`
        (FixedParameters expected)
  )

parametersFromVariableInputParserTestCase
  ∷ ∀ a
  . String
  → VariableInputParser a
  → Parameter
  → Spec Unit
parametersFromVariableInputParserTestCase title parser expected = it
  ( "extracts parameters from a variable input parser \"" <> title <>
      "\""
  )
  ( (Input.parametersFromVariableInputParser parser)
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
  → Spec Unit
runFixedInputParserTestCase title parser arguments expected = it
  ("parses fixed input \"" <> title <> "\"")
  ( (Input.runFixedInputParser parser $ List.fromFoldable arguments)
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
  → Spec Unit
runVariableInputParserTestCase title parser arguments expected = it
  ("parses variable input \"" <> title <> "\"")
  ( ( Input.runVariableInputParser parser $ List.fromFoldable
        arguments
    )
      `shouldEqual`
        (List.fromFoldable <$> expected)
  )

