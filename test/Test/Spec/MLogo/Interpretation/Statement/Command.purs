module Test.Spec.MLogo.Interpretation.Statement.Command (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List as List
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.Statement.Command (InputParser, Parameter, ValueType(..))
import MLogo.Interpretation.Statement.Command as Command
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Command" do

  describe "inputParserInfo" do

    inputParserInfoTestCase
      "no arguments"
      (pure unit)
      []

    inputParserInfoTestCase
      "single number argument"
      ( ado
          number ← Command.numberInputParser "number"
          in { number }
      )
      [ { name: "number", valueType: NumberType } ]

    inputParserInfoTestCase
      "single word argument"
      ( ado
          word ← Command.wordInputParser "word"
          in { word }
      )
      [ { name: "word", valueType: WordType } ]

    inputParserInfoTestCase
      "two number arguments"
      ( ado
          number1 ← Command.numberInputParser "number1"
          number2 ← Command.numberInputParser "number2"
          in { number1, number2 }
      )
      [ { name: "number1", valueType: NumberType }
      , { name: "number2", valueType: NumberType }
      ]

    inputParserInfoTestCase
      "three number arguments"
      ( ado
          number1 ← Command.numberInputParser "number1"
          number2 ← Command.numberInputParser "number2"
          number3 ← Command.numberInputParser "number3"
          in { number1, number2, number3 }
      )
      [ { name: "number1", valueType: NumberType }
      , { name: "number2", valueType: NumberType }
      , { name: "number3", valueType: NumberType }
      ]

  describe "runInputParser" do

    runInputParserTestCase
      "no arguments"
      (pure unit)
      []
      (Right unit)

    runInputParserTestCase
      "single number argument"
      ( ado
          number ← Command.numberInputParser "number"
          in { number }
      )
      [ NumberValue 10.0 ]
      (Right $ { number: 10.0 })

    runInputParserTestCase
      "single word argument"
      ( ado
          word ← Command.wordInputParser "word"
          in { word }
      )
      [ WordValue "aaa" ]
      (Right $ { word: "aaa" })

    runInputParserTestCase
      "two number arguments"
      ( ado
          number1 ← Command.numberInputParser "number1"
          number2 ← Command.numberInputParser "number2"
          in { number1, number2 }
      )
      [ NumberValue 10.0, NumberValue 20.0 ]
      (Right $ { number1: 10.0, number2: 20.0 })

    runInputParserTestCase
      "three number arguments"
      ( ado
          number1 ← Command.numberInputParser "number1"
          number2 ← Command.numberInputParser "number2"
          number3 ← Command.numberInputParser "number3"
          in { number1, number2, number3 }
      )
      [ NumberValue 10.0, NumberValue 20.0, NumberValue 30.0 ]
      (Right $ { number1: 10.0, number2: 20.0, number3: 30.0 })

inputParserInfoTestCase
  ∷ ∀ a
  . String
  → InputParser a
  → Array Parameter
  → Spec Unit
inputParserInfoTestCase title parser expected = it
  ("produces input parser info \"" <> title <> "\"")
  ( (Command.inputParserInfo parser)
      `shouldEqual`
        (List.fromFoldable expected)
  )

runInputParserTestCase
  ∷ ∀ a
  . Eq a
  ⇒ Show a
  ⇒ String
  → InputParser a
  → Array Value
  → String \/ a
  → Spec Unit
runInputParserTestCase title parser arguments expected = it
  ("parses input \"" <> title <> "\"")
  ( (Command.runInputParser parser $ List.fromFoldable arguments)
      `shouldEqual`
        expected
  )

