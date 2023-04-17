module Test.Spec.MLogo.Parsing (spec) where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Char.Gen as CharGen
import Data.Either (Either(..), note)
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable)
import Data.List (List(..))
import Data.List as List
import Data.Number as Number
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Traversable (class Traversable, sequence)
import Effect.Class (liftEffect)
import MLogo.Parsing (Expression(..), Parameter(..))
import MLogo.Parsing as Parsing
import Parsing as P
import Test.QuickCheck (Result(..), quickCheckGen)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec, describe, it)
import Test.Utils as Utils

spec ∷ Spec Unit
spec = describe "Parsing" do
  describe "expression" do
    testCase
      "a boolean literal"
      [ genBoolean ]
      ( \parts → do
          booleanString ← note
            "can't parse the boolean back"
            (parts !! 0)
          boolean ← case booleanString of
            "false" →
              Right false
            "true" →
              Right true
            other →
              Left $ "\"" <> other <> "\" is not a boolean value"
          pure $ BooleanLiteral boolean
      )

    arithmeticalBinaryOperatorTestCase
      "="
      Equation

    arithmeticalBinaryOperatorTestCase
      "+"
      Addition

    arithmeticalBinaryOperatorTestCase
      "*"
      Multiplication

    testCase
      "a value reference"
      [ genParameter ]
      ( \parts → ado
          name ← String.drop 1 <$> note
            "can't parse the reference name back"
            (parts !! 0)
          in
            ValueReference name
      )

    testCase
      "a variable assignment"
      [ pure "make", pure " ", genString, pure " ", genBoolean ]
      ( \parts → do
          name ← String.drop 1 <$> note
            "can't parse the variable name back"
            (parts !! 2)
          booleanString ← note
            "can't parse the boolean back"
            (parts !! 4)
          boolean ← case booleanString of
            "false" →
              Right false
            "true" →
              Right true
            other →
              Left $ "\"" <> other <> "\" is not a boolean value"
          pure $ VariableAssignment name (BooleanLiteral boolean)
      )

    testCase
      "a procedure call"
      [ genProcedureName, pure " ", genFloat, pure " ", genFloat ]
      ( \parts → ado
          procedureName ← note
            "can't parse the procedure name back"
            (parts !! 0)
          firstArgument ← note
            "can't parse the first argument back"
            (Number.fromString =<< parts !! 2)
          secondArgument ← note
            "can't parse the second argument back"
            (Number.fromString =<< parts !! 4)
          in
            ProcedureCall
              procedureName
              ( List.fromFoldable
                  [ FloatLiteral firstArgument
                  , FloatLiteral secondArgument
                  ]
              )
      )

    testCase
      "a procedure definition"
      [ pure "to"
      , pure " "
      , genProcedureName
      , pure " "
      , Utils.addRedundantSpaces
          =<< Array.intersperse " " <$> Gen.arrayOf genParameter
      , pure "\n"
      , pure "proc1"
      , pure " "
      , pure "1"
      , pure "\n"
      , pure "proc2"
      , pure " "
      , pure "2"
      , pure "\n"
      , pure "end"
      ]
      ( \parts → do
          procedureName ← note
            "can't parse the procedure name back"
            (parts !! 2)

          whitespacesRegex ← Regex.regex "\\s+" RegexFlags.global

          parameters ← Array.filter (_ /= "")
            <$> Regex.split whitespacesRegex
            <$> String.trim
            <$> note "can't parse parameters back" (parts !! 4)

          pure $ ProcedureDefinition
            procedureName
            ( List.fromFoldable
                $ Parameter <$> String.drop 1 <$> parameters
            )
            ( List.fromFoldable
                [ ProcedureCall "proc1"
                    (List.fromFoldable [ IntegerLiteral 1 ])
                , ProcedureCall "proc2"
                    (List.fromFoldable [ IntegerLiteral 2 ])
                ]
            )
      )

    testCase
      "an if block"
      [ pure "if"
      , pure " "
      , pure "true"
      , pure " "
      , pure "["
      , pure "proc1"
      , pure " "
      , pure "1"
      , pure " "
      , pure "proc2"
      , pure " "
      , pure "2"
      , pure "]"
      ]
      ( \parts → pure $ IfBlock (BooleanLiteral true)
          ( List.fromFoldable
              [ ProcedureCall "proc1"
                  (List.fromFoldable [ IntegerLiteral 1 ])
              , ProcedureCall "proc2"
                  (List.fromFoldable [ IntegerLiteral 2 ])
              ]
          )
      )

    testCase
      "an if block with a integer equation"
      [ pure "if"
      , pure " "
      , pure "("
      , pure "1"
      , pure "="
      , pure "2"
      , pure ")"
      , pure " "
      , pure "["
      , pure "proc1"
      , pure " "
      , pure "1"
      , pure " "
      , pure "proc2"
      , pure " "
      , pure "2"
      , pure "]"
      ]
      ( \parts → pure $ IfBlock
          (Equation (IntegerLiteral 1) (IntegerLiteral 2))
          ( List.fromFoldable
              [ ProcedureCall "proc1"
                  (List.fromFoldable [ IntegerLiteral 1 ])
              , ProcedureCall "proc2"
                  (List.fromFoldable [ IntegerLiteral 2 ])
              ]
          )
      )

    testCase
      "a repeat block"
      [ pure "repeat"
      , pure " "
      , pure "4"
      , pure " "
      , pure "["
      , pure "proc1"
      , pure " "
      , pure "1"
      , pure " "
      , pure "proc2"
      , pure " "
      , pure "2"
      , pure "]"
      ]
      ( \parts → pure $ RepeatBlock (IntegerLiteral 4)
          ( List.fromFoldable
              [ ProcedureCall "proc1"
                  (List.fromFoldable [ IntegerLiteral 1 ])
              , ProcedureCall "proc2"
                  (List.fromFoldable [ IntegerLiteral 2 ])
              ]
          )
      )

arithmeticalBinaryOperatorTestCase
  ∷ String
  → (Expression → Expression → Expression)
  → Spec Unit
arithmeticalBinaryOperatorTestCase symbol expected =
  testCase
    ("\"" <> symbol <> "\" symbol")
    [ genFloat, pure symbol, genFloat ]
    ( \parts → ado
        leftOperand ← note
          "can't parse left operand back"
          (Number.fromString =<< parts !! 0)
        rightOperand ← note
          "can't parse right operand back"
          (Number.fromString =<< parts !! 2)
        in
          expected
            (FloatLiteral leftOperand)
            (FloatLiteral rightOperand)
    )

genBoolean ∷ Gen String
genBoolean = do
  n ← Gen.chooseInt 0 1
  pure $ if n == 0 then "false" else "true"

genFloat ∷ Gen String
genFloat = do
  n1 ← Gen.chooseInt 0 9
  n2 ← Gen.chooseInt 0 9
  pure $ show n1 <> "." <> show n2

genString ∷ Gen String
genString = do
  identifier ← genIdentifier
  pure $ "\"" <> identifier

genParameter ∷ Gen String
genParameter = do
  identifier ← genIdentifier
  pure $ ":" <> identifier

genProcedureName ∷ Gen String
genProcedureName = genIdentifier

{- TODO: exclude reserved words -}
genIdentifier ∷ Gen String
genIdentifier = do
  chars ← Gen.arrayOf1 CharGen.genAlpha
  pure
    $ String.fromCodePointArray
    $ String.codePointFromChar <$> Array.fromFoldable chars

testCase
  ∷ ∀ f
  . Foldable f
  ⇒ Traversable f
  ⇒ String
  → f (Gen String)
  → (Array String → String \/ Expression)
  → Spec Unit
testCase title partGenerators makeExpected = it title do
  liftEffect $ quickCheckGen do
    parts ← sequence partGenerators
    source ← Utils.addRedundantParentheses
      =<< Utils.addRedundantSpaces parts

    let
      actual = case P.runParser source Parsing.expression of
        Left parseError →
          Left $ show parseError
        Right expression →
          Right $ expression

      expected = makeExpected $ Array.fromFoldable parts

    pure $
      if actual == expected then Success
      else Failed $
        "--- error >>> ---\n"
          <> show actual
          <> "\nis not equal to\n"
          <> show expected
          <> "\n--- source >>> ---\n"
          <> Utils.emphasizeWhitespaces source
          <> "\n--- <<< source ---"
          <> "\n--- <<< error ---"

