module Test.Spec.MLogo.Printing (spec) where

import Prelude

import Data.Argonaut.Core as A
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode as AE
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List (List(..))
import Data.List as List
import Data.String (Pattern(..))
import Data.String as String
import MLogo.Program.Example (Example(..))
import MLogo.Program.Examples as Examples
import MLogo.Interpretation.Command.Commands as Commands
import MLogo.Parsing as Parsing
import MLogo.Parsing.Expression
  ( BinaryOperationType(..)
  , Expression(..)
  , ParameterName(..)
  , UnaryOperationType(..)
  )
import MLogo.Parsing.Expression.Gen as ExpressionGen
import MLogo.Printing as Printing
import MLogo.Printing.Code (codeToString) as Code
import Parsing (ParseError)
import Parsing as P
import Test.QuickCheck (Result(..))
import Test.QuickCheck.Gen as Gen
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.MLogo.Printing.BinaryOperation as BinaryOperation
import Test.Spec.MLogo.Printing.Code (spec) as Code
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)
import Test.Utils as Utils

spec ∷ TestSpec
spec = describe "Printing" do
  BinaryOperation.spec
  Code.spec

  describe "printExpressions" do

    astBasedPrintTestCase
      "single word negated integer literal"
      [ UnaryOperation Negation (IntegerLiteral 1) ]
      100
      (String.joinWith "\n" [ "-1" ])

    astBasedPrintTestCase
      "single word string literal"
      [ StringLiteral "abc" ]
      100
      (String.joinWith "\n" [ "\"abc" ])

    astBasedPrintTestCase
      "integers in a single line"
      [ IntegerLiteral 1
      , IntegerLiteral 2
      , IntegerLiteral 3
      , IntegerLiteral 4
      ]
      100
      (String.joinWith "\n" [ "1 2 3 4" ])

    astBasedPrintTestCase
      "integers over multiple lines"
      [ IntegerLiteral 1
      , IntegerLiteral 2
      , IntegerLiteral 3
      , IntegerLiteral 4
      ]
      5
      (String.joinWith "\n" [ "1", "2", "3", "4" ])

    astBasedPrintTestCase
      "a chain of addition in a single line"
      [ BinaryOperation
          Addition
          ( BinaryOperation
              Addition
              ( BinaryOperation
                  Addition
                  (IntegerLiteral 1)
                  (IntegerLiteral 2)
              )
              (IntegerLiteral 3)
          )
          (IntegerLiteral 4)
      ]
      100
      (String.joinWith "\n" [ "1 + 2 + 3 + 4" ])

    astBasedPrintTestCase
      "a chain of addition over multiple lines"
      [ BinaryOperation
          Addition
          ( BinaryOperation
              Addition
              ( BinaryOperation
                  Addition
                  (IntegerLiteral 1)
                  (IntegerLiteral 2)
              )
              (IntegerLiteral 3)
          )
          (IntegerLiteral 4)
      ]
      10
      ( String.joinWith "\n"
          [ "1"
          , "+"
          , "2"
          , "+"
          , "3"
          , "+"
          , "4"
          ]
      )

    astBasedPrintTestCase
      "a chain of subtractions in a single line"
      [ BinaryOperation
          Subtraction
          ( BinaryOperation
              Subtraction
              ( BinaryOperation
                  Subtraction
                  (IntegerLiteral 1)
                  (IntegerLiteral 2)
              )
              (IntegerLiteral 3)
          )
          (IntegerLiteral 4)
      ]
      100
      (String.joinWith "\n" [ "1 - 2 - 3 - 4" ])

    {- TODO: implement

    astBasedPrintTestCase
      "a chain of subtractions over multiple lines"
      [ BinaryOperation
          Subtraction
          ( BinaryOperation
              Subtraction
              ( BinaryOperation
                  Subtraction
                  (IntegerLiteral 1)
                  (IntegerLiteral 2)
              )
              (IntegerLiteral 3)
          )
          (IntegerLiteral 4)
      ]
      10
      ( String.joinWith "\n"
          [ "1"
          , "-"
          , "2"
          , "-"
          , "3"
          , "-"
          , "4"
          ]
      )

-}

    astBasedPrintTestCase
      "procedure call without arguments"
      [ ProcedureCall "proc1" Nil ]
      100
      (String.joinWith "\n" [ "proc1" ])

    astBasedPrintTestCase
      "procedure call in one line"
      [ ProcedureCall "proc1"
          ( List.fromFoldable
              [ IntegerLiteral 1
              , IntegerLiteral 2
              , IntegerLiteral 3
              , IntegerLiteral 4
              ]
          )
      ]
      100
      (String.joinWith "\n" [ "proc1 1 2 3 4" ])

    astBasedPrintTestCase
      "procedure call over multiple lines"
      [ ProcedureCall "proc1"
          ( List.fromFoldable
              [ IntegerLiteral 1
              , IntegerLiteral 2
              , IntegerLiteral 3
              , IntegerLiteral 4
              ]
          )
      ]
      10
      ( String.joinWith "\n"
          [ "proc1", "  1", "  2", "  3", "  4" ]
      )

    astBasedPrintTestCase
      "procedure calls in one line"
      [ ProcedureCall "proc1"
          ( List.fromFoldable
              [ IntegerLiteral 1
              , IntegerLiteral 2
              , IntegerLiteral 3
              , IntegerLiteral 4
              ]
          )
      , ProcedureCall "proc2"
          ( List.fromFoldable
              [ IntegerLiteral 5
              , IntegerLiteral 6
              , IntegerLiteral 7
              , IntegerLiteral 8
              ]
          )
      ]
      100
      (String.joinWith "\n" [ "proc1 1 2 3 4 proc2 5 6 7 8" ])

    astBasedPrintTestCase
      "procedure calls over multiple lines"
      [ ProcedureCall "proc1"
          ( List.fromFoldable
              [ IntegerLiteral 1
              , IntegerLiteral 2
              , IntegerLiteral 3
              , IntegerLiteral 4
              ]
          )
      , ProcedureCall "proc2"
          ( List.fromFoldable
              [ IntegerLiteral 5
              , IntegerLiteral 6
              , IntegerLiteral 7
              , IntegerLiteral 8
              ]
          )
      ]
      20
      ( String.joinWith "\n"
          [ "proc1 1 2 3 4"
          , "proc2 5 6 7 8"
          ]
      )

    astBasedPrintTestCase
      "procedure calls over multiple lines"
      [ ProcedureCall "proc1"
          ( List.fromFoldable
              [ IntegerLiteral 1
              , IntegerLiteral 2
              , IntegerLiteral 3
              , IntegerLiteral 4
              ]
          )
      , ProcedureCall "proc2"
          ( List.fromFoldable
              [ IntegerLiteral 5
              , IntegerLiteral 6
              , IntegerLiteral 7
              , IntegerLiteral 8
              ]
          )
      ]
      10
      ( String.joinWith "\n"
          [ "proc1"
          , "  1"
          , "  2"
          , "  3"
          , "  4"
          , "proc2"
          , "  5"
          , "  6"
          , "  7"
          , "  8"
          ]
      )

    astBasedPrintTestCase
      "a zero-argument procedure call as an operand"
      [ BinaryOperation
          Addition
          (ProcedureCall "proc1" Nil)
          (IntegerLiteral 3)
      ]
      100
      (String.joinWith "\n" [ "proc1 + 3" ])

    astBasedPrintTestCase
      "a one-argument procedure call as an operand"
      [ BinaryOperation
          Addition
          ( ProcedureCall "proc1"
              ( List.fromFoldable
                  [ IntegerLiteral 1
                  ]
              )
          )
          (IntegerLiteral 2)
      ]
      100
      (String.joinWith "\n" [ "( proc1 1 ) + 2" ])

    astBasedPrintTestCase
      "a two-argument procedure call as an operand"
      [ BinaryOperation
          Addition
          ( ProcedureCall "proc1"
              ( List.fromFoldable
                  [ IntegerLiteral 1
                  , IntegerLiteral 2
                  ]
              )
          )
          (IntegerLiteral 3)
      ]
      100
      (String.joinWith "\n" [ "( proc1 1 2 ) + 3" ])

    astBasedPrintTestCase
      "an empty for block"
      [ ForBlock
          { binder: "i", initialValue: 1, step: 2, terminalValue: 5 }
          Nil
      ]
      100
      (String.joinWith "\n" [ "for [ i 1 5 2 ] []" ])

    astBasedPrintTestCase
      "an empty for block with a step of 1"
      [ ForBlock
          { binder: "i", initialValue: 1, step: 1, terminalValue: 5 }
          Nil
      ]
      100
      (String.joinWith "\n" [ "for [ i 1 5 ] []" ])

    astBasedPrintTestCase
      "a single line for block"
      [ ForBlock
          { binder: "i", initialValue: 1, step: 2, terminalValue: 5 }
          ( List.fromFoldable
              [ ProcedureCall "proc1" Nil
              , ProcedureCall "proc2" Nil
              , ProcedureCall "proc3" Nil
              ]
          )
      ]
      100
      (String.joinWith "\n" [ "for [ i 1 5 2 ] [ proc1 proc2 proc3 ]" ])

    astBasedPrintTestCase
      "a multi line for block, single line body"
      [ ForBlock
          { binder: "i", initialValue: 1, step: 2, terminalValue: 5 }
          ( List.fromFoldable
              [ ProcedureCall "proc1" Nil
              , ProcedureCall "proc2" Nil
              , ProcedureCall "proc3" Nil
              ]
          )
      ]
      30
      ( String.joinWith "\n"
          [ "for [ i 1 5 2 ]"
          , "  [ proc1 proc2 proc3 ]"
          ]
      )

    astBasedPrintTestCase
      "a multi line for block, multi line body"
      [ ForBlock
          { binder: "i", initialValue: 1, step: 2, terminalValue: 5 }
          ( List.fromFoldable
              [ ProcedureCall "proc1" Nil
              , ProcedureCall "proc2" Nil
              , ProcedureCall "proc3" Nil
              ]
          )
      ]
      20
      ( String.joinWith "\n"
          [ "for [ i 1 5 2 ]"
          , "  ["
          , "    proc1"
          , "    proc2"
          , "    proc3"
          , "  ]"
          ]
      )

    astBasedPrintTestCase
      "a multi line for block, multi line body and spec"
      [ ForBlock
          { binder: "i", initialValue: 1, step: 2, terminalValue: 5 }
          ( List.fromFoldable
              [ ProcedureCall "proc1" Nil
              , ProcedureCall "proc2" Nil
              , ProcedureCall "proc3" Nil
              ]
          )
      ]
      10
      ( String.joinWith "\n"
          [ "for"
          , "  [ i 1 5 2 ]"
          , "  ["
          , "    proc1"
          , "    proc2"
          , "    proc3"
          , "  ]"
          ]
      )

    astBasedPrintTestCase
      "an empty repeat block"
      [ RepeatBlock (IntegerLiteral 1) Nil ]
      100
      (String.joinWith "\n" [ "repeat 1 []" ])

    astBasedPrintTestCase
      "a short repeat block"
      [ RepeatBlock
          (IntegerLiteral 1)
          ( List.fromFoldable
              [ ProcedureCall "proc1" Nil, ProcedureCall "proc2" Nil ]
          )
      ]
      100
      (String.joinWith "\n" [ "repeat 1 [ proc1 proc2 ]" ])

    astBasedPrintTestCase
      "splitting repeat block over two lines"
      [ RepeatBlock
          (IntegerLiteral 1)
          ( List.fromFoldable
              [ ProcedureCall "proc1" Nil, ProcedureCall "proc2" Nil ]
          )
      ]
      20
      ( String.joinWith "\n"
          [ "repeat 1"
          , "  [ proc1 proc2 ]"
          ]
      )

    astBasedPrintTestCase
      "splitting repeat block over multiple lines"
      [ RepeatBlock
          (IntegerLiteral 1)
          ( List.fromFoldable
              [ ProcedureCall "proc1" Nil, ProcedureCall "proc2" Nil ]
          )
      ]
      10
      ( String.joinWith "\n"
          [ "repeat 1"
          , "  ["
          , "    proc1"
          , "    proc2"
          , "  ]"
          ]
      )

    astBasedPrintTestCase
      "an empty if block"
      [ IfBlock (BooleanLiteral true) Nil ]
      100
      (String.joinWith "\n" [ "if true []" ])

    astBasedPrintTestCase
      "a short if block"
      [ IfBlock
          (BooleanLiteral true)
          ( List.fromFoldable
              [ ProcedureCall "proc1" Nil, ProcedureCall "proc2" Nil ]
          )
      ]
      100
      (String.joinWith "\n" [ "if true [ proc1 proc2 ]" ])

    astBasedPrintTestCase
      "a long if block"
      [ IfBlock
          (BooleanLiteral true)
          ( List.fromFoldable
              [ ProcedureCall "proc1" Nil, ProcedureCall "proc2" Nil ]
          )
      ]
      20
      (String.joinWith "\n" [ "if true", "  [ proc1 proc2 ]" ])

    astBasedPrintTestCase
      "an empty ifelse block"
      [ IfElseBlock (BooleanLiteral true) Nil Nil ]
      100
      (String.joinWith "\n" [ "ifelse true [] []" ])

    astBasedPrintTestCase
      "a short ifelse block"
      [ IfElseBlock
          (BooleanLiteral true)
          ( List.fromFoldable
              [ ProcedureCall "proc1" Nil, ProcedureCall "proc2" Nil ]
          )
          ( List.fromFoldable
              [ ProcedureCall "proc3" Nil, ProcedureCall "proc4" Nil ]
          )
      ]
      100
      ( String.joinWith "\n"
          [ "ifelse true [ proc1 proc2 ] [ proc3 proc4 ]" ]
      )

    astBasedPrintTestCase
      "a long ifelse block"
      [ IfElseBlock
          (BooleanLiteral true)
          ( List.fromFoldable
              [ ProcedureCall "proc1" Nil, ProcedureCall "proc2" Nil ]
          )
          ( List.fromFoldable
              [ ProcedureCall "proc3" Nil, ProcedureCall "proc4" Nil ]
          )
      ]
      20
      ( String.joinWith "\n"
          [ "ifelse true", "  [ proc1 proc2 ]", "  [ proc3 proc4 ]" ]
      )

    astBasedPrintTestCase
      "a procedure definition with a single line body"
      [ ProcedureDefinition
          { name: "proc1"
          , parameterNames: List.fromFoldable
              [ ParameterName "param1", ParameterName "param2" ]
          }
          ( List.fromFoldable
              [ ProcedureCall "proc2" Nil
              , ProcedureCall "proc3" Nil
              , ProcedureCall "proc4" Nil
              , ProcedureCall "proc5" Nil
              , ProcedureCall "proc6" Nil
              ]
          )
      ]
      100
      ( String.joinWith "\n"
          [ "to proc1 :param1 :param2"
          , "proc2 proc3 proc4 proc5 proc6"
          , "end"
          ]
      )

    astBasedPrintTestCase
      "a procedure definition with a multi line body"
      [ ProcedureDefinition
          { name: "proc1"
          , parameterNames: List.fromFoldable
              [ ParameterName "param1", ParameterName "param2" ]
          }
          ( List.fromFoldable
              [ ProcedureCall "proc2" Nil
              , ProcedureCall "proc3" Nil
              , ProcedureCall "proc4" Nil
              , ProcedureCall "proc5" Nil
              , ProcedureCall "proc6" Nil
              ]
          )
      ]
      25
      ( String.joinWith "\n"
          [ "to proc1 :param1 :param2"
          , "proc2"
          , "proc3"
          , "proc4"
          , "proc5"
          , "proc6"
          , "end"
          ]
      )

    astBasedPrintTestCase
      "a single line variable assignment"
      [ VariableAssignment "var1" (IntegerLiteral 1) ]
      100
      (String.joinWith "\n" [ "make \"var1 1" ])

    printTestCase
      "integer literal"
      [ IntegerLiteral 1 ]

    printTestCase
      "a negated integer literal"
      [ UnaryOperation Negation (IntegerLiteral 1) ]

    printTestCase
      "float literal"
      [ FloatLiteral 1.0 ]

    printTestCase
      "operations out of precedence order"
      [ BinaryOperation
          Multiplication
          ( BinaryOperation
              Addition
              (IntegerLiteral 1)
              (IntegerLiteral 2)
          )
          (IntegerLiteral 3)
      ]

    printTestCase
      "negating an ifelse expression"
      [ UnaryOperation
          Negation
          ( IfElseBlock
              (BooleanLiteral true)
              (List.fromFoldable [ IntegerLiteral 1 ])
              (List.fromFoldable [ IntegerLiteral 2 ])
          )
      ]

    printTestCase
      "subtracting two ifelse expressions"
      [ BinaryOperation
          Subtraction
          ( IfElseBlock
              (BooleanLiteral true)
              (List.fromFoldable [ IntegerLiteral 1 ])
              (List.fromFoldable [ IntegerLiteral 2 ])
          )
          ( IfElseBlock
              (BooleanLiteral true)
              (List.fromFoldable [ IntegerLiteral 3 ])
              (List.fromFoldable [ IntegerLiteral 4 ])
          )
      ]

    printTestCase
      "non-commutative operation chain"
      [ BinaryOperation
          Subtraction
          ( BinaryOperation
              Subtraction
              (IntegerLiteral 1)
              (IntegerLiteral 2)
          )
          ( BinaryOperation
              Subtraction
              (IntegerLiteral 3)
              (IntegerLiteral 4)
          )
      ]

    printTestCase
      "same precedence in the right operand, left associativity"
      [ BinaryOperation
          Equation
          ( BinaryOperation
              Multiplication
              (ValueReference "var1")
              (ValueReference "var2")
          )
          ( BinaryOperation
              Equation
              (BooleanLiteral false)
              (StringLiteral "s1")
          )
      ]

    forWithIndex_
      Examples.all
      \title (Example { ast, source }) →
        sourceBasedPrintTestCase title source ast

    generativeTestCase
      Long
      "parses back a printed source of a random AST"
      do
        ast ← Gen.arrayOf $ ExpressionGen.genExpression

        let
          printedSource ∷ String
          printedSource = Code.codeToString
            $ Printing.printExpressions
                ast
                { pageWidth: maxLineLength
                , simplifyBinaryOperations: false
                }

          parsingResult ∷ ParseError \/ List Expression
          parsingResult = P.runParser
            printedSource
            (Parsing.expressions Commands.parsingContext)

        pure case parsingResult of
          Left parseError →
            Failed $ "--- error >>> ---\n"
              <> show parseError
              <> "\n--- AST >>> ---\n"
              <> A.stringify (AE.encodeJson ast)
              <> "\n--- <<< AST ---"
              <> "\n--- printed source >>> ---\n"
              <> Utils.emphasizeWhitespaces printedSource
              <> "\n--- <<< printed source ---"
              <> "\n--- <<< error ---"
          Right actual →
            let
              expected ∷ List Expression
              expected = List.fromFoldable ast

            in
              if actual == expected then Success
              else
                Failed $ "--- error >>> ---\n"
                  <> (A.stringify $ AE.encodeJson actual)
                  <> "\nis not equal to\n"
                  <> (A.stringify $ AE.encodeJson expected)
                  <> "\n--- printed source >>> ---\n"
                  <> Utils.emphasizeWhitespaces printedSource
                  <> "\n--- <<< printed source ---"
                  <> "\n--- <<< error ---"

    generativeTestCase
      Long
      ( "does not produce source code lines wider than "
          <> show maxLineLength
          <> " characters"
      )
      do
        ast ← Gen.arrayOf $ ExpressionGen.genExpression

        let
          {- TODO: remove relaxation once formatting is improved -}
          maxLineLengthWithGrace ∷ Int
          maxLineLengthWithGrace = maxLineLength * 3

          printedSource ∷ String
          printedSource = Code.codeToString
            $ Printing.printExpressions
                ast
                { pageWidth: maxLineLengthWithGrace
                , simplifyBinaryOperations: true
                }

          printedSourceLines ∷ Array String
          printedSourceLines = String.split
            (Pattern "\n")
            printedSource

          isTooLong ∷ String → Boolean
          isTooLong = (_ > maxLineLengthWithGrace) <<< String.length

        pure
          if Array.any isTooLong printedSourceLines then
            Failed $ "--- error >>> ---\n"
              <> "There is at least one line longer than "
              <> show maxLineLengthWithGrace
              <> " characters"
              <> "\n--- printed source >>> ---\n"
              <> Utils.emphasizeWhitespaces printedSource
              <> "\n--- <<< printed source ---"
              <> "\n--- <<< error ---"
          else Success

astBasedPrintTestCase
  ∷ ∀ f
  . EncodeJson (f Expression)
  ⇒ Foldable f
  ⇒ Functor f
  ⇒ Show (f Expression)
  ⇒ String
  → f Expression
  → Int
  → String
  → TestSpec
astBasedPrintTestCase title ast pageWidth expected =
  it
    ( "prints \"" <> title <> "\" for a page of width " <> show
        pageWidth
    )
    do
      let
        actual ∷ String
        actual = Code.codeToString
          $ Printing.printExpressions
              ast
              { pageWidth, simplifyBinaryOperations: true }

      if actual == expected then pure unit
      else
        fail $ "--- error >>> ---\n"
          <> Utils.emphasizeWhitespaces actual
          <> "\nis not equal to\n"
          <> Utils.emphasizeWhitespaces expected
          <> "\n--- AST >>> ---\n"
          <> A.stringify (AE.encodeJson ast)
          <> "\n--- <<< AST ---"
          <> "\n--- <<< error ---"

sourceBasedPrintTestCase
  ∷ ∀ f
  . Foldable f
  ⇒ Functor f
  ⇒ Show (f Expression)
  ⇒ String
  → String
  → f Expression
  → TestSpec
sourceBasedPrintTestCase title source ast =
  it ("parses back a printed source of \"" <> title <> "\"") do

    let
      printedSource ∷ String
      printedSource = Code.codeToString
        $ Printing.printExpressions
            ast
            { pageWidth: maxLineLength
            , simplifyBinaryOperations: false
            }

      parsingResult ∷ ParseError \/ List Expression
      parsingResult = P.runParser
        printedSource
        (Parsing.expressions Commands.parsingContext)

      actual ∷ String \/ List Expression
      actual = case parsingResult of
        Left parseError →
          Left $ show parseError
        Right expressions →
          Right expressions

      expected ∷ String \/ List Expression
      expected = Right $ List.fromFoldable ast

    if actual == expected then pure unit
    else
      fail $ "--- error >>> ---\n"
        <> show actual
        <> "\nis not equal to\n"
        <> show expected
        <> "\n--- original source >>> ---\n"
        <> Utils.emphasizeWhitespaces source
        <> "\n--- <<< original source ---"
        <> "\n--- printed source >>> ---\n"
        <> Utils.emphasizeWhitespaces printedSource
        <> "\n--- <<< printed source ---"
        <> "\n--- <<< error ---"

printTestCase
  ∷ ∀ f
  . Foldable f
  ⇒ Functor f
  ⇒ Show (f Expression)
  ⇒ String
  → f Expression
  → TestSpec
printTestCase title ast =
  it ("parses back a printed source of \"" <> title <> "\"") do

    let
      printedSource ∷ String
      printedSource = Code.codeToString
        $ Printing.printExpressions
            ast
            { pageWidth: 10, simplifyBinaryOperations: false }

      parsingResult ∷ ParseError \/ List Expression
      parsingResult = P.runParser
        printedSource
        (Parsing.expressions Commands.parsingContext)

      actual ∷ String \/ List Expression
      actual = case parsingResult of
        Left parseError →
          Left $ show parseError
        Right expressions →
          Right expressions

      expected ∷ String \/ List Expression
      expected = Right $ List.fromFoldable ast

    if actual == expected then pure unit
    else
      fail $ "--- error >>> ---\n"
        <> show actual
        <> "\nis not equal to\n"
        <> show expected
        <> "\n--- printed source >>> ---\n"
        <> Utils.emphasizeWhitespaces printedSource
        <> "\n--- <<< printed source ---"
        <> "\n--- <<< error ---"

maxLineLength ∷ Int
maxLineLength = 60
