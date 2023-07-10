module Test.Spec.MLogo.Parsing (spec) where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Int as Int
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Traversable (class Traversable, sequence)
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command.Commands as Commands
import MLogo.Parsing (ParsingContext)
import MLogo.Parsing as Parsing
import MLogo.Parsing.Expression
  ( BinaryOperationType(..)
  , Expression(..)
  , ParameterName(..)
  , ProcedureSignature
  , UnaryOperationType(..)
  )
import MLogo.Parsing.Expression as Expression
import MLogo.Parsing.Expression.Gen as ExpressionGen
import MLogo.Program.Example (Example(..))
import MLogo.Program.Examples as Examples
import Parsing as P
import Test.QuickCheck (Result(..))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.MLogo.Parsing.Operator as Operator
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)
import Test.Utils as Utils

spec ∷ TestSpec
spec = describe "Parsing" do
  Operator.spec

  describe "expression" do
    arithmeticalBinaryOperatorTestCase
      Equation

    arithmeticalBinaryOperatorTestCase
      Addition

    arithmeticalBinaryOperatorTestCase
      Division

    arithmeticalBinaryOperatorTestCase
      Exponentiation

    arithmeticalBinaryOperatorTestCase
      Multiplication

    expressionTestCase
      ("\"-\" symbol")
      [ genFloat, pure "-", pure " ", genFloat ]
      ( \parts → ado
          leftOperand ← Either.note
            "can't parse the left operand back"
            (Number.fromString =<< parts !! 0)
          rightOperand ← Either.note
            "can't parse the right operand back"
            (Number.fromString =<< parts !! 3)
          in
            { context: Map.empty
            , expected: BinaryOperation
                Subtraction
                (floatToExpression leftOperand)
                (floatToExpression rightOperand)
            }
      )

    expressionTestCase
      "adding a negative integer to an integer"
      [ genInteger, pure "+ -1" ]
      ( \parts → ado
          firstInteger ← parseBackInteger parts "first integer" 0
          in
            { context: Map.empty
            , expected: BinaryOperation
                Addition
                (IntegerLiteral firstInteger)
                (UnaryOperation Negation (IntegerLiteral 1))
            }
      )

    expressionTestCase
      "non-whitespace separated negative integer on the right side of an equation"
      [ genInteger, pure "=-1" ]
      ( \parts → ado
          firstInteger ← parseBackInteger parts "first integer" 0
          in
            { context: Map.empty
            , expected: BinaryOperation
                Equation
                (intToExpression firstInteger)
                (intToExpression (-1))
            }
      )

    expressionTestCase
      "multiplication and addition precedence"
      [ genFloat, pure "+", genFloat, pure "*", genFloat ]
      ( \parts → ado
          firstFloat ← parseBackFloat parts "first float" 0
          secondFloat ← parseBackFloat parts "second float" 2
          thirdFloat ← parseBackFloat parts "third float" 4
          in
            { context: Map.empty
            , expected: BinaryOperation
                Addition
                (floatToExpression firstFloat)
                ( BinaryOperation
                    Multiplication
                    (floatToExpression secondFloat)
                    (floatToExpression thirdFloat)
                )
            }
      )

    expressionTestCase
      "multiplication and division precedence"
      [ genFloat, pure "*", genFloat, pure "/", genFloat ]
      ( \parts → ado
          firstFloat ← parseBackFloat parts "first float" 0
          secondFloat ← parseBackFloat parts "second float" 2
          thirdFloat ← parseBackFloat parts "third float" 4
          in
            { context: Map.empty
            , expected: BinaryOperation
                Division
                ( BinaryOperation
                    Multiplication
                    (floatToExpression firstFloat)
                    (floatToExpression secondFloat)
                )
                (floatToExpression thirdFloat)

            }
      )

    expressionTestCase
      "a procedure call and a chain of binary operations precedence"
      [ genProcedureName
      , pure " "
      , genFloat
      , pure "*"
      , genFloat
      , pure "/"
      , genFloat
      ]
      ( \parts → ado
          procedureName ← parseBackIdentifier parts "procedure name" 0
          firstFloat ← parseBackFloat parts "first argument" 2
          secondFloat ← parseBackFloat parts "second argument" 4
          thirdFloat ← parseBackFloat parts "second argument" 6
          in
            { context: Map.fromFoldable [ procedureName /\ Just 1 ]
            , expected:
                ProcedureCall
                  procedureName
                  ( List.fromFoldable
                      [ BinaryOperation
                          Division
                          ( BinaryOperation
                              Multiplication
                              (floatToExpression firstFloat)
                              (floatToExpression secondFloat)
                          )
                          (floatToExpression thirdFloat)
                      ]
                  )
            }
      )

    expressionTestCase
      "a boolean literal"
      [ genBoolean ]
      ( \parts → ado
          boolean ← parseBackBoolean parts "boolean" 0
          in { context: Map.empty, expected: BooleanLiteral boolean }
      )

    expressionTestCase
      "a boolean literal in parentheses"
      [ pure "(", genBoolean, pure ")" ]
      ( \parts → ado
          boolean ← parseBackBoolean parts "boolean" 1
          in
            { context: Map.empty
            , expected: BooleanLiteral boolean
            }
      )

    expressionTestCase
      "an integer literal"
      [ genInteger ]
      ( \parts → ado
          integer ← parseBackInteger parts "integer" 0
          in { context: Map.empty, expected: IntegerLiteral integer }
      )

    expressionTestCase
      "a float literal"
      [ genFloat ]
      ( \parts → ado
          float ← parseBackFloat parts "float" 0
          in { context: Map.empty, expected: floatToExpression float }
      )

    expressionTestCase
      "a float literal in parentheses"
      [ pure "(", genFloat, pure ")" ]
      ( \parts → ado
          float ← parseBackFloat parts "float" 1
          in
            { context: Map.empty
            , expected: floatToExpression float
            }
      )

    expressionTestCase
      "a string literal"
      [ ado
          quote ← pure "\""
          word ← ExpressionGen.genIdentifier
          in quote <> word
      ]
      ( \parts → ado
          string ← parseBackPrefixedIdentifier parts "string" 0
          in { context: Map.empty, expected: StringLiteral string }
      )

    expressionTestCase
      "a value reference"
      [ genParameter ]
      ( \parts → ado
          name ← parseBackPrefixedIdentifier parts "reference" 0
          in
            { context: Map.empty, expected: ValueReference name }
      )

    expressionTestCase
      "a variable assignment"
      [ pure "make", pure " ", genString, pure " ", genBoolean ]
      ( \parts → ado
          name ← parseBackPrefixedIdentifier parts "variable name" 2
          boolean ← parseBackBoolean parts "boolean" 4
          in
            { context: Map.empty
            , expected: VariableAssignment name (BooleanLiteral boolean)
            }
      )

    expressionTestCase
      "a procedure call with literal arguments"
      [ genProcedureName, pure " ", genFloat, pure " ", genFloat ]
      ( \parts → ado
          procedureName ← parseBackIdentifier parts "procedure name" 0
          firstArgument ← parseBackFloat parts "first argument" 2
          secondArgument ← parseBackFloat parts "second argument" 4
          in
            { context: Map.fromFoldable [ procedureName /\ Just 2 ]
            , expected:
                ProcedureCall
                  procedureName
                  ( List.fromFoldable
                      [ floatToExpression firstArgument
                      , floatToExpression secondArgument
                      ]
                  )
            }
      )

    expressionTestCase
      "a procedure call with an operation argument 1"
      [ genProcedureName
      , pure " "
      , genFloat
      , pure " "
      , pure "+"
      , pure " "
      , genFloat
      ]
      ( \parts → ado
          procedureName ← parseBackIdentifier parts "procedure name" 0
          firstOperationArgument ← parseBackFloat
            parts
            "first operation argument"
            2
          secondOperationArgument ← parseBackFloat
            parts
            "second operation argument"
            6
          in
            { context: Map.fromFoldable [ procedureName /\ Just 1 ]
            , expected: ProcedureCall
                procedureName
                ( List.fromFoldable
                    [ BinaryOperation
                        Addition
                        (floatToExpression firstOperationArgument)
                        (floatToExpression secondOperationArgument)
                    ]
                )
            }
      )

    expressionTestCase
      "a procedure call with an operation argument 2"
      [ genProcedureName
      , pure " "
      , pure "("
      , genFloat
      , pure " "
      , pure "+"
      , pure " "
      , genFloat
      , pure ")"
      ]
      ( \parts → ado
          procedureName ← parseBackIdentifier parts "procedure name" 0
          firstOperationArgument ← parseBackFloat
            parts
            "first operation argument"
            3
          secondOperationArgument ← parseBackFloat
            parts
            "second operation argument"
            7
          in
            { context: Map.fromFoldable [ procedureName /\ Just 1 ]
            , expected: ProcedureCall
                procedureName
                ( List.fromFoldable
                    [ BinaryOperation
                        Addition
                        (floatToExpression firstOperationArgument)
                        (floatToExpression secondOperationArgument)
                    ]
                )
            }
      )

    expressionTestCase
      "a procedure call with an operation argument 3"
      [ genProcedureName
      , pure " "
      , pure "("
      , genValueReference
      , pure " "
      , pure "+"
      , pure " "
      , genValueReference
      , pure ")"
      ]
      ( \parts → ado
          procedureName ← parseBackIdentifier
            parts
            "procedure name"
            0
          firstOperationArgument ← parseBackPrefixedIdentifier
            parts
            "first operation argument"
            3
          secondOperationArgument ← parseBackPrefixedIdentifier
            parts
            "second operation argument"
            7
          in
            { context: Map.fromFoldable [ procedureName /\ Just 1 ]
            , expected: ProcedureCall
                procedureName
                ( List.fromFoldable
                    [ BinaryOperation
                        Addition
                        (ValueReference firstOperationArgument)
                        (ValueReference secondOperationArgument)
                    ]
                )
            }
      )

    expressionTestCase
      "an operation with a literal and procedure call argument 1"
      [ genFloat, pure "+", genProcedureName, pure " ", genFloat ]
      ( \parts → ado
          firstOperationArgument ← parseBackFloat
            parts
            "first operation argument"
            0
          procedureName ← parseBackIdentifier
            parts
            "procedure name"
            2
          procedureArgument ← parseBackFloat
            parts
            "procedure argument"
            4
          in
            { context: Map.fromFoldable [ procedureName /\ Just 1 ]
            , expected: BinaryOperation
                Addition
                (floatToExpression firstOperationArgument)
                ( ProcedureCall
                    procedureName
                    ( List.fromFoldable
                        [ floatToExpression procedureArgument ]
                    )
                )
            }
      )

    expressionTestCase
      "an operation with a literal and procedure call argument 2"
      [ genFloat
      , pure "+"
      , genProcedureName
      , pure " "
      , pure "("
      , genFloat
      , pure ")"
      ]
      ( \parts → ado
          firstOperationArgument ← parseBackFloat
            parts
            "first operation argument"
            0
          procedureName ← parseBackIdentifier parts "procedure name" 2
          procedureArgument ← parseBackFloat
            parts
            "procedure argument"
            5
          in
            { context: Map.fromFoldable [ procedureName /\ Just 1 ]
            , expected: BinaryOperation
                Addition
                (floatToExpression firstOperationArgument)
                ( ProcedureCall
                    procedureName
                    ( List.fromFoldable
                        [ floatToExpression procedureArgument
                        ]
                    )
                )
            }
      )

    expressionTestCase
      "an operation with a literal and procedure call argument 3"
      [ genFloat
      , pure "+"
      , genProcedureName
      , pure " "
      , pure "("
      , genFloat
      , pure "+"
      , genFloat
      , pure ")"
      ]
      ( \parts → ado
          firstOperationArgument ← parseBackFloat
            parts
            "first operation argument"
            0
          procedureName ← parseBackIdentifier parts "procedure name" 2
          firstProcedureArgument ← parseBackFloat
            parts
            "first procedure argument"
            5
          secondProcedureArgument ← parseBackFloat
            parts
            "second procedure argument"
            7
          in
            { context: Map.fromFoldable [ procedureName /\ Just 1 ]
            , expected: BinaryOperation
                Addition
                (floatToExpression firstOperationArgument)
                ( ProcedureCall
                    procedureName
                    ( List.fromFoldable
                        [ BinaryOperation
                            Addition
                            (floatToExpression firstProcedureArgument)
                            (floatToExpression secondProcedureArgument)
                        ]
                    )
                )
            }
      )

    expressionTestCase
      "an operation with a literal and procedure call argument 4"
      [ genFloat
      , pure "+"
      , genProcedureName
      , pure " "
      , pure "("
      , genValueReference
      , pure "+"
      , genValueReference
      , pure ")"
      ]
      ( \parts → ado
          firstOperationArgument ← parseBackFloat
            parts
            "first operation argument"
            0
          procedureName ← parseBackIdentifier parts "procedure name" 2
          firstProcedureArgument ← parseBackPrefixedIdentifier
            parts
            "first procedure argument"
            5
          secondProcedureArgument ← parseBackPrefixedIdentifier
            parts
            "second procedure argument"
            7
          in
            { context: Map.fromFoldable [ procedureName /\ Just 1 ]
            , expected: BinaryOperation
                Addition
                (floatToExpression firstOperationArgument)
                ( ProcedureCall
                    procedureName
                    ( List.fromFoldable
                        [ BinaryOperation
                            Addition
                            (ValueReference firstProcedureArgument)
                            (ValueReference secondProcedureArgument)
                        ]
                    )
                )
            }
      )

    expressionTestCase
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
          name ← parseBackIdentifier parts "procedure name" 2
          whitespacesRegex ← Regex.regex "\\s+" RegexFlags.global

          parameterNames ← List.fromFoldable
            <$> map (ParameterName <<< String.drop 1)
            <$> Array.filter (_ /= "")
            <$> Regex.split whitespacesRegex
            <$> String.trim
            <$> Either.note "can't parse parameters back" (parts !! 4)

          pure $
            { context: Map.fromFoldable
                [ "proc1" /\ Just 1, "proc2" /\ Just 1 ]
            , expected: ProcedureDefinition
                { name, parameterNames }
                ( List.fromFoldable
                    [ ProcedureCall "proc1"
                        (List.fromFoldable [ IntegerLiteral 1 ])
                    , ProcedureCall "proc2"
                        (List.fromFoldable [ IntegerLiteral 2 ])
                    ]
                )
            }
      )

    expressionTestCase
      "an if block"
      [ pure "if"
      , pure " "
      , genBoolean
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
      ( \parts → ado
          boolean ← parseBackBoolean parts "condition" 2
          in
            { context:
                Map.fromFoldable
                  [ "proc1" /\ Just 1, "proc2" /\ Just 1 ]
            , expected: IfBlock (BooleanLiteral boolean)
                ( List.fromFoldable
                    [ ProcedureCall "proc1"
                        (List.fromFoldable [ IntegerLiteral 1 ])
                    , ProcedureCall "proc2"
                        (List.fromFoldable [ IntegerLiteral 2 ])
                    ]
                )
            }
      )

    expressionTestCase
      "an if block with a integer equation"
      [ pure "if"
      , pure " "
      , pure "("
      , genInteger
      , pure "="
      , genInteger
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
      ( \parts → ado
          firstInteger ← parseBackInteger parts "first integer" 3
          secondInteger ← parseBackInteger parts "second integer" 5
          in
            { context:
                Map.fromFoldable
                  [ "proc1" /\ Just 1, "proc2" /\ Just 1 ]
            , expected: IfBlock
                ( BinaryOperation
                    Equation
                    (IntegerLiteral firstInteger)
                    (IntegerLiteral secondInteger)
                )
                ( List.fromFoldable
                    [ ProcedureCall "proc1"
                        (List.fromFoldable [ IntegerLiteral 1 ])
                    , ProcedureCall "proc2"
                        (List.fromFoldable [ IntegerLiteral 2 ])
                    ]
                )
            }
      )

    expressionTestCase
      "a repeat block"
      [ pure "repeat"
      , pure " "
      , genInteger
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
      ( \parts → ado
          times ← parseBackInteger parts "times" 2
          in
            { context:
                Map.fromFoldable
                  [ "proc1" /\ Just 1, "proc2" /\ Just 1 ]
            , expected: RepeatBlock (IntegerLiteral times)
                ( List.fromFoldable
                    [ ProcedureCall "proc1"
                        (List.fromFoldable [ IntegerLiteral 1 ])
                    , ProcedureCall "proc2"
                        (List.fromFoldable [ IntegerLiteral 2 ])
                    ]
                )
            }
      )

    expressionTestCase
      "a for block"
      [ pure "for"
      , pure " "
      , pure "["
      , pure "idx"
      , pure " "
      , genInteger
      , pure " "
      , genInteger
      , pure "]"
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
      ( \parts → ado
          initialValue ← parseBackInteger parts "initial value" 5
          terminalValue ← parseBackInteger parts "terminal value" 7
          in
            { context:
                Map.fromFoldable
                  [ "proc1" /\ Just 1, "proc2" /\ Just 1 ]
            , expected: ForBlock
                { binder: "idx", initialValue, step: 1, terminalValue }
                ( List.fromFoldable
                    [ ProcedureCall "proc1"
                        (List.fromFoldable [ IntegerLiteral 1 ])
                    , ProcedureCall "proc2"
                        (List.fromFoldable [ IntegerLiteral 2 ])
                    ]
                )
            }
      )

  arityTestCase
    "the second procedure takes 0 arguments"
    ( \{ argument1, argument2, procedureName1, procedureName2 } →
        { context:
            Map.fromFoldable
              [ procedureName1 /\ Just 3, procedureName2 /\ Just 0 ]
        , expected: ProcedureCall procedureName1
            ( List.fromFoldable
                [ ProcedureCall procedureName2 Nil
                , IntegerLiteral argument1
                , IntegerLiteral argument2
                ]
            )
        }
    )

  arityTestCase
    "the second procedure takes 1 argument"
    ( \{ argument1, argument2, procedureName1, procedureName2 } →
        { context:
            Map.fromFoldable
              [ procedureName1 /\ Just 2, procedureName2 /\ Just 1 ]
        , expected: ProcedureCall procedureName1
            ( List.fromFoldable
                [ ProcedureCall
                    procedureName2
                    (List.fromFoldable [ IntegerLiteral argument1 ])
                , IntegerLiteral argument2
                ]
            )
        }
    )

  arityTestCase
    "the second procedure takes 2 arguments"
    ( \{ argument1, argument2, procedureName1, procedureName2 } →
        { context:
            Map.fromFoldable
              [ procedureName1 /\ Just 1, procedureName2 /\ Just 2 ]
        , expected: ProcedureCall procedureName1
            ( List.fromFoldable
                [ ProcedureCall
                    procedureName2
                    ( List.fromFoldable
                        [ IntegerLiteral argument1
                        , IntegerLiteral argument2
                        ]
                    )
                ]
            )
        }
    )

  describe "expressions" do
    traverseWithIndex_
      ( \title (Example { ast, source }) →
          expressionsTestCase title source ast
      )
      Examples.all

    expressionsTestCase
      "string literals are terminated by a space"
      "\"string 1"
      [ StringLiteral "string", IntegerLiteral 1 ]

    expressionsTestCase
      "two positive integer literals"
      "1 2"
      [ IntegerLiteral 1, IntegerLiteral 2 ]

    expressionsTestCase
      "a negative and a positive integer literals"
      "-1 2"
      [ UnaryOperation Negation (IntegerLiteral 1), IntegerLiteral 2 ]

    expressionsTestCase
      "a positive and a negative integer literals"
      "1 -2"
      [ IntegerLiteral 1, UnaryOperation Negation (IntegerLiteral 2) ]

    expressionsTestCase
      "two negative integer literals"
      "-1 -2"
      [ UnaryOperation Negation (IntegerLiteral 1)
      , UnaryOperation Negation (IntegerLiteral 2)
      ]

    expressionsTestCase
      "subtraction"
      "1 - 2"
      [ BinaryOperation
          Subtraction
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      ]

  describe "procedureSignature" do
    procedureSignatureTestCase
      "no parameters"
      [ pure "to", pure " ", genProcedureName ]
      ( \parts → do
          name ← parseBackIdentifier parts "procedure name" 2
          pure { name, parameterNames: Nil }
      )
    procedureSignatureTestCase
      "one parameters"
      [ pure "to", pure " ", genProcedureName, genParameter ]
      ( \parts → do
          name ← parseBackIdentifier parts "procedure name" 2

          firstParameter ← ParameterName <$> parseBackPrefixedIdentifier
            parts
            "first parameter"
            3

          pure
            { name
            , parameterNames: List.fromFoldable [ firstParameter ]
            }
      )

    procedureSignatureTestCase
      "two parameters"
      [ pure "to"
      , pure " "
      , genProcedureName
      , genParameter
      , genParameter
      ]
      ( \parts → do
          name ← parseBackIdentifier parts "procedure name" 2

          firstParameter ← ParameterName <$> parseBackPrefixedIdentifier
            parts
            "first parameter"
            3

          secondParameter ← ParameterName
            <$> parseBackPrefixedIdentifier
              parts
              "second parameter"
              4

          pure
            { name
            , parameterNames: List.fromFoldable
                [ firstParameter, secondParameter ]
            }
      )

  describe "procedureSignatures" do
    procedureSignaturesTestCase
      "multiple procedures of various arity"
      [ "forward 1"
      , "to proc0"
      , "clearscreen"
      , "end"
      , "forward 2"
      , "proc0"
      , "forward 3"
      , "to proc1 :param1"
      , "forward :param1"
      , "end"
      , "forward 4"
      , "proc1 10"
      , "forward 5"
      , "to proc2 :param1 :param2"
      , "forward :param1"
      , "forward :param2"
      , "end"
      , "forward 6"
      , "proc2 10 20"
      , "forward 7"
      ]
      [ { name: "proc0"
        , parameterNames: Nil
        }
      , { name: "proc1"
        , parameterNames: List.fromFoldable [ ParameterName "param1" ]
        }
      , { name: "proc2"
        , parameterNames: List.fromFoldable
            [ ParameterName "param1", ParameterName "param2" ]
        }
      ]

arithmeticalBinaryOperatorTestCase
  ∷ BinaryOperationType
  → TestSpec
arithmeticalBinaryOperatorTestCase operationType =
  let
    symbol = Expression.binaryOperationTypeSymbol operationType
  in
    expressionTestCase
      ("\"" <> symbol <> "\" symbol")
      [ genFloat, pure symbol, genFloat ]
      ( \parts → ado
          leftOperand ← Either.note
            "can't parse the left operand back"
            (Number.fromString =<< parts !! 0)
          rightOperand ← Either.note
            "can't parse the right operand back"
            (Number.fromString =<< parts !! 2)
          in
            { context: Map.empty
            , expected: BinaryOperation
                operationType
                (floatToExpression leftOperand)
                (floatToExpression rightOperand)
            }
      )

genBoolean ∷ Gen String
genBoolean = show <$> ExpressionGen.genBoolean

genInteger ∷ Gen String
genInteger = show <$> ExpressionGen.genInteger

genFloat ∷ Gen String
genFloat = show <$> ExpressionGen.genFloat

genString ∷ Gen String
genString = do
  s ← ExpressionGen.genString
  pure $ "\"" <> s

genValueReference ∷ Gen String
genValueReference = genParameter

genParameter ∷ Gen String
genParameter = do
  identifier ← ExpressionGen.genIdentifier
  pure $ ":" <> identifier

genProcedureName ∷ Gen String
genProcedureName = ExpressionGen.genIdentifier

expressionTestCase
  ∷ ∀ f
  . Foldable f
  ⇒ Traversable f
  ⇒ String
  → f (Gen String)
  → ( Array String
      → String \/ { expected ∷ Expression, context ∷ ParsingContext }
    )
  → TestSpec
expressionTestCase title partGenerators makeExpected =
  generativeTestCase Short title do
    parts ← sequence partGenerators
    source ← Utils.addRedundantSpaces parts

    case makeExpected $ Array.fromFoldable parts of
      Left errorMessage →
        pure $ Failed errorMessage
      Right { expected, context } → do
        let
          actual =
            case
              P.runParser source (Parsing.expression context)
              of
              Left parseError →
                Left $ show parseError
              Right expression →
                Right expression

        pure $
          if actual == (Right expected) then Success
          else Failed $
            "--- error >>> ---\n"
              <> show actual
              <> "\nis not equal to\n"
              <> show expected
              <> "\n--- source >>> ---\n"
              <> Utils.emphasizeWhitespaces source
              <> "\n--- <<< source ---"
              <> "\n--- <<< error ---"

arityTestCase
  ∷ String
  → ( { argument1 ∷ Int
      , argument2 ∷ Int
      , procedureName1 ∷ String
      , procedureName2 ∷ String
      }
      → { expected ∷ Expression, context ∷ ParsingContext }
    )
  → TestSpec
arityTestCase title makeExpected = expressionTestCase
  ("arity - " <> title)
  [ do
      procedureName1 ← genProcedureName
      procedureName2 ← genProcedureName
        `Gen.suchThat` (_ /= procedureName1)
      pure $ procedureName1 <> " " <> procedureName2
  , pure " "
  , genInteger
  , pure " "
  , genInteger
  ]
  ( \parts → do
      procedureNames ← String.split (Pattern " ")
        <$> parseBackIdentifier parts "procedure names" 0
      procedureName1 ← Either.note
        "procedure name 1"
        (procedureNames !! 0)
      procedureName2 ← Either.note
        "procedure name 2"
        (procedureNames !! 1)
      argument1 ← parseBackInteger parts "argument 1" 2
      argument2 ← parseBackInteger parts "argument 2" 4
      pure $ makeExpected
        { argument1, argument2, procedureName1, procedureName2 }
  )

expressionsTestCase
  ∷ ∀ f
  . Foldable f
  ⇒ Show (f Expression)
  ⇒ String
  → String
  → f Expression
  → TestSpec
expressionsTestCase title source expected = it title do
  let
    parsingResult = P.runParser
      source
      (Parsing.expressions Commands.parsingContext)

    actual = case parsingResult of
      Left parseError →
        Left $ show parseError
      Right expressions →
        Right expressions

  if actual == (Right $ List.fromFoldable expected) then pure unit
  else
    fail $ "--- error >>> ---\n"
      <> show actual
      <> "\nis not equal to\n"
      <> show expected
      <> "\n--- source >>> ---\n"
      <> Utils.emphasizeWhitespaces source
      <> "\n--- <<< source ---"
      <> "\n--- <<< error ---"

procedureSignatureTestCase
  ∷ ∀ f
  . Foldable f
  ⇒ Traversable f
  ⇒ String
  → f (Gen String)
  → (Array String → String \/ ProcedureSignature)
  → TestSpec
procedureSignatureTestCase title partGenerators makeExpected =
  generativeTestCase Short title do
    parts ← sequence partGenerators
    source ← Utils.addRedundantSpaces parts

    let
      actual = case P.runParser source Parsing.procedureSignature of
        Left parseError →
          Left $ show parseError
        Right procedureSignature →
          Right procedureSignature

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

procedureSignaturesTestCase
  ∷ ∀ f
  . Foldable f
  ⇒ Show (f ProcedureSignature)
  ⇒ Traversable f
  ⇒ String
  → f String
  → f ProcedureSignature
  → TestSpec
procedureSignaturesTestCase title sourceLines expected = it title do
  let
    source = String.joinWith "\n" (Array.fromFoldable sourceLines)
    actual = case P.runParser source Parsing.procedureSignatures of
      Left parseError →
        Left $ show parseError
      Right signatures →
        Right signatures

  if actual == (Right $ List.fromFoldable expected) then pure unit
  else
    fail $ "--- error >>> ---\n"
      <> show actual
      <> "\nis not equal to\n"
      <> show expected
      <> "\n--- source >>> ---\n"
      <> Utils.emphasizeWhitespaces source
      <> "\n--- <<< source ---"
      <> "\n--- <<< error ---"

parseBackPrefixedIdentifier
  ∷ Array String → String → Int → String \/ String
parseBackPrefixedIdentifier parts name index =
  String.drop 1 <$> parseBackIdentifier parts name index

parseBackIdentifier
  ∷ Array String → String → Int → String \/ String
parseBackIdentifier parts name index =
  parseBack parts name index pure

parseBackBoolean
  ∷ Array String → String → Int → String \/ Boolean
parseBackBoolean parts name index =
  parseBack parts name index case _ of
    "false" →
      Just false
    "true" →
      Just true
    _ →
      Nothing

parseBackInteger
  ∷ Array String → String → Int → String \/ Int
parseBackInteger parts name index =
  parseBack parts name index Int.fromString

parseBackFloat
  ∷ Array String → String → Int → String \/ Number
parseBackFloat parts name index =
  parseBack parts name index Number.fromString

parseBack
  ∷ ∀ a
  . Array String
  → String
  → Int
  → (String → Maybe a)
  → String \/ a
parseBack parts name index fromString = Either.note
  ("can't parse back the " <> name)
  (fromString =<< parts !! index)

floatToExpression ∷ Number → Expression
floatToExpression x =
  if x < 0.0 then
    UnaryOperation Negation (FloatLiteral (-x))
  else
    FloatLiteral x

intToExpression ∷ Int → Expression
intToExpression n =
  if n < 0 then
    UnaryOperation Negation (IntegerLiteral (-n))
  else
    IntegerLiteral n
