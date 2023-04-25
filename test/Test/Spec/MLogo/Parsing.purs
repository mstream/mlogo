module Test.Spec.MLogo.Parsing (spec) where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Char.Gen as CharGen
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
import Effect.Class (liftEffect)
import Examples (Example(..))
import Examples as Examples
import MLogo.Interpretation.Command.Commands as Commands
import MLogo.Lexing as Lexing
import MLogo.Parsing
  ( Expression(..)
  , Parameter(..)
  , ParsingContext
  , ProcedureSignature
  )
import MLogo.Parsing as Parsing
import Parsing as P
import Test.QuickCheck (Result(..), quickCheckGen)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Utils as Utils

spec ∷ Spec Unit
spec = describe "Parsing" do
  describe "expression" do
    arithmeticalBinaryOperatorTestCase
      "="
      Equation

    arithmeticalBinaryOperatorTestCase
      "+"
      Addition

    arithmeticalBinaryOperatorTestCase
      "/"
      Division

    arithmeticalBinaryOperatorTestCase
      "^"
      Exponentiation

    arithmeticalBinaryOperatorTestCase
      "*"
      Multiplication

    arithmeticalBinaryOperatorTestCase
      "-"
      Subtraction

    expressionTestCase
      "multiplication and addition precedence"
      [ genFloat, pure "+", genFloat, pure "*", genFloat ]
      ( \parts → ado
          firstFloat ← parseBackFloat parts "first float" 0
          secondFloat ← parseBackFloat parts "second float" 2
          thirdFloat ← parseBackFloat parts "third float" 4
          in
            { context: Map.empty
            , expected: Addition
                (FloatLiteral firstFloat)
                ( Multiplication
                    (FloatLiteral secondFloat)
                    (FloatLiteral thirdFloat)
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
            , expected: Division
                ( Multiplication
                    (FloatLiteral firstFloat)
                    (FloatLiteral secondFloat)
                )
                (FloatLiteral thirdFloat)

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
            , expected: SubExpression $ BooleanLiteral boolean
            }
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
                      [ FloatLiteral firstArgument
                      , FloatLiteral secondArgument
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
                    [ Addition (FloatLiteral firstOperationArgument)
                        (FloatLiteral secondOperationArgument)
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
                    [ SubExpression $ Addition
                        (FloatLiteral firstOperationArgument)
                        (FloatLiteral secondOperationArgument)
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
                    [ SubExpression $ Addition
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
            , expected: Addition
                (FloatLiteral firstOperationArgument)
                ( ProcedureCall
                    procedureName
                    ( List.fromFoldable
                        [ FloatLiteral procedureArgument ]
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
            , expected: Addition
                (FloatLiteral firstOperationArgument)
                ( ProcedureCall
                    procedureName
                    ( List.fromFoldable
                        [ SubExpression $ FloatLiteral procedureArgument
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
            , expected: Addition
                (FloatLiteral firstOperationArgument)
                ( ProcedureCall
                    procedureName
                    ( List.fromFoldable
                        [ SubExpression $ Addition
                            (FloatLiteral firstProcedureArgument)
                            (FloatLiteral secondProcedureArgument)
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
            , expected: Addition
                (FloatLiteral firstOperationArgument)
                ( ProcedureCall
                    procedureName
                    ( List.fromFoldable
                        [ SubExpression $ Addition
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

          parameters ← List.fromFoldable
            <$> map (Parameter <<< String.drop 1)
            <$> Array.filter (_ /= "")
            <$> Regex.split whitespacesRegex
            <$> String.trim
            <$> Either.note "can't parse parameters back" (parts !! 4)

          pure $
            { context: Map.fromFoldable
                [ "proc1" /\ Just 1, "proc2" /\ Just 1 ]
            , expected: ProcedureDefinition
                { name, parameters }
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
                ( SubExpression $ Equation
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
      Examples.examplesByTitle

  describe "procedureSignature" do
    procedureSignatureTestCase
      "no parameters"
      [ pure "to", pure " ", genProcedureName ]
      ( \parts → do
          name ← parseBackIdentifier parts "procedure name" 2
          pure { name, parameters: Nil }
      )
    procedureSignatureTestCase
      "one parameters"
      [ pure "to", pure " ", genProcedureName, genParameter ]
      ( \parts → do
          name ← parseBackIdentifier parts "procedure name" 2
          firstParameter ← Parameter <$> parseBackPrefixedIdentifier
            parts
            "first parameter"
            3
          pure
            { name, parameters: List.fromFoldable [ firstParameter ] }
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
          firstParameter ← Parameter <$> parseBackPrefixedIdentifier
            parts
            "first parameter"
            3
          secondParameter ← Parameter <$> parseBackPrefixedIdentifier
            parts
            "second parameter"
            4
          pure
            { name
            , parameters: List.fromFoldable
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
        , parameters: Nil
        }
      , { name: "proc1"
        , parameters: List.fromFoldable [ Parameter "param1" ]
        }
      , { name: "proc2"
        , parameters: List.fromFoldable
            [ Parameter "param1", Parameter "param2" ]
        }
      ]

arithmeticalBinaryOperatorTestCase
  ∷ String
  → (Expression → Expression → Expression)
  → Spec Unit
arithmeticalBinaryOperatorTestCase symbol expected =
  expressionTestCase
    ("\"" <> symbol <> "\" symbol")
    [ genFloat, pure symbol, genFloat ]
    ( \parts → ado
        leftOperand ← Either.note
          "can't parse left operand back"
          (Number.fromString =<< parts !! 0)
        rightOperand ← Either.note
          "can't parse right operand back"
          (Number.fromString =<< parts !! 2)
        in
          { context: Map.empty
          , expected: expected
              (FloatLiteral leftOperand)
              (FloatLiteral rightOperand)
          }
    )

genBoolean ∷ Gen String
genBoolean = do
  n ← Gen.chooseInt 0 1
  pure $ if n == 0 then "false" else "true"

genInteger ∷ Gen String
genInteger = show <$> Gen.chooseInt 0 9

genFloat ∷ Gen String
genFloat = do
  n1 ← Gen.chooseInt 0 9
  n2 ← Gen.chooseInt 0 9
  pure $ show n1 <> "." <> show n2

genString ∷ Gen String
genString = do
  identifier ← genIdentifier
  pure $ "\"" <> identifier

genValueReference ∷ Gen String
genValueReference = genParameter

genParameter ∷ Gen String
genParameter = do
  identifier ← genIdentifier
  pure $ ":" <> identifier

genProcedureName ∷ Gen String
genProcedureName = genIdentifier

genIdentifier ∷ Gen String
genIdentifier =
  gen `Gen.suchThat` (_ `Array.notElem` Lexing.reservedNames)
  where
  gen = do
    chars ← Gen.arrayOf1 CharGen.genAlpha
    pure
      $ String.fromCodePointArray
      $ String.codePointFromChar <$> Array.fromFoldable chars

expressionTestCase
  ∷ ∀ f
  . Foldable f
  ⇒ Traversable f
  ⇒ String
  → f (Gen String)
  → ( Array String
      → String \/ { expected ∷ Expression, context ∷ ParsingContext }
    )
  → Spec Unit
expressionTestCase title partGenerators makeExpected = it title do
  liftEffect $ quickCheckGen do
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
  → Spec Unit
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
  → Spec Unit
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
  → Spec Unit
procedureSignatureTestCase title partGenerators makeExpected = it title
  do
    liftEffect $ quickCheckGen do
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
  → Spec Unit
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
