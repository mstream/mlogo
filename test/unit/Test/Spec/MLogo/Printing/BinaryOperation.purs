module Test.Spec.MLogo.Printing.BinaryOperation (spec) where

import Prelude

import Data.List as List
import Data.List.NonEmpty as ListNonEmpty
import Data.Tuple.Nested ((/\))
import MLogo.Parsing.Expression
  ( BinaryOperationType(..)
  , Expression(..)
  )
import MLogo.Printing as Printing
import MLogo.Printing.BinaryOperation (BinaryOperationCollection)
import MLogo.Printing.BinaryOperation as BinaryOperation
import MLogo.Printing.Code (Code(..), CodeWord(..))
import MLogo.Printing.Print (PrintConfig)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail)
import Test.Types (TestSpec)

spec ∷ TestSpec
spec = describe "BinaryOperation" do

  describe "collect" do

    collectTestCase
      "a single addition"
      { leftOperand: IntegerLiteral 1
      , operationType: Addition
      , rightOperand: IntegerLiteral 2
      }
      true
      { head: IntegerLiteral 1
      , tail: ListNonEmpty.singleton $ Addition /\ IntegerLiteral 2
      }

    collectTestCase
      "a single subtraction"
      { leftOperand: IntegerLiteral 1
      , operationType: Subtraction
      , rightOperand: IntegerLiteral 2
      }
      true
      { head: IntegerLiteral 1
      , tail: ListNonEmpty.singleton $ Subtraction /\ IntegerLiteral 2
      }

    collectTestCase
      "a left-biased chain of additions"
      { leftOperand:
          BinaryOperation
            Addition
            ( BinaryOperation
                Addition
                (IntegerLiteral 1)
                (IntegerLiteral 2)
            )
            (IntegerLiteral 3)
      , operationType: Addition
      , rightOperand: IntegerLiteral 4
      }
      true
      { head: IntegerLiteral 1
      , tail: ListNonEmpty.cons'
          (Addition /\ IntegerLiteral 2)
          ( List.fromFoldable
              [ Addition /\ IntegerLiteral 3
              , Addition /\ IntegerLiteral 4
              ]
          )
      }

    collectTestCase
      "a left-biased chain of subtractions"
      { leftOperand:
          BinaryOperation
            Subtraction
            ( BinaryOperation
                Subtraction
                (IntegerLiteral 1)
                (IntegerLiteral 2)
            )
            (IntegerLiteral 3)
      , operationType: Subtraction
      , rightOperand: IntegerLiteral 4
      }
      true
      { head: IntegerLiteral 1
      , tail: ListNonEmpty.cons'
          (Subtraction /\ IntegerLiteral 2)
          ( List.fromFoldable
              [ Subtraction /\ IntegerLiteral 3
              , Subtraction /\ IntegerLiteral 4
              ]
          )
      }

    collectTestCase
      "a right-biased chain of additions"
      { leftOperand: IntegerLiteral 1
      , operationType: Addition
      , rightOperand: BinaryOperation
          Addition
          (IntegerLiteral 2)
          ( BinaryOperation
              Addition
              (IntegerLiteral 3)
              (IntegerLiteral 4)
          )
      }
      true
      { head: IntegerLiteral 1
      , tail: ListNonEmpty.cons'
          (Addition /\ IntegerLiteral 2)
          ( List.fromFoldable
              [ Addition /\ IntegerLiteral 3
              , Addition /\ IntegerLiteral 4
              ]
          )
      }

    collectTestCase
      "a right-biased chain of subtractions"
      { leftOperand: IntegerLiteral 1
      , operationType: Subtraction
      , rightOperand: BinaryOperation
          Subtraction
          (IntegerLiteral 2)
          ( BinaryOperation
              Subtraction
              (IntegerLiteral 3)
              (IntegerLiteral 4)
          )
      }
      true
      { head: IntegerLiteral 1
      , tail: ListNonEmpty.singleton
          $ Subtraction /\ BinaryOperation
              Subtraction
              (IntegerLiteral 2)
              ( BinaryOperation Subtraction
                  (IntegerLiteral 3)
                  (IntegerLiteral 4)
              )
      }

    collectTestCase
      "a balanced chain of additions"
      { leftOperand:
          BinaryOperation
            Addition
            (IntegerLiteral 1)
            (IntegerLiteral 2)
      , operationType: Addition
      , rightOperand: BinaryOperation
          Addition
          (IntegerLiteral 3)
          (IntegerLiteral 4)
      }
      true
      { head: IntegerLiteral 1
      , tail: ListNonEmpty.cons'
          (Addition /\ IntegerLiteral 2)
          ( List.fromFoldable
              [ Addition /\ IntegerLiteral 3
              , Addition /\ IntegerLiteral 4
              ]
          )
      }

    collectTestCase
      "a balanced chain of subtraction"
      { leftOperand: BinaryOperation
          Subtraction
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      , operationType: Subtraction
      , rightOperand:
          BinaryOperation
            Subtraction
            (IntegerLiteral 3)
            (IntegerLiteral 4)
      }
      true
      { head: IntegerLiteral 1
      , tail:
          ListNonEmpty.cons'
            (Subtraction /\ IntegerLiteral 2)
            ( List.fromFoldable
                [ Subtraction /\
                    BinaryOperation
                      Subtraction
                      (IntegerLiteral 3)
                      (IntegerLiteral 4)

                ]
            )
      }

    collectTestCase
      "a balanced chain of subtraction"
      { leftOperand: BinaryOperation
          Subtraction
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      , operationType: Subtraction
      , rightOperand:
          BinaryOperation
            Subtraction
            (IntegerLiteral 3)
            (IntegerLiteral 4)
      }
      false
      { head: BinaryOperation
          Subtraction
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      , tail: ListNonEmpty.singleton
          $ Subtraction /\
              ( BinaryOperation
                  Subtraction
                  (IntegerLiteral 3)
                  (IntegerLiteral 4)
              )
      }

    collectTestCase
      "multiplication of an addition and a literal"
      { leftOperand: BinaryOperation
          Addition
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      , operationType: Multiplication
      , rightOperand: IntegerLiteral 3
      }
      true
      { head: BinaryOperation
          Addition
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      , tail: ListNonEmpty.singleton
          $ Multiplication /\ IntegerLiteral 3
      }

    collectTestCase
      "multiplication of a literal and an addition"
      { leftOperand: IntegerLiteral 1
      , operationType: Multiplication
      , rightOperand: BinaryOperation Addition
          (IntegerLiteral 2)
          (IntegerLiteral 3)
      }
      true
      { head: IntegerLiteral 1
      , tail: ListNonEmpty.singleton
          $ Multiplication /\ BinaryOperation Addition
              (IntegerLiteral 2)
              (IntegerLiteral 3)
      }

    collectTestCase
      "multiplication of additions"
      { leftOperand: BinaryOperation
          Addition
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      , operationType: Multiplication
      , rightOperand:
          BinaryOperation
            Addition
            (IntegerLiteral 3)
            (IntegerLiteral 4)
      }
      true
      { head: BinaryOperation
          Addition
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      , tail: ListNonEmpty.singleton
          $ Multiplication /\
              ( BinaryOperation
                  Addition
                  (IntegerLiteral 3)
                  (IntegerLiteral 4)
              )
      }

    collectTestCase
      "addition of a multiplication and an addition"
      { leftOperand: BinaryOperation
          Multiplication
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      , operationType: Addition
      , rightOperand:
          BinaryOperation
            Addition
            (IntegerLiteral 3)
            (IntegerLiteral 4)
      }
      true
      { head: BinaryOperation
          Multiplication
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      , tail: ListNonEmpty.cons'
          (Addition /\ IntegerLiteral 3)
          (List.fromFoldable [ Addition /\ IntegerLiteral 4 ])
      }

    collectTestCase
      "addition of a multiplication and an addition"
      { leftOperand: BinaryOperation
          Multiplication
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      , operationType: Addition
      , rightOperand:
          BinaryOperation
            Addition
            (IntegerLiteral 3)
            (IntegerLiteral 4)
      }
      false
      { head: BinaryOperation
          Multiplication
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      , tail: ListNonEmpty.singleton
          $ Addition /\ BinaryOperation
              Addition
              (IntegerLiteral 3)
              (IntegerLiteral 4)
      }

  describe "print" do

    printTestCase
      "a left-biased chain of additions"
      { leftOperand:
          BinaryOperation
            Addition
            ( BinaryOperation
                Addition
                (IntegerLiteral 1)
                (IntegerLiteral 2)
            )
            (IntegerLiteral 3)
      , operationType: Addition
      , rightOperand: IntegerLiteral 4
      }
      { pageWidth: 100, simplifyBinaryOperations: true }
      ( SingleLine $ CodeWord <$> List.fromFoldable
          [ "1"
          , "+"
          , "2"
          , "+"
          , "3"
          , "+"
          , "4"
          ]
      )

    printTestCase
      "a left-biased chain of additions"
      { leftOperand:
          BinaryOperation
            Addition
            ( BinaryOperation
                Addition
                (IntegerLiteral 1)
                (IntegerLiteral 2)
            )
            (IntegerLiteral 3)
      , operationType: Addition
      , rightOperand: IntegerLiteral 4
      }
      { pageWidth: 10, simplifyBinaryOperations: true }
      ( MultiLine $ List.fromFoldable
          [ SingleLine $ List.fromFoldable [ CodeWord "1" ]
          , SingleLine $ List.fromFoldable [ CodeWord "+" ]
          , SingleLine $ List.fromFoldable [ CodeWord "2" ]
          , SingleLine $ List.fromFoldable [ CodeWord "+" ]
          , SingleLine $ List.fromFoldable [ CodeWord "3" ]
          , SingleLine $ List.fromFoldable [ CodeWord "+" ]
          , SingleLine $ List.fromFoldable [ CodeWord "4" ]
          ]
      )

    printTestCase
      "multiplication of an addition and a literal"
      { leftOperand: BinaryOperation
          Addition
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      , operationType: Multiplication
      , rightOperand: IntegerLiteral 3
      }
      { pageWidth: 100, simplifyBinaryOperations: true }
      ( SingleLine $ CodeWord <$> List.fromFoldable
          [ "("
          , "1"
          , "+"
          , "2"
          , ")"
          , "*"
          , "3"
          ]
      )

    printTestCase
      "multiplication of a literal and an addition"
      { leftOperand: IntegerLiteral 1
      , operationType: Multiplication
      , rightOperand: BinaryOperation
          Addition
          (IntegerLiteral 2)
          (IntegerLiteral 3)
      }
      { pageWidth: 100, simplifyBinaryOperations: true }
      ( SingleLine $ CodeWord <$> List.fromFoldable
          [ "1"
          , "*"
          , "("
          , "2"
          , "+"
          , "3"
          , ")"
          ]
      )

    printTestCase
      "multiplication of additions"
      { leftOperand: BinaryOperation
          Addition
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      , operationType: Multiplication
      , rightOperand:
          BinaryOperation
            Addition
            (IntegerLiteral 3)
            (IntegerLiteral 4)
      }
      { pageWidth: 100, simplifyBinaryOperations: true }
      ( SingleLine $ CodeWord <$> List.fromFoldable
          [ "("
          , "1"
          , "+"
          , "2"
          , ")"
          , "*"
          , "("
          , "3"
          , "+"
          , "4"
          , ")"
          ]
      )

    printTestCase
      "subtraction of subtractions"
      { leftOperand: BinaryOperation
          Subtraction
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      , operationType: Subtraction
      , rightOperand:
          BinaryOperation
            Subtraction
            (IntegerLiteral 3)
            (IntegerLiteral 4)
      }
      { pageWidth: 100, simplifyBinaryOperations: true }
      ( SingleLine $ CodeWord <$> List.fromFoldable
          [ "1"
          , "-"
          , "2"
          , "-"
          , "("
          , "3"
          , "-"
          , "4"
          , ")"
          ]
      )

    printTestCase
      "subtraction of subtractions"
      { leftOperand: BinaryOperation
          Subtraction
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      , operationType: Subtraction
      , rightOperand:
          BinaryOperation
            Subtraction
            (IntegerLiteral 3)
            (IntegerLiteral 4)
      }
      { pageWidth: 100, simplifyBinaryOperations: false }
      ( SingleLine $ CodeWord <$> List.fromFoldable
          [ "("
          , "1"
          , "-"
          , "2"
          , ")"
          , "-"
          , "("
          , "3"
          , "-"
          , "4"
          , ")"
          ]
      )

    printTestCase
      "addition of a multiplication and an addition"
      { leftOperand: BinaryOperation
          Multiplication
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      , operationType: Addition
      , rightOperand:
          BinaryOperation
            Addition
            (IntegerLiteral 3)
            (IntegerLiteral 4)
      }
      { pageWidth: 100, simplifyBinaryOperations: true }
      ( SingleLine $ CodeWord <$> List.fromFoldable
          [ "1"
          , "*"
          , "2"
          , "+"
          , "3"
          , "+"
          , "4"
          ]
      )

    printTestCase
      "addition of a multiplication and an addition"
      { leftOperand: BinaryOperation
          Multiplication
          (IntegerLiteral 1)
          (IntegerLiteral 2)
      , operationType: Addition
      , rightOperand:
          BinaryOperation
            Addition
            (IntegerLiteral 3)
            (IntegerLiteral 4)
      }
      { pageWidth: 10, simplifyBinaryOperations: true }
      ( MultiLine $ List.fromFoldable
          [ SingleLine
              $ CodeWord <$> List.fromFoldable [ "1", "*", "2" ]
          , SingleLine $ List.fromFoldable [ CodeWord "+" ]
          , SingleLine $ List.fromFoldable [ CodeWord "3" ]
          , SingleLine $ List.fromFoldable [ CodeWord "+" ]
          , SingleLine $ List.fromFoldable [ CodeWord "4" ]
          ]
      )

collectTestCase
  ∷ String
  → { leftOperand ∷ Expression
    , operationType ∷ BinaryOperationType
    , rightOperand ∷ Expression
    }
  → Boolean
  → BinaryOperationCollection
  → TestSpec
collectTestCase
  title
  { leftOperand, operationType, rightOperand }
  simplifyBinaryOperations
  expected = it
  ( "collects by operation type \"" <> title <> "\", "
      <> (if simplifyBinaryOperations then "with" else "without")
      <> " binary operations simplification"
  )
  do
    let
      actual ∷ BinaryOperationCollection
      actual = BinaryOperation.collect
        simplifyBinaryOperations
        operationType
        leftOperand
        rightOperand

    if actual == expected then pure unit
    else
      fail $ "--- error >>> ---\n"
        <> show actual
        <> "\nis not equal to\n"
        <> show expected
        <> "\n--- <<< error ---"

printTestCase
  ∷ String
  → { leftOperand ∷ Expression
    , operationType ∷ BinaryOperationType
    , rightOperand ∷ Expression
    }
  → PrintConfig
  → Code
  → TestSpec
printTestCase
  title
  { leftOperand, operationType, rightOperand }
  printConfig
  expected = it
  ( "prints \""
      <> title
      <> "\" - a width of "
      <> show printConfig.pageWidth
      <> ", "
      <>
        ( if printConfig.simplifyBinaryOperations then ""
          else "no "
        )
      <> "binary operation simplification"
  )
  do
    let
      actual ∷ Code
      actual = BinaryOperation.print
        Printing.printExpression
        operationType
        leftOperand
        rightOperand
        printConfig

    if actual == expected then pure unit
    else
      fail $ "--- error >>> ---\n"
        <> show actual
        <> "\nis not equal to\n"
        <> show expected
        <> "\n--- <<< error ---"

