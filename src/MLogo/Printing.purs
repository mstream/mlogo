module MLogo.Printing (printExpressions) where

import Prelude

import Data.Foldable (class Foldable, foldMap, null)
import Data.List (List, (:))
import Data.List as List
import Data.Newtype (wrap)
import Data.Number as Number
import Data.Number.Format as NumberFormat
import MLogo.Lexing as Lexing
import MLogo.Parsing.Expression
  ( BinaryOperationType
  , Expression(..)
  , ForBlockSpec
  , ParameterName(..)
  , UnaryOperationType(..)
  )
import MLogo.Parsing.Expression as Expression
import MLogo.Parsing.Operator (Associativity)
import MLogo.Parsing.Operator as Operator
import MLogo.Printing.Code (Code(..), CodeWord(..))
import MLogo.Printing.Code as Code
import MLogo.Printing.Print (Print)
import Parsing.Expr (Assoc(..))

printExpressions ∷ ∀ f. Foldable f ⇒ Functor f ⇒ Print (f Expression)
printExpressions expressions pageWidth =
  if
    Code.allSingleLine printedExpressions
      && Code.codeWidth singleLineForm <= pageWidth then singleLineForm
  else multiLineForm
  where
  singleLineForm ∷ Code
  singleLineForm = SingleLine $ foldMap Code.words printedExpressions

  multiLineForm ∷ Code
  multiLineForm = MultiLine printedExpressions

  printedExpressions ∷ List Code
  printedExpressions = List.fromFoldable
    $ flip printExpression pageWidth <$> expressions

printExpression ∷ Print Expression
printExpression = case _ of
  BinaryOperation operationType leftOperand rightOperand →
    printBinaryOperation { leftOperand, operationType, rightOperand }
  BooleanLiteral b →
    printBooleanLiteral b
  FloatLiteral x →
    printFloatLiteral x
  ForBlock spec body →
    printForBlock { body, spec }
  IfBlock condition positiveBranch →
    printIfBlock { condition, positiveBranch }
  IfElseBlock condition positiveBranch negativeBranch →
    printIfElseBlock { condition, positiveBranch, negativeBranch }
  IntegerLiteral n →
    printIntegerLiteral n
  ProcedureCall name arguments →
    printProcedureCall { arguments, name }
  ProcedureDefinition { name, parameterNames } body →
    printProcedureDefinition { body, name, parameterNames }
  RepeatBlock times body →
    printRepeatBlock { body, times }
  StringLiteral s →
    printStringLiteral s
  UnaryOperation operationType operand →
    printUnaryOperation { operand, operationType }
  ValueReference name →
    printValueReference name
  VariableAssignment name value →
    printVariableAssignment { name, value }

printBinaryOperation
  ∷ Print
      { leftOperand ∷ Expression
      , operationType ∷ BinaryOperationType
      , rightOperand ∷ Expression
      }
printBinaryOperation
  { leftOperand, operationType, rightOperand }
  pageWidth =
  if Code.codeWidth singleLineForm <= pageWidth then singleLineForm
  else multiLineForm
  where
  singleLineForm ∷ Code
  singleLineForm = SingleLine
    $ Code.words printedLeftOperand
        <> List.singleton symbolCodeWord
        <> Code.words printedRightOperand

  multiLineForm ∷ Code
  multiLineForm = MultiLine $ List.fromFoldable
    [ printedLeftOperand
    , SingleLine $ List.singleton symbolCodeWord
    , printedRightOperand
    ]

  symbolCodeWord ∷ CodeWord
  symbolCodeWord = CodeWord
    $ Expression.binaryOperationTypeSymbol operationType

  printedLeftOperand ∷ Code
  printedLeftOperand = printOperand (wrap AssocLeft) leftOperand

  printedRightOperand ∷ Code
  printedRightOperand = printOperand (wrap AssocRight) rightOperand

  printOperand ∷ Associativity → Expression → Code
  printOperand neutralAssociativity operand =
    let
      unwrappedForm = printExpression operand pageWidth

      wrappedForm = SingleLine
        $ Code.wrapInParentheses
        $ Code.words
        $ printExpression operand pageWidth
    in
      case operand of
        BinaryOperation operandOperationType _ _ →
          case
            operandOperationType `Operator.precedenceComparingTo`
              operationType
            of
            EQ →
              if
                Operator.associativity operationType ==
                  neutralAssociativity then
                unwrappedForm
              else wrappedForm
            GT →
              unwrappedForm
            LT →
              wrappedForm
        BooleanLiteral _ →
          unwrappedForm
        FloatLiteral _ →
          unwrappedForm
        IntegerLiteral _ →
          unwrappedForm
        StringLiteral _ →
          unwrappedForm
        ProcedureCall _ arguments →
          if null arguments then unwrappedForm else wrappedForm
        _ →
          unwrappedForm

printForBlock ∷ Print { body ∷ List Expression, spec ∷ ForBlockSpec }
printForBlock { body, spec } pageWidth =
  if
    Code.isSingleLine printedBody
      && Code.codeWidth singleLineForm <= pageWidth then
    singleLineForm
  else multiLineForm
  where
  singleLineForm ∷ Code
  singleLineForm = SingleLine
    $ forCodeWord : Code.words printedSpec <> Code.words printedBody

  multiLineForm ∷ Code
  multiLineForm = MultiLine $ List.fromFoldable
    [ SingleLine $ forCodeWord : Code.words printedSpec
    , Indented printedBody
    ]

  printedSpec ∷ Code
  printedSpec = SingleLine $ List.fromFoldable
    ( [ CodeWord "["
      , CodeWord spec.binder
      , CodeWord $ show spec.initialValue
      , CodeWord $ show spec.terminalValue
      ] <> (if spec.step > 1 then [ CodeWord $ show spec.step ] else [])
        <> [ CodeWord "]" ]
    )

  printedBody ∷ Code
  printedBody = printBlockOfExpressions body pageWidth

  forCodeWord ∷ CodeWord
  forCodeWord = CodeWord Lexing.forKeyword

printIfBlock
  ∷ Print { condition ∷ Expression, positiveBranch ∷ List Expression }
printIfBlock { condition, positiveBranch } pageWidth =
  if
    Code.allSingleLine [ printedCondition, printedPositiveBranch ]
      && Code.codeWidth singleLineForm <= pageWidth then singleLineForm
  else multiLineForm
  where
  singleLineForm ∷ Code
  singleLineForm = SingleLine
    $ CodeWord Lexing.ifKeyword : (Code.words printedCondition)
        <> (Code.words printedPositiveBranch)

  multiLineForm ∷ Code
  multiLineForm = MultiLine $ List.fromFoldable
    [ SingleLine
        $ CodeWord Lexing.ifKeyword : (Code.words printedCondition)
    , Indented printedPositiveBranch
    ]

  printedPositiveBranch ∷ Code
  printedPositiveBranch = printBlockOfExpressions
    positiveBranch
    pageWidth

  printedCondition ∷ Code
  printedCondition =
    let
      printedExpression = printExpression condition pageWidth
      printedWords = Code.words printedExpression
    in
      SingleLine
        if List.length printedWords > 2 then
          Code.wrapInParentheses printedWords
        else printedWords

printIfElseBlock
  ∷ Print
      { condition ∷ Expression
      , negativeBranch ∷ List Expression
      , positiveBranch ∷ List Expression
      }
printIfElseBlock { condition, negativeBranch, positiveBranch } pageWidth =
  if
    Code.allSingleLine
      [ printedCondition, printedNegativeBranch, printedPositiveBranch ]
      && Code.codeWidth singleLineForm <= pageWidth then singleLineForm
  else multiLineForm
  where
  singleLineForm ∷ Code
  singleLineForm = SingleLine
    $ CodeWord Lexing.ifElseKeyword : (Code.words printedCondition)
        <> (Code.words printedPositiveBranch)
        <> (Code.words printedNegativeBranch)

  multiLineForm ∷ Code
  multiLineForm = MultiLine $ List.fromFoldable
    [ SingleLine
        $ CodeWord Lexing.ifElseKeyword : (Code.words printedCondition)
    , Indented printedPositiveBranch
    , Indented printedNegativeBranch
    ]

  printedNegativeBranch ∷ Code
  printedNegativeBranch = printBlockOfExpressions
    negativeBranch
    pageWidth

  printedPositiveBranch ∷ Code
  printedPositiveBranch = printBlockOfExpressions
    positiveBranch
    pageWidth

  printedCondition ∷ Code
  printedCondition = printExpression condition pageWidth

printProcedureCall
  ∷ Print { arguments ∷ List Expression, name ∷ String }
printProcedureCall { arguments, name } pageWidth =
  if
    Code.allSingleLine printedArguments
      && Code.codeWidth singleLineForm <= pageWidth then singleLineForm
  else multiLineForm
  where
  singleLineForm ∷ Code
  singleLineForm = SingleLine
    $ CodeWord name : foldMap Code.words printedArguments

  multiLineForm ∷ Code
  multiLineForm = MultiLine $
    (SingleLine $ List.singleton $ CodeWord name) :
      (Indented <$> printedArguments)

  printedArguments ∷ List Code
  printedArguments = flip printExpression pageWidth <$> arguments

printProcedureDefinition
  ∷ Print
      { body ∷ List Expression
      , name ∷ String
      , parameterNames ∷ List ParameterName
      }
printProcedureDefinition { body, name, parameterNames } pageWidth =
  multiLineForm
  where
  multiLineForm ∷ Code
  multiLineForm = MultiLine $ List.fromFoldable
    [ SingleLine $ CodeWord Lexing.toKeyword
        : CodeWord name
        : parameterNameCodeWords
    , printExpressions body pageWidth
    , SingleLine $ List.singleton $ CodeWord Lexing.endKeyword
    ]

  parameterNameCodeWords ∷ List CodeWord
  parameterNameCodeWords = parameterNames <#> \(ParameterName s) →
    CodeWord $ ":" <> s

printRepeatBlock ∷ Print { body ∷ List Expression, times ∷ Expression }
printRepeatBlock { body, times } pageWidth =
  if
    Code.allSingleLine [ printedBody, printedTimes ]
      && Code.codeWidth singleLineForm <= pageWidth then singleLineForm
  else multiLineForm
  where
  singleLineForm ∷ Code
  singleLineForm = SingleLine
    $ CodeWord Lexing.repeatKeyword : (Code.words printedTimes)
        <> (Code.words printedBody)

  multiLineForm ∷ Code
  multiLineForm = MultiLine $ List.fromFoldable
    [ SingleLine
        $ CodeWord Lexing.repeatKeyword : (Code.words printedTimes)
    , Indented $ printedBody
    ]

  printedBody ∷ Code
  printedBody = printBlockOfExpressions body pageWidth

  printedTimes ∷ Code
  printedTimes = printExpression times pageWidth

printBlockOfExpressions
  ∷ ∀ f. Foldable f ⇒ Functor f ⇒ Print (f Expression)
printBlockOfExpressions expressions pageWidth =
  if null expressions then SingleLine $ List.singleton $ CodeWord "[]"
  else if Code.isSingleLine printedExpressions then
    SingleLine $ (List.singleton leftBracket)
      <> (Code.words printedExpressions)
      <> (List.singleton rightBracket)
  else MultiLine $ List.fromFoldable
    [ SingleLine $ List.singleton leftBracket
    , Indented printedExpressions
    , SingleLine $ List.singleton rightBracket
    ]
  where
  printedExpressions ∷ Code
  printedExpressions = printExpressions expressions pageWidth

  leftBracket ∷ CodeWord
  leftBracket = CodeWord "["

  rightBracket ∷ CodeWord
  rightBracket = CodeWord "]"

printValueReference ∷ Print String
printValueReference name _ =
  SingleLine $ List.singleton $ CodeWord $ ":" <> name

printVariableAssignment ∷ Print { name ∷ String, value ∷ Expression }
printVariableAssignment { name, value } pageWidth =
  SingleLine
    $
      List.fromFoldable
        [ CodeWord Lexing.makeKeyword
        , CodeWord $ "\"" <> name
        ] <> (Code.words $ printExpression value pageWidth)

printBooleanLiteral ∷ Print Boolean
printBooleanLiteral b _ = SingleLine
  $ List.singleton
  $ CodeWord if b then "true" else "false"

printFloatLiteral ∷ Print Number
printFloatLiteral x _ = SingleLine
  $ List.singleton
  $ CodeWord
  $ floatToString x

floatToString ∷ Number → String
floatToString x = NumberFormat.toString x
  <> if Number.trunc x == x then ".0" else ""

printIntegerLiteral ∷ Print Int
printIntegerLiteral n _ = SingleLine
  $ List.singleton
  $ CodeWord
  $ integerToString n

integerToString ∷ Int → String
integerToString = show

{- TODO strings with spaces, multi-line strings -}
printStringLiteral ∷ Print String
printStringLiteral s _ =
  SingleLine $ List.singleton $ CodeWord $ "\"" <> s

printUnaryOperation
  ∷ Print { operand ∷ Expression, operationType ∷ UnaryOperationType }
printUnaryOperation { operand, operationType } pageWidth =
  singleLineForm
  where
  singleLineForm ∷ Code
  singleLineForm = SingleLine
    $ Code.words
    $ Code.prependWith
        symbolCodeWord
        ( SingleLine case operationType, operand of
            Negation, FloatLiteral _ →
              Code.words printedOperand
            Negation, IntegerLiteral _ →
              Code.words printedOperand
            Negation, _ →
              Code.words printedOperand
        )

  symbolCodeWord ∷ CodeWord
  symbolCodeWord = CodeWord
    $ Expression.unaryOperationTypeSymbol operationType

  printedOperand ∷ Code
  printedOperand = printExpression operand pageWidth

