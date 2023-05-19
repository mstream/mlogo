module MLogo.Printing (printExpression, printExpressions) where

import Prelude

import Data.Foldable (class Foldable, foldMap, null)
import Data.List (List, (:))
import Data.List as List
import Data.Number as Number
import Data.Number.Format as NumberFormat
import Data.String as String
import MLogo.Lexing as Lexing
import MLogo.Parsing.Expression
  ( Expression(..)
  , ForBlockSpec
  , ParameterName(..)
  , UnaryOperationType(..)
  )
import MLogo.Parsing.Expression as Expression
import MLogo.Printing.BinaryOperation as BinaryOperation
import MLogo.Printing.Code (Code(..), CodeWord(..))
import MLogo.Printing.Code as Code
import MLogo.Printing.Print (Print)

printExpressions ∷ ∀ f. Foldable f ⇒ Functor f ⇒ Print (f Expression)
printExpressions expressions printConfig =
  if
    Code.allSingleLine printedExpressions
      && Code.codeWidth singleLineForm <= printConfig.pageWidth then
    singleLineForm
  else multiLineForm
  where
  singleLineForm ∷ Code
  singleLineForm = SingleLine $ foldMap Code.words printedExpressions

  multiLineForm ∷ Code
  multiLineForm = MultiLine printedExpressions

  printedExpressions ∷ List Code
  printedExpressions = List.fromFoldable
    $ flip printExpression printConfig <$> expressions

printExpression ∷ Print Expression
printExpression = case _ of
  BinaryOperation operationType leftOperand rightOperand →
    BinaryOperation.print
      printExpression
      operationType
      leftOperand
      rightOperand
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

printForBlock ∷ Print { body ∷ List Expression, spec ∷ ForBlockSpec }
printForBlock { body, spec } printConfig =
  let
    willFit code = Code.codeWidth code <= printConfig.pageWidth
  in
    if willFit singleLineCode then singleLineCode
    else if willFit singleLineKeywordAndSpecCode then
      MultiLine $ List.fromFoldable
        [ singleLineKeywordAndSpecCode
        , indentedPrintedBody
        ]
    else
      MultiLine $ List.fromFoldable
        [ SingleLine $ List.singleton keywordCodeWord
        , Indented printedSpec
        , indentedPrintedBody
        ]
  where
  singleLineCode = SingleLine $
    keywordCodeWord
      : Code.words printedSpec
      <> Code.words printedBody

  singleLineKeywordAndSpecCode =
    SingleLine $ keywordCodeWord : Code.words printedSpec

  keywordCodeWord ∷ CodeWord
  keywordCodeWord = CodeWord Lexing.forKeyword

  indentedPrintedBody ∷ Code
  indentedPrintedBody = Indented
    $ printBlockOfExpressions
        body
        printConfig { pageWidth = printConfig.pageWidth - 2 }

  printedBody ∷ Code
  printedBody = printBlockOfExpressions body printConfig

  printedSpec ∷ Code
  printedSpec = SingleLine $ List.fromFoldable
    ( [ CodeWord "["
      , CodeWord spec.binder
      , CodeWord $ show spec.initialValue
      , CodeWord $ show spec.terminalValue
      ] <> (if spec.step > 1 then [ CodeWord $ show spec.step ] else [])
        <> [ CodeWord "]" ]
    )

printIfBlock
  ∷ Print { condition ∷ Expression, positiveBranch ∷ List Expression }
printIfBlock { condition, positiveBranch } printConfig =
  if
    Code.allSingleLine [ printedCondition, printedPositiveBranch ]
      && Code.codeWidth singleLineForm <= printConfig.pageWidth then
    singleLineForm
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
    printConfig

  printedCondition ∷ Code
  printedCondition =
    let
      printedExpression = printExpression condition printConfig
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
printIfElseBlock
  { condition, negativeBranch, positiveBranch }
  printConfig =
  let
    willFit code = Code.codeWidth code <= printConfig.pageWidth
  in
    if willFit singleLineCode then singleLineCode
    else if willFit singleLineKeywordAndConditionCode then
      MultiLine $ List.fromFoldable
        [ singleLineKeywordAndConditionCode
        , Indented printedPositiveBranch
        , Indented printedNegativeBranch
        ]
    else
      MultiLine $ List.fromFoldable
        [ SingleLine $ List.singleton keywordCodeWord
        , Indented printedCondition
        , Indented printedPositiveBranch
        , Indented printedNegativeBranch
        ]
  where
  singleLineCode = SingleLine $
    keywordCodeWord
      : Code.words printedCondition
      <> Code.words printedPositiveBranch
      <> Code.words printedNegativeBranch

  singleLineKeywordAndConditionCode =
    SingleLine $ keywordCodeWord : Code.words printedCondition

  keywordCodeWord ∷ CodeWord
  keywordCodeWord = CodeWord Lexing.ifElseKeyword

  printedNegativeBranch ∷ Code
  printedNegativeBranch = printBlockOfExpressions
    negativeBranch
    printConfig

  printedPositiveBranch ∷ Code
  printedPositiveBranch = printBlockOfExpressions
    positiveBranch
    printConfig

  printedCondition ∷ Code
  printedCondition = printExpression condition printConfig

printProcedureCall
  ∷ Print { arguments ∷ List Expression, name ∷ String }
printProcedureCall { arguments, name } printConfig =
  if
    Code.allSingleLine printedArguments
      && Code.codeWidth singleLineForm <= printConfig.pageWidth then
    singleLineForm
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
  printedArguments = flip printExpression printConfig <$> arguments

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
printRepeatBlock { body, times } printConfig =
  let
    willFit code = Code.codeWidth code <= printConfig.pageWidth
  in
    if willFit singleLineCode then singleLineCode
    else if willFit singleLineKeywordAndTimesCode then
      MultiLine $ List.fromFoldable
        [ singleLineKeywordAndTimesCode
        , Indented printedBody
        ]
    else
      MultiLine $ List.fromFoldable
        [ SingleLine $ List.singleton keywordCodeWord
        , Indented printedTimes
        , Indented printedBody
        ]
  where
  singleLineCode = SingleLine $
    keywordCodeWord
      : Code.words printedTimes
      <> Code.words printedBody

  singleLineKeywordAndTimesCode =
    SingleLine $ keywordCodeWord : Code.words printedTimes

  keywordCodeWord ∷ CodeWord
  keywordCodeWord = CodeWord Lexing.repeatKeyword

  printedBody ∷ Code
  printedBody = printBlockOfExpressions body printConfig

  printedTimes ∷ Code
  printedTimes = printExpression times printConfig

printBlockOfExpressions
  ∷ ∀ f. Foldable f ⇒ Functor f ⇒ Print (f Expression)
printBlockOfExpressions expressions printConfig =
  if null expressions then SingleLine $ List.singleton $ CodeWord "[]"
  else if Code.isSingleLine printedExpressionsMinus4 then
    SingleLine $ (List.singleton leftBracketCodeWord)
      <> (Code.words printedExpressionsMinus4)
      <> (List.singleton rightBracketCodeWord)
  else MultiLine $ List.fromFoldable
    [ SingleLine $ List.singleton leftBracketCodeWord
    , Indented $ printExpressions
        expressions
        printConfig { pageWidth = printConfig.pageWidth - 2 }
    , SingleLine $ List.singleton rightBracketCodeWord
    ]
  where
  printedExpressionsMinus4 ∷ Code
  printedExpressionsMinus4 = printExpressions
    expressions
    printConfig { pageWidth = printConfig.pageWidth - 4 }

  leftBracketCodeWord ∷ CodeWord
  leftBracketCodeWord = CodeWord "["

  rightBracketCodeWord ∷ CodeWord
  rightBracketCodeWord = CodeWord "]"

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
floatToString x
  | Number.trunc x == x = NumberFormat.toString x <> ".0"
  | x > 0.0 && x < 1.0 = String.drop 1 (NumberFormat.toString x)
  | x < 0.0 && x > -1.0 = "-" <> String.drop 2 (NumberFormat.toString x)
  | otherwise = NumberFormat.toString x

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

