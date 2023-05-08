module MLogo.Printing
  ( Code(..)
  , CodeWord(..)
  , codeWidth
  , codeToString
  , printExpressions
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable
  ( class Foldable
  , all
  , foldMap
  , foldl
  , maximum
  , null
  )
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.Number as Number
import Data.Number.Format as NumberFormat
import Data.String as String
import MLogo.Lexing as Lexing
import MLogo.Parsing.Expression
  ( BinaryOperationType
  , Expression(..)
  , ForBlockSpec
  , ParameterName(..)
  , UnaryOperationType
  )
import MLogo.Parsing.Expression as Expression
import MLogo.Parsing.Operator as Operator

data Code
  = Indented Code
  | SingleLine (List CodeWord)
  | MultiLine (List Code)

words ∷ Code → List CodeWord
words = go Nil
  where
  go ∷ List CodeWord → Code → List CodeWord
  go acc = case _ of
    Indented code →
      go acc code
    SingleLine codeWords →
      codeWords
    MultiLine codes →
      acc <> foldMap (go acc) codes

codeWidth ∷ Code → Int
codeWidth = go 0
  where
  go ∷ Int → Code → Int
  go indentation = case _ of
    Indented code →
      go (indentation + 2) code
    SingleLine Nil →
      indentation
    SingleLine codeWords →
      let
        spacesLength = List.length codeWords - 1
        wordsLength = foldl
          (\acc codeWord → acc + codeWordLength codeWord)
          0
          codeWords
      in
        indentation + spacesLength + wordsLength
    MultiLine codeLines →
      indentation + fromMaybe 0 (maximum $ go indentation <$> codeLines)

newtype CodeWord = CodeWord String

codeWordLength ∷ CodeWord → Int
codeWordLength (CodeWord s) = String.length s

isSingleLine ∷ Code → Boolean
isSingleLine = case _ of
  Indented code →
    isSingleLine code
  SingleLine _ →
    true
  MultiLine _ →
    false

codeToString ∷ Code → String
codeToString = go 0
  where
  go ∷ Int → Code → String
  go indentation = case _ of
    Indented code →
      go (indentation + 2) code
    SingleLine codeWords →
      indentationToString indentation
        <> String.joinWith
          " "
          (Array.fromFoldable $ codeWordToString <$> codeWords)
    MultiLine codes →
      String.joinWith
        "\n"
        (Array.fromFoldable $ go indentation <$> codes)

codeWordToString ∷ CodeWord → String
codeWordToString (CodeWord s) = s

indentationToString ∷ Int → String
indentationToString n = String.joinWith "" (Array.replicate n " ")

type Print a = a → Int → Code

printExpressions ∷ ∀ f. Foldable f ⇒ Functor f ⇒ Print (f Expression)
printExpressions expressions pageWidth =
  if
    allSingleLine printedExpressions
      && codeWidth singleLineForm <= pageWidth then singleLineForm
  else multiLineForm
  where
  singleLineForm ∷ Code
  singleLineForm = SingleLine $ foldMap words printedExpressions

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
  if codeWidth singleLineForm <= pageWidth then singleLineForm
  else multiLineForm
  where
  singleLineForm ∷ Code
  singleLineForm = SingleLine
    $ words printedLeftOperand
        <> List.singleton symbolCodeWord
        <> words printedRightOperand

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
  printedLeftOperand = printOperand leftOperand

  printedRightOperand ∷ Code
  printedRightOperand = printOperand rightOperand

  printOperand ∷ Expression → Code
  printOperand = case _ of
    op@(BinaryOperation childOperationType _ _) →
      if childOperationType `Operator.isLowerPriorityThan` operationType then
        SingleLine
          $ wrapInParentheses
          $ words
          $ printExpression op pageWidth
      else printExpression op pageWidth
    pc@(ProcedureCall _ args) →
      if List.length args > 0 then
        SingleLine
          $ wrapInParentheses
          $ words
          $ printExpression pc pageWidth
      else printExpression pc pageWidth
    other →
      printExpression other pageWidth

  wrapInParentheses ∷ List CodeWord → List CodeWord
  wrapInParentheses codeWords = List.singleton (CodeWord "(")
    <> codeWords
    <> List.singleton (CodeWord ")")

printForBlock ∷ Print { body ∷ List Expression, spec ∷ ForBlockSpec }
printForBlock { body, spec } pageWidth =
  if isSingleLine printedBody && codeWidth singleLineForm <= pageWidth then
    singleLineForm
  else multiLineForm
  where
  singleLineForm ∷ Code
  singleLineForm = SingleLine
    $ forCodeWord : words printedSpec <> words printedBody

  multiLineForm ∷ Code
  multiLineForm = MultiLine $ List.fromFoldable
    [ SingleLine $ forCodeWord : words printedSpec
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
    allSingleLine [ printedCondition, printedPositiveBranch ] &&
      codeWidth singleLineForm <= pageWidth then singleLineForm
  else multiLineForm
  where
  singleLineForm ∷ Code
  singleLineForm = SingleLine
    $ CodeWord Lexing.ifKeyword : (words printedCondition)
        <> (words printedPositiveBranch)

  multiLineForm ∷ Code
  multiLineForm = MultiLine $ List.fromFoldable
    [ SingleLine $ CodeWord Lexing.ifKeyword : (words printedCondition)
    , Indented printedPositiveBranch
    ]

  printedPositiveBranch ∷ Code
  printedPositiveBranch = printBlockOfExpressions
    positiveBranch
    pageWidth

  printedCondition ∷ Code
  printedCondition = printExpression condition pageWidth

printIfElseBlock
  ∷ Print
      { condition ∷ Expression
      , negativeBranch ∷ List Expression
      , positiveBranch ∷ List Expression
      }
printIfElseBlock { condition, negativeBranch, positiveBranch } pageWidth =
  if
    allSingleLine
      [ printedCondition, printedNegativeBranch, printedPositiveBranch ]
      && codeWidth singleLineForm <= pageWidth then singleLineForm
  else multiLineForm
  where
  singleLineForm ∷ Code
  singleLineForm = SingleLine
    $ CodeWord Lexing.ifElseKeyword : (words printedCondition)
        <> (words printedPositiveBranch)
        <> (words printedNegativeBranch)

  multiLineForm ∷ Code
  multiLineForm = MultiLine $ List.fromFoldable
    [ SingleLine
        $ CodeWord Lexing.ifElseKeyword : (words printedCondition)
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
    allSingleLine printedArguments && codeWidth singleLineForm <=
      pageWidth then singleLineForm
  else multiLineForm
  where
  singleLineForm ∷ Code
  singleLineForm = SingleLine
    $ CodeWord name : foldMap words printedArguments

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
    allSingleLine [ printedBody, printedTimes ] &&
      codeWidth singleLineForm <= pageWidth then singleLineForm
  else multiLineForm
  where
  singleLineForm ∷ Code
  singleLineForm = SingleLine
    $ CodeWord Lexing.repeatKeyword : (words printedTimes)
        <> (words printedBody)

  multiLineForm ∷ Code
  multiLineForm = MultiLine $ List.fromFoldable
    [ SingleLine $ CodeWord Lexing.repeatKeyword : (words printedTimes)
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
  else if isSingleLine printedExpressions then
    SingleLine $ (List.singleton leftBracket)
      <> (words printedExpressions)
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
        , CodeWord name
        ] <> (words $ printExpression value pageWidth)

printBooleanLiteral ∷ Print Boolean
printBooleanLiteral b _ = SingleLine
  $ List.singleton
  $ CodeWord if b then "true" else "false"

printFloatLiteral ∷ Print Number
printFloatLiteral x _ = SingleLine
  $ List.singleton
  $ CodeWord
  $ NumberFormat.toString x <> if Number.trunc x == x then ".0" else ""

printIntegerLiteral ∷ Print Int
printIntegerLiteral n _ = SingleLine
  $ List.singleton
  $ CodeWord
  $ show n

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
  singleLineForm = SingleLine $ symbolCodeWord : words printedOperand

  symbolCodeWord ∷ CodeWord
  symbolCodeWord = CodeWord
    $ Expression.unaryOperationTypeSymbol operationType

  printedOperand ∷ Code
  printedOperand = printExpression operand pageWidth

allSingleLine ∷ ∀ f. Foldable f ⇒ f Code → Boolean
allSingleLine = all isSingleLine
