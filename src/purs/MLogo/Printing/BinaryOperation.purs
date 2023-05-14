module MLogo.Printing.BinaryOperation
  ( BinaryOperationCollection
  , collect
  , print
  ) where

import Prelude

import Data.Foldable (all, foldMap, foldl, foldr, null)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as ListNonEmpty
import Data.Newtype (wrap)
import Data.Tuple.Nested (type (/\), (/\))
import MLogo.Parsing.Expression (BinaryOperationType, Expression(..))
import MLogo.Parsing.Expression as Expression
import MLogo.Parsing.Operator as Operator
import MLogo.Printing.Code (Code(..), CodeWord(..))
import MLogo.Printing.Code as Code
import MLogo.Printing.Print (Print, PrintConfig)
import Parsing.Expr (Assoc(..))

{- TODO: rewrite the entire module as it is messy and inefficient -}

type BinaryOperationCollection =
  { head ∷ Expression
  , tail ∷ NonEmptyList (BinaryOperationType /\ Expression)
  }

collect
  ∷ Boolean
  → BinaryOperationType
  → Expression
  → Expression
  → BinaryOperationCollection
collect simplifyBinaryOperations operationType = case _, _ of
  BinaryOperation lot llo lro, BinaryOperation rot rlo rro →
    let
      left = collectLeftBinaryOperationOperand lot llo lro
      right = collectRightBinaryOperationOperand rot rlo rro
    in
      { head: left.head
      , tail: foldr ListNonEmpty.cons right left.tail
      }

  BinaryOperation lot llo lro, otherRightOperand →
    let
      { head, tail } = collectLeftBinaryOperationOperand lot llo lro
    in
      { head
      , tail: ListNonEmpty.snoc'
          tail
          (operationType /\ otherRightOperand)
      }
  otherLeftOperand, BinaryOperation rot rlo rro →
    { head: otherLeftOperand
    , tail: collectRightBinaryOperationOperand rot rlo rro
    }
  otherLeftOperand, otherRightOperand →
    { head: otherLeftOperand
    , tail: ListNonEmpty.singleton $ operationType /\ otherRightOperand
    }
  where
  collectLeftBinaryOperationOperand
    ∷ BinaryOperationType
    → Expression
    → Expression
    → { head ∷ Expression
      , tail ∷ List (BinaryOperationType /\ Expression)
      }
  collectLeftBinaryOperationOperand lot llo lro =
    let
      samePrecendencesAsParent =
        lot `Operator.precedenceComparingTo` operationType == EQ

      areAssociative = Operator.isAssociative operationType
        && Operator.isAssociative lot

      isParentLeftAssociable =
        Operator.associativity operationType == (wrap AssocLeft)

    in
      if
        samePrecendencesAsParent
          && simplifyBinaryOperations
          && (areAssociative || isParentLeftAssociable) then
        let
          leftCollection = collect simplifyBinaryOperations lot llo lro
        in
          { head: leftCollection.head
          , tail: ListNonEmpty.toList leftCollection.tail
          }
      else
        { head: BinaryOperation lot llo lro
        , tail: Nil
        }

  collectRightBinaryOperationOperand
    ∷ BinaryOperationType
    → Expression
    → Expression
    → NonEmptyList (BinaryOperationType /\ Expression)
  collectRightBinaryOperationOperand rot rlo rro =
    let
      samePrecendencesAsParent =
        rot `Operator.precedenceComparingTo` operationType == EQ

      areAssociative = Operator.isAssociative operationType
        && Operator.isAssociative rot

      isParentRightAssociable =
        Operator.associativity operationType == (wrap AssocRight)

    in
      if
        samePrecendencesAsParent
          && simplifyBinaryOperations
          && (areAssociative || isParentRightAssociable) then
        let
          rightCollection = collect simplifyBinaryOperations rot rlo rro
        in
          ListNonEmpty.singleton (operationType /\ rightCollection.head)
            <> rightCollection.tail
      else
        ListNonEmpty.singleton
          $ operationType /\ BinaryOperation rot rlo rro

print
  ∷ Print Expression
  → BinaryOperationType
  → Expression
  → Expression
  → PrintConfig
  → Code
print printExpression operationType leftOperand rightOperand printConfig =
  if
    printedHeadWidth + printedTailWidth <= printConfig.pageWidth
      && Code.isSingleLine printedHead
      && all Code.isSingleLine printedTailElements then
    SingleLine $ Code.words printedHead
      <> foldMap Code.words (List.reverse printedTailElements)
  else
    MultiLine
      $ (SingleLine $ Code.words printedHead)
          :
            ( foldMap
                ( \(tailOperationType /\ tailExpression) →
                    ( List.singleton $ SingleLine $ List.singleton
                        $ CodeWord
                        $ Expression.binaryOperationTypeSymbol
                            tailOperationType
                    )
                      <>
                        ( List.singleton
                            $ SingleLine
                            $ Code.words
                            $ printOperand
                                printExpression
                                operationType
                                tailExpression
                                printConfig
                        )
                )
                collection.tail
            )
  where
  printedTailWidth ∷ Int
  printedTailWidth = foldl
    (\acc code → acc + Code.codeWidth code)
    (List.length printedTailElements - 1)
    printedTailElements

  printedTailElements ∷ List Code
  printedTailElements = foldl
    ( \acc (tailOperationType /\ tailExpression) →
        let
          code = SingleLine
            $
              CodeWord
                (Expression.binaryOperationTypeSymbol tailOperationType)
                :
                  ( Code.words
                      $ printOperand
                          printExpression
                          tailOperationType
                          tailExpression
                          printConfig
                  )
        in
          code : acc
    )
    Nil
    collection.tail

  printedHeadWidth ∷ Int
  printedHeadWidth = Code.codeWidth printedHead

  printedHead ∷ Code
  printedHead = printOperand
    printExpression
    operationType
    collection.head
    printConfig

  collection ∷ BinaryOperationCollection
  collection = collect
    printConfig.simplifyBinaryOperations
    operationType
    leftOperand
    rightOperand

printOperand ∷ Print Expression → BinaryOperationType → Print Expression
printOperand
  printExpression
  parentOperationType
  operandExpression
  printConfig =
  case operandExpression of
    BinaryOperation operandOperationType _ _ →
      let
        isOperanOfLowerPrecedenceThanParent =
          operandOperationType `Operator.precedenceComparingTo`
            parentOperationType == LT

        areOperatorsAssociative = printConfig.simplifyBinaryOperations
          && Operator.isAssociative parentOperationType
          && Operator.isAssociative operandOperationType
      in
        if
          isOperanOfLowerPrecedenceThanParent
            || not areOperatorsAssociative then wrappedForm
        else unwrappedForm
    ProcedureCall _ arguments →
      if null arguments then unwrappedForm else wrappedForm
    _ →
      unwrappedForm
  where
  unwrappedForm ∷ Code
  unwrappedForm = printExpression operandExpression printConfig

  wrappedForm ∷ Code
  wrappedForm = SingleLine
    $ Code.wrapInParentheses
    $ Code.words
    $ printExpression
        operandExpression
        printConfig { pageWidth = printConfig.pageWidth - 4 }
