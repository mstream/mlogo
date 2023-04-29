module MLogo.Parsing.Expression.Gen
  ( genBoolean
  , genExpression
  , genFloat
  , genInteger
  , genString
  ) where

import Prelude

import Control.Lazy (class Lazy)
import Control.Lazy as Lazy
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array.NonEmpty as ArrayNE
import Data.Char.Gen as GenChar
import Data.Int as Int
import Data.String as String
import Data.String.Gen as StringGen
import MLogo.Parsing.Expression (Expression(..))

genExpression
  ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ Lazy (m Expression) ⇒ m Expression
genExpression = Gen.resize (min 3) (Gen.sized go)
  where
  go ∷ Int → m Expression
  go size = if size == 0 then genLeaf else Gen.resize (_ - 1) genBranch

  genLeaf ∷ m Expression
  genLeaf = Gen.oneOf $ ArrayNE.cons'
    genBooleanLiteral
    [ genFloatLiteral
    , genIntegerLiteral
    , genStringLiteral
    , genValueReference
    ]

  genBranch ∷ m Expression
  genBranch = Gen.oneOf $ ArrayNE.cons'
    genAddition
    [ genDivision
    , genEquation
    , genExponentiation
    , genIfBlock
    , genMultiplication
    ]

  genAddition ∷ m Expression
  genAddition = genBinaryOperation Addition

  genBooleanLiteral ∷ m Expression
  genBooleanLiteral = BooleanLiteral <$> genBoolean

  genEquation ∷ m Expression
  genEquation = genBinaryOperation Equation

  genExponentiation ∷ m Expression
  genExponentiation = genBinaryOperation Exponentiation

  genIfBlock ∷ m Expression
  genIfBlock = do
    condition ← Lazy.defer \_ → genExpression
    positiveBranch ← Gen.unfoldable $ Lazy.defer \_ → genExpression
    pure $ IfBlock condition positiveBranch

  genIntegerLiteral ∷ m Expression
  genIntegerLiteral = IntegerLiteral <$> genInteger

  genFloatLiteral ∷ m Expression
  genFloatLiteral = FloatLiteral <$> genFloat

  genStringLiteral ∷ m Expression
  genStringLiteral = StringLiteral <$> genString

  genDivision ∷ m Expression
  genDivision = Lazy.defer \_ → genBinaryOperation Division

  genMultiplication ∷ m Expression
  genMultiplication = Lazy.defer \_ → genBinaryOperation Multiplication

  genValueReference ∷ m Expression
  genValueReference = ValueReference <$> genIdentifier

  genIdentifier ∷ m String
  genIdentifier = do
    firstChar ← GenChar.genAlpha

    otherChars ← Gen.unfoldable
      $ Gen.choose GenChar.genAlpha GenChar.genDigitChar

    pure
      $ String.fromCodePointArray
      $ String.codePointFromChar <$> ([ firstChar ] <> otherChars)

  genBinaryOperation
    ∷ (Expression → Expression → Expression) → m Expression
  genBinaryOperation make = do
    leftOperand ← Lazy.defer \_ → genExpression
    rightOperand ← Lazy.defer \_ → genExpression
    pure $ make leftOperand rightOperand

genBoolean ∷ ∀ m. MonadGen m ⇒ m Boolean
genBoolean = Gen.chooseBool

genFloat ∷ ∀ m. MonadGen m ⇒ m Number
genFloat = do
  isPositive ← Gen.chooseBool
  n1 ← Gen.chooseInt 0 99
  n2 ← Gen.chooseInt 0 9
  pure $ (Int.toNumber n1 + (Int.toNumber n2) / 10.0) *
    (Int.toNumber if isPositive then 1 else -1)

genInteger ∷ ∀ m. MonadGen m ⇒ m Int
genInteger = Gen.chooseInt 0 99

genString ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m String
genString = StringGen.genAlphaString
