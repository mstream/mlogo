module Test.Spec.MLogo.Parsing (spec) where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable, foldM)
import Data.Number as Number
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Traversable (class Traversable, sequence)
import Effect.Class (liftEffect)
import MLogo.Parsing (Expression(..))
import MLogo.Parsing as Parsing
import Parsing as P
import Test.QuickCheck (Result(..), quickCheckGen)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec, describe, it)

spec ∷ Spec Unit
spec = describe "Parsing" do
  describe "expression" do
    arithmeticalBinaryOperatorTestCase
      "+"
      Addition

    arithmeticalBinaryOperatorTestCase
      "*"
      Multiplication

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
            (NumberLiteral leftOperand)
            (NumberLiteral rightOperand)
    )

genFloat ∷ Gen String
genFloat = do
  n1 ← Gen.chooseInt 0 9
  n2 ← Gen.chooseInt 0 9
  pure $ show n1 <> "." <> show n2

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
    source ← addRedundantSpaces parts

    let
      actual = case P.runParser source Parsing.expression of
        Left parseError →
          Left $ P.parseErrorMessage parseError
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
          <> String.replaceAll
            (Pattern " ")
            (Replacement "␣")
            source
          <> "\n--- <<< source ---"
          <> "\n--- <<< error ---"

addRedundantSpaces ∷ ∀ f. Foldable f ⇒ f String → Gen String
addRedundantSpaces parts = genSpaces >>= \spaces → foldM f spaces parts
  where
  f ∷ String → String → Gen String
  f acc part = do
    spaces ← genSpaces
    pure $ acc <> part <> spaces

  genSpaces ∷ Gen String
  genSpaces = Gen.chooseInt 0 2 <#> \n →
    String.joinWith "" (Array.replicate n " ")
