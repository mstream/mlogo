module Test.Spec.MLogo.Parsing.Operator (spec) where

import Prelude

import MLogo.Parsing.Expression (BinaryOperationType(..))
import MLogo.Parsing.Operator as Operator
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Operator" do
  describe "isLowerPriorityThan" do
    it "considers addition of lower priority than multiplication"
      do
        let
          actual = Addition `Operator.isLowerPriorityThan`
            Multiplication
          expected = true

        actual `shouldEqual` expected

    it "considers multiplication of lower priority than exponentiation"
      do
        let
          actual = Multiplication `Operator.isLowerPriorityThan`
            Exponentiation
          expected = true

        actual `shouldEqual` expected

    it
      "does not consider multiplication of lower priority than division"
      do
        let
          actual = Multiplication `Operator.isLowerPriorityThan`
            Division
          expected = false

        actual `shouldEqual` expected

