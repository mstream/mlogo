module Test.Spec.MLogo.Parsing.Operator (spec) where

import Prelude

import MLogo.Parsing.Expression (BinaryOperationType(..))
import MLogo.Parsing.Expression.Gen as ExpressionGen
import MLogo.Parsing.Operator as Operator
import Test.QuickCheck (Result(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "Operator" do
  describe "isLowerPriorityThan" do
    it "considers addition of lower priority than multiplication"
      do
        let
          actual = Addition
            `Operator.precedenceComparingTo` Multiplication

          expected = LT

        actual `shouldEqual` expected

    it "considers multiplication of lower priority than exponentiation"
      do
        let
          actual = Multiplication
            `Operator.precedenceComparingTo` Exponentiation

          expected = LT

        actual `shouldEqual` expected

    it
      "does not consider multiplication of lower priority than division"
      do
        let
          actual = Multiplication
            `Operator.precedenceComparingTo` Division

          expected = EQ

        actual `shouldEqual` expected

    generativeTestCase Long "maintains the transitivity" do
      firstOperationType ← ExpressionGen.genBinaryOperationType
      secondOperationType ← ExpressionGen.genBinaryOperationType

      let
        originPrecedence ∷ Ordering
        originPrecedence = firstOperationType
          `Operator.precedenceComparingTo` secondOperationType

        inversedPrecedence ∷ Ordering
        inversedPrecedence = secondOperationType
          `Operator.precedenceComparingTo` firstOperationType

        fail ∷ Result
        fail = Failed $ show
          { firstOperationType
          , inversedPrecedence
          , originPrecedence
          , secondOperationType
          }

      pure case originPrecedence of
        EQ →
          if inversedPrecedence /= EQ then fail else Success
        GT →
          if inversedPrecedence /= LT then fail else Success
        LT →
          if inversedPrecedence /= GT then fail else Success

