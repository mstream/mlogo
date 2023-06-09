module Test.Spec.MLogo.Lexing (spec) where

import Prelude

import Data.Either (Either(..))
import MLogo.Lexing as Lexing
import Parsing as P
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Types (TestSpec)

spec ∷ TestSpec
spec = describe "Lexing" do
  describe "integer" do
    it "parses a positive integer" do
      let
        actual = P.runParser "123" Lexing.lexer.integer
        expected = Right 123
      actual `shouldEqual` expected

    it "parses a negative integer" do
      let
        actual = P.runParser "-123" Lexing.lexer.integer
        expected = Right (-123)
      actual `shouldEqual` expected

  describe "float" do
    it "parses a positive float" do
      let
        actual = P.runParser "123.5" Lexing.lexer.float
        expected = Right 123.5
      actual `shouldEqual` expected

    it "parses a negative float" do
      let
        actual = P.runParser "-123.5" Lexing.lexer.float
        expected = Right (-123.5)
      actual `shouldEqual` expected

    it "parses a negative float which is smaller than 1" do
      let
        actual = P.runParser "-0.5" Lexing.lexer.float
        expected = Right (-0.5)
      actual `shouldEqual` expected

    it "parses a positive float without a leading zero" do
      let
        actual = P.runParser ".5" Lexing.lexer.float
        expected = Right 0.5
      actual `shouldEqual` expected

    it "parses a positive float in a scientific notation" do
      let
        actual = P.runParser "1e3" Lexing.lexer.float
        expected = Right 1000.0
      actual `shouldEqual` expected

    it "parses a negative float in a scientific notation" do
      let
        actual = P.runParser "-1e3" Lexing.lexer.float
        expected = Right (-1000.0)
      actual `shouldEqual` expected

    it
      "refuses to parse float in a scientific notation containing only an exponent"
      do
        let
          actual = P.runParser "e3" Lexing.lexer.float
        case actual of
          Left _ →
            pure unit
          Right _ →
            fail "should not parse"

    it "parses a string literal" do
      let
        actual = P.runParser "\"abc" Lexing.lexer.stringLiteral
        expected = Right "abc"
      actual `shouldEqual` expected

    it "parses a string literal - otherwise a reserved word" do
      let
        actual = P.runParser "\"if" Lexing.lexer.stringLiteral
        expected = Right "if"
      actual `shouldEqual` expected

