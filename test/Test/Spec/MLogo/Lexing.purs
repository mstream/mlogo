module Test.Spec.MLogo.Lexing (spec) where

import Prelude

import Data.Either (Either(..))
import MLogo.Lexing as Lexing
import Parsing as P
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
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
