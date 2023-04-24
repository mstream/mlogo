module Test.Spec.MLogo.Printing (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.List (List)
import Data.List as List
import Examples (Example(..))
import Examples as Examples
import MLogo.Interpretation.Command.Commands as Commands
import MLogo.Parsing (Expression)
import MLogo.Parsing as Parsing
import MLogo.Printing as Printing
import Parsing (ParseError)
import Parsing as P
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Utils as Utils

spec ∷ Spec Unit
spec = describe "Printing" do
  describe "printExpressions" do
    traverseWithIndex_
      ( \title (Example { ast, source }) →
          printTestCase title source ast
      )
      Examples.examplesByTitle

printTestCase
  ∷ ∀ f
  . Foldable f
  ⇒ Show (f Expression)
  ⇒ String
  → String
  → f Expression
  → Spec Unit
printTestCase title source ast = it title do
  let
    printedSource ∷ String
    printedSource =
      Printing.printExpressions ast

    parsingResult ∷ ParseError \/ List Expression
    parsingResult = P.runParser
      printedSource
      (Parsing.expressions Commands.parsingContext)

    actual ∷ String \/ List Expression
    actual = case parsingResult of
      Left parseError →
        Left $ show parseError
      Right expressions →
        Right expressions

    expected ∷ String \/ List Expression
    expected = Right $ List.fromFoldable ast

  if actual == expected then pure unit
  else
    fail $ "--- error >>> ---\n"
      <> show actual
      <> "\nis not equal to\n"
      <> show expected
      <> "\n--- original source >>> ---\n"
      <> Utils.emphasizeWhitespaces source
      <> "\n--- <<< original source ---"
      <> "\n--- printed source >>> ---\n"
      <> Utils.emphasizeWhitespaces printedSource
      <> "\n--- <<< printed source ---"
      <> "\n--- <<< error ---"

