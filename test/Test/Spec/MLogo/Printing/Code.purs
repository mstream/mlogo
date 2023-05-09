module Test.Spec.MLogo.Printing.Code (spec) where

import Prelude

import Data.List as List
import MLogo.Printing.Code (Code(..), CodeWord(..))
import MLogo.Printing.Code as Code
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Utils as Utils

spec ∷ Spec Unit
spec = describe "Code" do
  describe "codeWidth" do
    codeWidthTestCase
      "single line, three words of different lengths"
      ( SingleLine $ List.fromFoldable
          [ CodeWord "abc"
          , CodeWord "de"
          , CodeWord "f"
          ]
      )
      8

    codeWidthTestCase
      "single line, three words of different lengths, indented"
      ( Indented $ SingleLine $ List.fromFoldable
          [ CodeWord "abc"
          , CodeWord "de"
          , CodeWord "f"
          ]
      )
      10

codeWidthTestCase ∷ String → Code → Int → Spec Unit
codeWidthTestCase title code expected =
  it ("calculates the width of \"" <> title <> "\" code") do
    let
      actual = Code.codeWidth code
    if actual == expected then pure unit
    else
      fail $ "--- error >>> ---\n"
        <> show actual
        <> "\nis not equal to\n"
        <> show expected
        <> "\n--- printed code >>> ---\n"
        <> (Utils.emphasizeWhitespaces $ Code.codeToString code)
        <> "\n--- <<< printed code ---"
        <> "\n--- <<< error ---"

