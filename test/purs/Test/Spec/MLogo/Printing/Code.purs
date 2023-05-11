module Test.Spec.MLogo.Printing.Code (spec) where

import Prelude

import Data.List as List
import MLogo.Printing.Code (Code(..), CodeWord(..))
import MLogo.Printing.Code as Code
import MLogo.Printing.Code.Gen as CodeGen
import Test.QuickCheck (Result(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail)
import Test.Types (TestSpec)
import Test.Utils (generativeTestCase)
import Test.Utils as Utils

spec ∷ TestSpec
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

    codeWidthTestCase
      "multi line"
      ( MultiLine $ List.fromFoldable
          [ SingleLine $ List.fromFoldable
              [ CodeWord "abc"
              , CodeWord "de"
              , CodeWord "f"
              ]
          , SingleLine $ List.fromFoldable
              [ CodeWord "ghij"
              , CodeWord "klm"
              , CodeWord "no"
              , CodeWord "p"
              ]
          ]
      )
      13

    codeWidthTestCase
      "multi line, indented"
      ( Indented $ MultiLine $ List.fromFoldable
          [ SingleLine $ List.fromFoldable
              [ CodeWord "abc"
              , CodeWord "de"
              , CodeWord "f"
              ]
          , SingleLine $ List.fromFoldable
              [ CodeWord "ghij"
              , CodeWord "klm"
              , CodeWord "no"
              , CodeWord "p"
              ]
          ]
      )
      15

    generativeTestCase "indentation increases code width by 2" do
      code ← CodeGen.genCode

      let
        originWidth ∷ Int
        originWidth = Code.codeWidth code

        indentedWidth ∷ Int
        indentedWidth = Code.codeWidth $ Indented code

        fail ∷ Result
        fail = Failed $ show
          { indentedWidth
          , originWidth
          }

      pure if indentedWidth - originWidth == 2 then Success else fail

codeWidthTestCase ∷ String → Code → Int → TestSpec
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

