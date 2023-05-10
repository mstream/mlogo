module MLogo.Printing.Code
  ( Code(..)
  , CodeWord(..)
  , allSingleLine
  , codeToString
  , codeWidth
  , isSingleLine
  , prependWith
  , words
  , wrapInParentheses
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, all, foldMap, foldl, maximum)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.String as String

data Code
  = Indented Code
  | MultiLine (List Code)
  | SingleLine (List CodeWord)

prependWith ∷ CodeWord → Code → Code
prependWith prefix@(CodeWord p) = case _ of
  Indented code →
    Indented $ prependWith prefix code
  MultiLine Nil →
    MultiLine Nil
  MultiLine (code : codes) →
    MultiLine $ prependWith prefix code : codes
  SingleLine Nil →
    SingleLine Nil
  SingleLine ((CodeWord s) : codeWords) →
    SingleLine $ (CodeWord $ p <> s) : codeWords

codeWidth ∷ Code → Int
codeWidth = go 0
  where
  go ∷ Int → Code → Int
  go indentation = case _ of
    Indented code →
      go (indentation + 2) code
    SingleLine Nil →
      indentation
    SingleLine codeWords →
      let
        spacesLength = List.length codeWords - 1
        wordsLength = foldl
          (\acc codeWord → acc + codeWordLength codeWord)
          0
          codeWords
      in
        indentation + spacesLength + wordsLength
    MultiLine codeLines →
      indentation + fromMaybe 0 (maximum $ go indentation <$> codeLines)

isSingleLine ∷ Code → Boolean
isSingleLine = case _ of
  Indented code →
    isSingleLine code
  SingleLine _ →
    true
  MultiLine _ →
    false

allSingleLine ∷ ∀ f. Foldable f ⇒ f Code → Boolean
allSingleLine = all isSingleLine

codeToString ∷ Code → String
codeToString = go 0
  where
  go ∷ Int → Code → String
  go indentation = case _ of
    Indented code →
      go (indentation + 2) code
    SingleLine codeWords →
      indentationToString indentation
        <> String.joinWith
          " "
          (Array.fromFoldable $ codeWordToString <$> codeWords)
    MultiLine codes →
      String.joinWith
        "\n"
        (Array.fromFoldable $ go indentation <$> codes)

words ∷ Code → List CodeWord
words = go Nil
  where
  go ∷ List CodeWord → Code → List CodeWord
  go acc = case _ of
    Indented code →
      go acc code
    SingleLine codeWords →
      codeWords
    MultiLine codes →
      acc <> foldMap (go acc) codes

indentationToString ∷ Int → String
indentationToString n = String.joinWith "" (Array.replicate n " ")

newtype CodeWord = CodeWord String

codeWordLength ∷ CodeWord → Int
codeWordLength (CodeWord s) = String.length s

codeWordToString ∷ CodeWord → String
codeWordToString (CodeWord s) = s

wrapInParentheses ∷ List CodeWord → List CodeWord
wrapInParentheses codeWords = List.singleton (CodeWord "(")
  <> codeWords
  <> List.singleton (CodeWord ")")

