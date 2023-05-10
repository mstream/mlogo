module Test.Utils
  ( addRedundantParentheses
  , addRedundantSpaces
  , emphasizeWhitespaces
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldM)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen

addRedundantParentheses ∷ String → Gen String
addRedundantParentheses s = do
  n ← Gen.chooseInt 0 2
  pure $ n `charsOf` '(' <> s <> n `charsOf` ')'
  where
  charsOf ∷ Int → Char → String
  charsOf n c = String.fromCodePointArray
    $ (Array.replicate n (String.codePointFromChar c))

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

emphasizeWhitespaces ∷ String → String
emphasizeWhitespaces = emphasizeLineBreaks <<< emphasizeSpaces
  where
  emphasizeSpaces ∷ String → String
  emphasizeSpaces = String.replaceAll
    (Pattern " ")
    (Replacement "␣")

  emphasizeLineBreaks ∷ String → String
  emphasizeLineBreaks = String.replaceAll
    (Pattern "\n")
    (Replacement "⏎\n")
