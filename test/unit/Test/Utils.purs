module Test.Utils
  ( TestLength(..)
  , addRedundantParentheses
  , addRedundantSpaces
  , emphasizeWhitespaces
  , generativeTestCase
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldM)
import Data.Maybe (maybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Effect (Effect)
import Effect.Class (liftEffect)
import Node.Process as Process
import Test.QuickCheck (Result, quickCheckGen')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Test.Spec (it)
import Test.Types (TestSpec)

data TestLength = Long | Short

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

generativeTestCase ∷ TestLength → String → Gen Result → TestSpec
generativeTestCase testLength title property = do
  shouldRun ← liftEffect $ not <$> checkShouldSkip
  when
    shouldRun
    (it title (liftEffect $ quickCheckGen' iterations property))
  where
  checkShouldSkip ∷ Effect Boolean
  checkShouldSkip = maybe false (_ == "true")
    <$> Process.lookupEnv "SKIP_GENERATIVE_TESTS"

  iterations ∷ Int
  iterations = case testLength of
    Short →
      10
    Long →
      100
