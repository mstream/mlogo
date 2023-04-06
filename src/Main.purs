module Main where

import Prelude

import Data.List as List
import Effect (Effect)
import Effect.Class.Console as Console
import MLogo.Interpretation as Interpretation

main :: Effect Unit
main = do
  Console.info $ show $ Interpretation.run $ List.fromFoldable []

