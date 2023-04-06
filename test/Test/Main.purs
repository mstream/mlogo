module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.MLogo.Interpretation as Interpretation
import Test.Spec.MLogo.Lexing as Lexing
import Test.Spec.MLogo.Parsing as Parsing
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  Interpretation.spec
  Lexing.spec
  Parsing.spec

