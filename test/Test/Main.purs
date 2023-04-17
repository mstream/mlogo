module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.MLogo.Interpretation as Interpretation
import Test.Spec.MLogo.Interpretation.Command as Command
import Test.Spec.MLogo.Interpretation.Command.Input as Input
import Test.Spec.MLogo.Parsing as Parsing
import Test.Spec.MLogo.Parsing as Parsing
import Test.Spec.MLogo.Program as Program
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  Command.spec
  Input.spec
  Interpretation.spec
  Parsing.spec
  Program.spec
