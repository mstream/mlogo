module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.MLogo.Interpretation as Interpretation
import Test.Spec.MLogo.Interpretation.Command.Commands as Commands
import Test.Spec.MLogo.Interpretation.Types as Types
import Test.Spec.MLogo.Parsing as Parsing
import Test.Spec.MLogo.Printing as Printing
import Test.Spec.MLogo.Program as Program
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  Commands.spec
  Interpretation.spec
  Parsing.spec
  Printing.spec
  Program.spec
  Types.spec
