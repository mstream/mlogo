module Test.Spec.MLogo.Interpretation.Command (spec) where

import Prelude

import Test.Spec (Spec, describe)
import Test.Spec.MLogo.Interpretation.Command.Commands as Commands
import Test.Types (TestSpec)

spec ∷ TestSpec
spec = describe "Command" do
  Commands.spec

