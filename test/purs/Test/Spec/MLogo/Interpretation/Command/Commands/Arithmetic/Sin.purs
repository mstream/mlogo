module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Sin
  ( spec
  ) where

import Prelude

import Test.Spec (describe)
import Test.Types (TestSpec)

spec ∷ TestSpec
spec = describe "sin" do
  describe "interpret" do
    pure unit
