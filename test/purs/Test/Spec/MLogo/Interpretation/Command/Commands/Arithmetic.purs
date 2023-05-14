module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic (spec) where

import Prelude

import Test.Spec (describe)
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Difference as Difference
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.EqualP as EqualP
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Minus as Minus
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Power as Power
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Quotient as Quotient
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Sin as Sin
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Sum as Sum
import Test.Types (TestSpec)

spec ∷ TestSpec
spec = describe "Arithmetic" do
  Difference.spec
  EqualP.spec
  Minus.spec
  Power.spec
  Quotient.spec
  Sin.spec
  Sum.spec
