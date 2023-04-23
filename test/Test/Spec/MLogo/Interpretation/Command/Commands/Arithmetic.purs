module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic (spec) where

import Prelude

import Test.Spec (Spec, describe)
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.EqualP as EqualP
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Power as Power
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Quotient as Quotient
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Sin as Sin
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Sum as Sum

spec ∷ Spec Unit
spec = describe "Arithmetic" do
  EqualP.spec
  Power.spec
  Quotient.spec
  Sin.spec
  Sum.spec
