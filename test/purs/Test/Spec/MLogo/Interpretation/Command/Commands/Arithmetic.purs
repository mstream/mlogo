module Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic (spec) where

import Prelude

import Test.Spec (describe)
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Abs as Abs
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Cos as Cos
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Difference as Difference
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.EqualP as EqualP
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Minus as Minus
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Power as Power
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Product as Product
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Quotient as Quotient
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Random as Random
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.ReRandom as ReRandom
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Sin as Sin
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic.Sum as Sum
import Test.Types (TestSpec)

spec ∷ TestSpec
spec = describe "Arithmetic" do
  Abs.spec
  Cos.spec
  Difference.spec
  EqualP.spec
  Minus.spec
  Power.spec
  Product.spec
  Random.spec
  ReRandom.spec
  Quotient.spec
  Sin.spec
  Sum.spec
