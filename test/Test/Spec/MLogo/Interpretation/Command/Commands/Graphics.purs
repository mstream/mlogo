module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics (spec) where

import Prelude

import Test.Spec (Spec, describe)
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.Clean as Clean
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.ClearScreen as ClearScreen
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.Home as Home
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.PenDown as PenDown
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.PenUp as PenUp

spec ∷ Spec Unit
spec = describe "Graphics" do
  Clean.spec
  ClearScreen.spec
  Home.spec
  PenDown.spec
  PenUp.spec
