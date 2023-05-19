module Test.Spec.MLogo.Interpretation.Command.Commands.Graphics (spec) where

import Prelude

import Test.Spec (describe)
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.Clean as Clean
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.ClearScreen as ClearScreen
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.Home as Home
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.PenDown as PenDown
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.PenUp as PenUp
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.SetPenColor as SetPenColor
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.SetX as SetX
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.SetXY as SetXY
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.SetY as SetY
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.XCor as XCor
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics.YCor as YCor
import Test.Types (TestSpec)

spec ∷ TestSpec
spec = describe "Graphics" do
  Clean.spec
  ClearScreen.spec
  Home.spec
  PenDown.spec
  PenUp.spec
  SetPenColor.spec
  SetX.spec
  SetXY.spec
  SetY.spec
  XCor.spec
  YCor.spec
