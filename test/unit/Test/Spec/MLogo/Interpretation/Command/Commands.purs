module Test.Spec.MLogo.Interpretation.Command.Commands (spec) where

import Prelude

import Test.Spec (describe)
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic as Arithmetic
import Test.Spec.MLogo.Interpretation.Command.Commands.ControlStructures as ControlStructures
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics as Graphics
import Test.Spec.MLogo.Interpretation.Command.Commands.WorkspaceManagement as WorkspaceManagement
import Test.Types (TestSpec)

spec ∷ TestSpec
spec = describe "Commands" do
  Arithmetic.spec
  ControlStructures.spec
  Graphics.spec
  WorkspaceManagement.spec
