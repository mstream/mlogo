module Test.Spec.MLogo.Interpretation.Command.Commands (spec) where

import Prelude

import Test.Spec (Spec, describe)
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic as Arithmetic
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics as Graphics
import Test.Spec.MLogo.Interpretation.Command.Commands.WorkspaceManagement as WorkspaceManagement

spec ∷ Spec Unit
spec = describe "Commands" do
  Arithmetic.spec
  Graphics.spec
  WorkspaceManagement.spec
