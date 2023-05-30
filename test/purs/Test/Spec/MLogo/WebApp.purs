module Test.Spec.MLogo.WebApp (spec) where

import Test.Spec (describe)
import Test.Spec.MLogo.WebApp.Route (spec) as Route
import Test.Types (TestSpec)

spec ∷ TestSpec
spec = describe "WebApp" do
  Route.spec
