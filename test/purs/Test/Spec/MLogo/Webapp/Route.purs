module Test.Spec.MLogo.WebApp.Route (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import MLogo.WebApp.Route (Route(..))
import MLogo.WebApp.Route as Route
import Test.QuickCheck ((===))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "Route" do
  it "prints home route properly" do
    Route.print Home `shouldEqual` "/"

  it "prints sandbox route properly - without source" do
    let
      actual = Route.print (Sandbox { s: Nothing })

    actual `shouldEqual` "/sandbox.html"

  it "parses home route properly - with leading slash" do
    Route.parse "/" `shouldEqual` Right Home

  it "parses home route properly - without leading slash" do
    Route.parse "" `shouldEqual` Right Home

  it "prints sandbox route properly - with source" do
    let
      actual = Route.print (Sandbox { s: Just "fd 10" })

    actual `shouldEqual` "/sandbox.html?s=GYEwBAjADEA"

  generativeTestCase Long "codes routes properly" do
    route ← Route.genRoute
    pure $ Route.parse (Route.print route) === Right route
