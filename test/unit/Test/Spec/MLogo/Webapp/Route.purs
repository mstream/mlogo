module Test.Spec.MLogo.WebApp.Route (spec) where

import Prelude

import Control.Alternative ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.NonEmpty as StringNonEmpty
import MLogo.WebApp.BaseUrl (BaseUrl)
import MLogo.WebApp.BaseUrl as BaseUrl
import MLogo.WebApp.Route (Route(..))
import MLogo.WebApp.Route as Route
import Test.QuickCheck (Result(..), (===))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)
import Type.Proxy (Proxy(..))

rootBasePath ∷ BaseUrl
rootBasePath = BaseUrl.fromSegments []

prefixBasePath ∷ BaseUrl
prefixBasePath = BaseUrl.fromSegments
  [ StringNonEmpty.nes (Proxy ∷ Proxy "base")
  , StringNonEmpty.nes (Proxy ∷ Proxy "path")
  ]

spec ∷ TestSpec
spec = describe "Route" do
  describe "print" do
    it "prints home route properly - root base path" do
      let
        actual = Route.print rootBasePath Home
        expected = "/"

      actual `shouldEqual` expected

    it "prints static asset route properly - root base path" do
      let
        actual = Route.print rootBasePath (StaticAsset "abc.svg")
        expected = "/abc.svg"

      actual `shouldEqual` expected

    it "prints static asset route properly - prefix base path" do
      let
        actual = Route.print prefixBasePath (StaticAsset "abc.svg")
        expected = "/base/path/abc.svg"

      actual `shouldEqual` expected

  it "prints home route properly - prefix base path" do
    let
      actual = Route.print prefixBasePath Home
      expected = "/base/path"

    actual `shouldEqual` expected

  it "prints sandbox route properly - root base path, without source"
    do
      let
        actual = Route.print rootBasePath (Sandbox { s: Nothing })
        expected = "/sandbox"

      actual `shouldEqual` expected

  it "prints sandbox route properly - prefix base path, without source"
    do
      let
        actual = Route.print prefixBasePath (Sandbox { s: Nothing })
        expected = "/base/path/sandbox"

      actual `shouldEqual` expected

  it "prints sandbox route properly - root path, with source" do
    let
      actual = Route.print rootBasePath (Sandbox { s: Just "fd 10" })
      expected = "/sandbox?s=GYEwBAjADEA"

    actual `shouldEqual` expected

  it "prints sandbox route properly - prefix base path, with source" do
    let
      actual = Route.print prefixBasePath (Sandbox { s: Just "fd 10" })
      expected = "/base/path/sandbox?s=GYEwBAjADEA"

    actual `shouldEqual` expected

  it "parses home route properly - root base path" do
    let
      actual = Route.parse rootBasePath ""
      expected = Right Home

    actual `shouldEqual` expected

  it "parses home route properly - root base path, trailing slash" do
    let
      actual = Route.parse rootBasePath "/"
      expected = Right Home

    actual `shouldEqual` expected

  it "parses static asset route properly - root base path"
    do
      let
        actual = Route.parse rootBasePath "/abc.svg"
        expected = Right $ StaticAsset "abc.svg"

      actual `shouldEqual` expected

  it "parses static asset route properly - prefix base path"
    do
      let
        actual = Route.parse prefixBasePath "/base/path/abc.svg"
        expected = Right $ StaticAsset "abc.svg"

      actual `shouldEqual` expected

  it "parses sandbox route properly - root base path" do
    let
      actual = Route.parse rootBasePath "/sandbox"
      expected = Right (Sandbox { s: Nothing })

    actual `shouldEqual` expected

  it "parses sandbox route properly - root base path, trailing slash" do
    let
      actual = Route.parse rootBasePath "/sandbox/"
      expected = Right (Sandbox { s: Nothing })

    actual `shouldEqual` expected

  it "parses sandbox route properly - prefix base path" do
    let
      actual = Route.parse prefixBasePath "/base/path/sandbox"
      expected = Right (Sandbox { s: Nothing })

    actual `shouldEqual` expected

  generativeTestCase Long "codes routes properly" do
    basePath ← pure rootBasePath <|> pure prefixBasePath
    route ← Route.genRoute

    let
      actual = Route.parse basePath (Route.print basePath route)
      expected = Right route

    pure $ actual === expected

  generativeTestCase Long "printed paths start with a leading slash" do
    basePath ← pure rootBasePath <|> pure prefixBasePath
    route ← Route.genRoute

    let
      actual = Route.print basePath route

    pure case String.take 1 actual of
      "/" →
        Success
      _ →
        Failed $ "the path does not start with slash: " <> actual
