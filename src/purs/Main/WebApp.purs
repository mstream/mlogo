module Main.WebApp (main) where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Halogen.Aff (selectElement)
import Halogen.VDom.Driver (runUI)
import MLogo.WebApp.Components.Router as RouterComponent
import MLogo.WebApp.Utils as Utils
import Web.DOM.ParentNode (QuerySelector(..))

main ∷ Effect Unit
main = launchAff_ do
  randomNumberSeed ← liftEffect $ randomInt 0 top
  baseUrl ← liftEffect Utils.loadBaseUrl
  mbRootElement ← selectElement rootElementSelector

  for_ mbRootElement
    ( runUI
        RouterComponent.component
        { basePath: baseUrl, randomNumberSeed }
    )

rootElementSelector ∷ QuerySelector
rootElementSelector = QuerySelector "#root"
