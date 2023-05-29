module Main.WebApp (main) where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Random (randomInt)
import Halogen.Aff (selectElement)
import Halogen.VDom.Driver (runUI)
import MLogo.WebApp.Components.Router as RouterComponent
import Web.DOM.ParentNode (QuerySelector(..))

main ∷ Effect Unit
main = do
  randomNumberSeed ← randomInt 0 top
  launchAff_ do
    mbRootElement ← selectElement rootElementSelector
    for_ mbRootElement
      (runUI RouterComponent.component { randomNumberSeed })

rootElementSelector ∷ QuerySelector
rootElementSelector = QuerySelector "#root"
