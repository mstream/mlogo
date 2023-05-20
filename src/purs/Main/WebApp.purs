module Main.WebApp (main) where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Random (randomInt)
import Halogen.Aff (selectElement)
import Halogen.VDom.Driver (runUI)
import MLogo.WebApp.Components.App as App
import Web.DOM.ParentNode (QuerySelector(..))

main ∷ Effect Unit
main = do
  randomNumberSeed ← randomInt 0 top
  launchAff_ do
    mbHalogenElement ← selectElement halogenElementSelector
    for_ mbHalogenElement (runUI App.component randomNumberSeed)

halogenElementSelector ∷ QuerySelector
halogenElementSelector = QuerySelector "#halogen"
