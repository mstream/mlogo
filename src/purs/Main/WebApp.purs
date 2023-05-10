module Main.WebApp (main) where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen.Aff (selectElement)
import Halogen.VDom.Driver (runUI)
import MLogo.WebApp.AppComponent as AppComponent
import Web.DOM.ParentNode (QuerySelector(..))

main ∷ Effect Unit
main = launchAff_ do
  mbDivElem ← selectElement $ QuerySelector "#halogen"
  for_ mbDivElem \divElem → do
    void $ runUI AppComponent.component 0 divElem

