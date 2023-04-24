module Main.WebApp (main) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), Component)
import Halogen.Aff (selectElement)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.VDom.Driver (runUI)
import MLogo.Program as Program
import MLogo.WebApp.AceComponent (Output(..))
import MLogo.WebApp.AceComponent as AceComponent
import MLogo.WebApp.CanvasComponent as CanvasComponent
import MLogo.WebApp.SideBarComponent as SideBarComponent
import Type.Proxy (Proxy(..))
import Web.DOM.ParentNode (QuerySelector(..))

main ∷ Effect Unit
main = launchAff_ do
  mbDivElem ← selectElement $ QuerySelector "#halogen"
  for_ mbDivElem \divElem → do
    void $ runUI rootComp 0 divElem

rootComp ∷ ∀ i m o q. MonadAff m ⇒ Component q i o m
rootComp = Hooks.component \_ _ → Hooks.do
  source /\ sourceId ← Hooks.useState ""
  let
    handleAceOutput = case _ of
      TextChanged s →
        Hooks.put sourceId s
  Hooks.pure do
    HH.div
      [ HP.id "container" ]
      [ HH.slot
          (Proxy ∷ Proxy "ace")
          unit
          AceComponent.component
          unit
          handleAceOutput
      , HH.slot_
          (Proxy ∷ Proxy "sideBar")
          unit
          SideBarComponent.component
          unit
      , case Program.run source of
          Left errorMessage →
            HH.div
              [ HP.classes [ ClassName "error" ] ]
              [ HH.text errorMessage ]
          Right visibleState →
            HH.slot_
              (Proxy ∷ Proxy "canvas")
              unit
              CanvasComponent.component
              visibleState
      ]

