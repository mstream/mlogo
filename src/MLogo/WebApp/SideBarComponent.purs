module MLogo.WebApp.SideBarComponent (component) where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Examples as Examples
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import MLogo.Interpretation.Command.Commands as Commands
import MLogo.WebApp.ExamplesComponent as ExamplesComponent
import MLogo.WebApp.ReferenceComponent as ReferenceComponent
import Type.Proxy (Proxy(..))

data Tab = ExamplesTab | ReferenceTab

derive instance Eq Tab

data Action = HandleTabSelection Tab

component ∷ ∀ i m o q. MonadAff m ⇒ H.Component q i o m
component = Hooks.component \_ _ → Hooks.do
  currentTab /\ currentTabId ← Hooks.useState ReferenceTab

  let
    handleTabClick tab = Hooks.modify_ currentTabId (const tab)

    currentComponent = case currentTab of
      ExamplesTab →
        HH.slot_
          (Proxy ∷ Proxy "examples")
          unit
          ExamplesComponent.component
          Examples.examplesByTitle
      ReferenceTab →
        HH.slot_
          (Proxy ∷ Proxy "reference")
          unit
          ReferenceComponent.component
          Commands.commandsByAliasByCategory

    renderTab tab = HH.div
      [ HE.onClick \_ → handleTabClick tab
      , HP.classes
          ( [ ClassName "tab" ]
              <>
                if currentTab == tab then
                  [ ClassName "selected" ]
                else []
          )
      ]
      [ HH.text case tab of
          ExamplesTab →
            "Examples"
          ReferenceTab →
            "Reference"
      ]

  Hooks.pure do
    HH.div
      [ HP.id "side-bar" ]
      [ HH.div
          [ HP.classes [ ClassName "tabs" ] ]
          [ renderTab ReferenceTab, renderTab ExamplesTab ]
      , HH.div
          [ HP.classes [ ClassName "content" ] ]
          [ currentComponent ]
      ]

