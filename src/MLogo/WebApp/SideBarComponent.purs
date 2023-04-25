module MLogo.WebApp.SideBarComponent (component) where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Examples as Examples
import Halogen (ClassName(..), Component)
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

component
  ∷ ∀ i m q. MonadAff m ⇒ Component q i ExamplesComponent.Output m
component = Hooks.component \{ outputToken } _ → Hooks.do
  currentTab /\ currentTabId ← Hooks.useState ReferenceTab

  let
    handleExamplesOutput = Hooks.raise outputToken
    handleTabClick tab = Hooks.modify_ currentTabId (const tab)

    currentComponent = case currentTab of
      ExamplesTab →
        HH.slot
          (Proxy ∷ Proxy "examples")
          unit
          ExamplesComponent.component
          Examples.examplesByTitle
          handleExamplesOutput
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

