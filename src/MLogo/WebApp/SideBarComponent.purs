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
    handleTabClick tab = Hooks.put currentTabId tab

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

    renderTab tab =
      let
        iconName = case tab of
          ExamplesTab →
            "mdi-book-play"
          ReferenceTab →
            "mdi-book-alphabet"

        isActive = currentTab == tab

        label = case tab of
          ExamplesTab →
            "Examples"
          ReferenceTab →
            "Reference"
      in
        HH.li
          [ HE.onClick \_ → handleTabClick tab
          , HP.classes
              if isActive then [ ClassName "is-active" ] else []
          ]
          [ HH.a_
              [ HH.span
                  [ HP.classes
                      [ ClassName "icon", ClassName "is-small" ]
                  ]
                  [ HH.i
                      [ HP.classes
                          [ ClassName "aria-hidden"
                          , ClassName "mdi"
                          , ClassName iconName
                          ]
                      ]
                      []
                  ]
              , HH.text label
              ]
          ]

  Hooks.pure do
    HH.div
      [ HP.classes
          [ ClassName "body"
          ]
      ]
      [ HH.nav
          [ HP.classes
              [ ClassName "is-boxed"
              , ClassName "is-centered"
              , ClassName "has-background-white-bis"
              , ClassName "navigation"
              , ClassName "tabs"
              ]
          ]
          [ HH.ul_ [ renderTab ReferenceTab, renderTab ExamplesTab ] ]
      , HH.div
          [ HP.classes
              [ ClassName "has-background-white-bis", ClassName "p-1" ]
          ]
          [ currentComponent ]
      ]

