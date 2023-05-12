module MLogo.WebApp.SideBarComponent (component) where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Examples as Examples
import Halogen (Component)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks as ExtraHooks
import MLogo.Interpretation.Command.Commands as Commands
import MLogo.WebApp.ExamplesComponent as ExamplesComponent
import MLogo.WebApp.ReferenceComponent as ReferenceComponent
import MLogo.Webapp.Utils (classes)
import Type.Proxy (Proxy(..))

data Tab = ExamplesTab | ReferenceTab

derive instance Eq Tab

component
  ∷ ∀ i m q. MonadAff m ⇒ Component q i ExamplesComponent.Output m
component = Hooks.component \{ outputToken } _ → Hooks.do
  currentTab /\ putCurrentTab ← ExtraHooks.usePutState ReferenceTab

  let
    handleExamplesOutput ∷ ExamplesComponent.Output → HookM m Unit
    handleExamplesOutput = Hooks.raise outputToken

    handleTabClick ∷ Tab → HookM m Unit
    handleTabClick tab = putCurrentTab tab

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

    renderTab ∷ ∀ w. Tab → HTML w (HookM m Unit)
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
          , classes if isActive then [ "is-active" ] else []
          ]
          [ HH.a_
              [ HH.span
                  [ classes [ "icon", "is-small" ] ]
                  [ HH.i
                      [ classes [ "aria-hidden", "mdi", iconName ] ]
                      []
                  ]
              , HH.text label
              ]
          ]

  Hooks.pure do
    HH.div
      [ classes
          [ "is-full-height"
          , "is-full-width"
          ]
      ]
      [ HH.nav
          [ classes
              [ "is-boxed"
              , "is-centered"
              , "has-background-white-bis"
              , "navigation"
              , "tabs"
              ]
          ]
          [ HH.ul_ [ renderTab ReferenceTab, renderTab ExamplesTab ] ]
      , HH.div
          [ classes
              [ "has-background-white-bis"
              , "is-full-height"
              , "p-1"
              ]
          ]
          [ currentComponent ]
      ]

