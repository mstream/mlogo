module MLogo.WebApp.Components.SideBar (component) where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Examples as Examples
import Halogen (Component)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks as ExtraHooks
import MLogo.Interpretation.Command.Commands as Commands
import MLogo.WebApp.Components.About as AboutComponent
import MLogo.WebApp.Components.Examples as ExamplesComponent
import MLogo.WebApp.Components.Reference as ReferenceComponent
import MLogo.WebApp.Parts (IconSize(..))
import MLogo.WebApp.Parts as Parts
import MLogo.WebApp.Utils (classes)
import Type.Proxy (Proxy(..))

data Tab = AboutTab | ExamplesTab | ReferenceTab

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
      AboutTab →
        HH.slot_
          (Proxy ∷ Proxy "about")
          unit
          AboutComponent.component
          unit
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
        iconStyle /\ iconName = case tab of
          AboutTab →
            "fas" /\ "fa-question-circle"
          ExamplesTab →
            "fas" /\ "fa-atlas"
          ReferenceTab →
            "fas" /\ "fa-book"

        isActive = currentTab == tab

        label = case tab of
          AboutTab →
            "About"
          ExamplesTab →
            "Examples"
          ReferenceTab →
            "Reference"
      in
        HH.li
          [ HE.onClick \_ → handleTabClick tab
          , classes if isActive then [ "is-active" ] else []
          ]
          [ HH.a
              [ HE.onClick $ const $ pure unit
              , HP.href "#"
              ]
              [ Parts.icon iconStyle iconName Small
              , HH.span
                  [ classes [ "ml-1" ] ]
                  [ HH.text label ]
              ]
          ]

  Hooks.pure do
    HH.div
      [ classes
          [ "is-flex"
          , "is-flex-direction-column"
          , "is-full-height"
          ]
      ]
      [ HH.nav
          [ classes
              [ "is-boxed"
              , "is-centered"
              , "is-flex-grow-0"
              , "is-flex-shrink-0"
              , "navigation"
              , "tabs"
              ]
          ]
          [ HH.ul_
              [ renderTab ReferenceTab
              , renderTab ExamplesTab
              , renderTab AboutTab
              ]
          ]
      , HH.div
          [ HP.tabIndex 0
          , classes
              [ "is-flex-grow-1"
              , "is-flex-shrink-1"
              , "is-full-height"
              , "is-overflow-y-scroll"
              , "p-1"
              ]
          ]
          [ currentComponent ]
      ]

