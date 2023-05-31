module MLogo.WebApp.Components.Home (component) where

import Effect.Aff.Class (class MonadAff)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import MLogo.WebApp.Utils (classes)

component ∷ ∀ i m o q. MonadAff m ⇒ Component q i o m
component = Hooks.component \_ _ → Hooks.do
  Hooks.pure do
    HH.div
      [ classes
          [ "is-flex", "is-flex-direction-column", "is-full-height" ]
      ]
      [ HH.section
          [ classes [ "hero" ] ]
          [ HH.div
              [ classes [ "hero-body" ] ]
              [ HH.p
                  [ classes [ "title" ] ]
                  [ HH.text "Home" ]
              , HH.p
                  [ classes [ "subtitle" ] ]
                  [ HH.text "subtitle" ]
              ]
          ]
      , HH.div
          [ classes [ "content", "ml-2" ] ]
          [ HH.p_ [ HH.text "content line 1" ]
          , HH.p_ [ HH.text "content line 2" ]
          , HH.p_ [ HH.text "content line 3" ]
          , HH.p_ [ HH.text "content line 4" ]
          ]
      , HH.footer
          [ classes [ "footer", "mt-auto" ] ]
          [ HH.div
              [ classes [ "content", "has-text-centered" ] ]
              [ HH.p_
                  [ HH.strong_ [ HH.text "MLogo" ]
                  , HH.text " by "
                  , HH.a
                      [ HP.href "https://github.com/mstream" ]
                      [ HH.text "Maciej Laciak" ]
                  ]
              ]
          ]
      ]
