module MLogo.WebApp.Components.About (component) where

import Effect.Aff.Class (class MonadAff)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import MLogo.WebApp.Parts as Parts
import MLogo.WebApp.Utils (classes)

component ∷ ∀ i m o q. MonadAff m ⇒ Component q i o m
component = Hooks.component \_ _ → Hooks.do
  let
    renderLink href iconName label = HH.a
      [ HP.href href ]
      [ Parts.icon iconName
      , HH.span
          [ classes [ "ml-1" ] ]
          [ HH.text label ]
      ]

  Hooks.pure do
    HH.div
      [ classes
          [ "is-background-white"
          , "is-flex"
          , "is-flex-direction-column"
          ]
      ]
      [ renderLink
          "https://github.com/mstream/mlogo"
          "mdi-github"
          "This website's source code repository"
      , renderLink
          "https://en.wikipedia.org/wiki/Logo_(programming_language)"
          "mdi-wikipedia"
          "Logo programming language article"
      ]

