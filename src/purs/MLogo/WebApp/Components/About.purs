module MLogo.WebApp.Components.About (component) where

import Effect.Aff.Class (class MonadAff)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import MLogo.WebApp.Parts (IconSize(..))
import MLogo.WebApp.Parts as Parts
import MLogo.WebApp.Utils (classes)

component ∷ ∀ i m o q. MonadAff m ⇒ Component q i o m
component = Hooks.component \_ _ → Hooks.do
  let
    renderLink href iconStyle iconName label = HH.a
      [ HP.href href ]
      [ Parts.icon iconStyle iconName Small
      , HH.span
          [ classes [ "ml-1" ] ]
          [ HH.text label ]
      ]

  Hooks.pure do
    HH.div
      [ classes [ "is-flex", "is-flex-direction-column" ] ]
      [ renderLink
          "https://github.com/mstream/mlogo"
          "fab"
          "fa-github-square"
          "This website's source code repository"
      , renderLink
          "https://en.wikipedia.org/wiki/Logo_(programming_language)"
          "fab"
          "fa-wikipedia-w"
          "Logo programming language article"
      ]

