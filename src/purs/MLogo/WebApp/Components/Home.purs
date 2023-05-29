module MLogo.WebApp.Components.Home (component) where

import Effect.Aff.Class (class MonadAff)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.Hooks as Hooks

component ∷ ∀ i m o q. MonadAff m ⇒ Component q i o m
component = Hooks.component \_ _ → Hooks.do
  Hooks.pure do
    HH.text "home page"
