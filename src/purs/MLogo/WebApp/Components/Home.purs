module MLogo.WebApp.Components.Home (component) where

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
  Hooks.pure do
    HH.text "home page"
