module MLogo.WebApp.Utils (classes) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Halogen (ClassName(..))
import Halogen.HTML (IProp)
import Halogen.HTML.Properties as HP

classes
  ∷ ∀ f i r
  . Foldable f
  ⇒ Functor f
  ⇒ f String
  → IProp (class ∷ String | r) i
classes = HP.classes <<< Array.fromFoldable <<< map ClassName

