module MLogo.WebApp.Parts (IconSize(..), icon) where

import Prelude

import Halogen.HTML (HTML)
import Halogen.HTML as HH
import MLogo.WebApp.Utils (classes)

data IconSize = Large | Medium | Regular | Small

icon ∷ ∀ i w. String → String → IconSize → HTML w i
icon style name size = HH.span
  [ classes $ [ "icon" ] <> containerSizeClasses ]
  [ HH.i
      [ classes $ [ "aria-hidden", style, name ] <> iconSizeClasses ]
      []
  ]
  where
  containerSizeClasses ∷ Array String
  containerSizeClasses = case size of
    Large →
      [ "is-large" ]
    Medium →
      [ "is-medium" ]
    Regular →
      []
    Small →
      [ "is-small" ]

  iconSizeClasses ∷ Array String
  iconSizeClasses = case size of
    Large →
      [ "fa-2x" ]
    Medium →
      [ "fa-lg" ]
    Regular →
      []
    Small →
      []
