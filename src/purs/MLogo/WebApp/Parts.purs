module MLogo.WebApp.Parts (icon) where

import DOM.HTML.Indexed (HTMLa)
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as HH
import MLogo.WebApp.Utils (classes)

icon ∷ ∀ i w. String → HTML w i
icon mdiIconName = HH.span
  [ classes [ "icon", "is-small" ] ]
  [ HH.i
      [ classes [ "aria-hidden", "mdi", mdiIconName ] ]
      []
  ]
