module MLogo.WebApp.Components.Home (component) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import MLogo.WebApp.BaseUrl (BaseUrl)
import MLogo.WebApp.Route (Route(..))
import MLogo.WebApp.Route as Route
import MLogo.WebApp.Utils (classes)

component ∷ ∀ m o q. MonadAff m ⇒ Component q { basePath ∷ BaseUrl } o m
component = Hooks.component \_ { basePath } → Hooks.do
  let
    printRoute ∷ Route → String
    printRoute = Route.print basePath

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
      , HH.section
          [ classes [ "section" ] ]
          [ HH.h1
              [ classes [ "title" ] ]
              [ HH.text "title" ]
          , HH.div
              [ classes [ "columns" ] ]
              [ HH.div
                  [ classes [ "column" ] ]
                  [ HH.text "blablabla" ]
              , HH.div
                  [ classes [ "column" ] ]
                  [ HH.img
                      [ HP.src
                          $ printRoute
                          $ StaticAsset "example-canvas.svg"
                      , classes
                          [ "has-background-light"
                          , "image"
                          , "is-full-width"
                          ]
                      ]
                  ]
              ]
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
