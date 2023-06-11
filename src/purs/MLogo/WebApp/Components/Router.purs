module MLogo.WebApp.Components.Router (component) where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks as ExtraHooks
import MLogo.WebApp.BaseUrl (BaseUrl)
import MLogo.WebApp.Components.Home as HomeComponent
import MLogo.WebApp.Components.Sandbox as SandboxComponent
import MLogo.WebApp.Parts (IconSize(..))
import MLogo.WebApp.Parts as Parts
import MLogo.WebApp.Route (Route(..))
import MLogo.WebApp.Route as Route
import MLogo.WebApp.Utils (classes)
import Type.Proxy (Proxy(..))
import Web.HTML as HTML
import Web.HTML.History (DocumentTitle(..), URL(..))
import Web.HTML.History as History
import Web.HTML.Location as Location
import Web.HTML.Window as Window

type Input = { basePath ∷ BaseUrl, randomNumberSeed ∷ Int }

component
  ∷ ∀ m o q. MonadAff m ⇒ Component q Input o m
component = Hooks.component \_ { basePath, randomNumberSeed } →
  Hooks.do
    mbRoute /\ putMbRoute ← ExtraHooks.usePutState Nothing

    isBurgerMenuOpen /\ modifyIsBurgerMenuOpen ←
      ExtraHooks.useModifyState_ false

    let
      toggleBurgerMenuOpen ∷ HookM m Unit
      toggleBurgerMenuOpen = modifyIsBurgerMenuOpen not

      printRoute ∷ Route → String
      printRoute = Route.print basePath

      handleSandboxOutput ∷ SandboxComponent.Output → HookM m Unit
      handleSandboxOutput = case _ of
        SandboxComponent.SourceChanged source →
          liftEffect do
            window ← HTML.window
            history ← Window.history window
            historyState ← History.state history
            location ← Window.location window
            origin ← Location.origin location

            History.replaceState
              historyState
              (DocumentTitle "MLogo")
              ( URL $ origin
                  <> (Route.print basePath (Sandbox { s: Just source }))
              )
              history

      renderCurrentComponent = case mbRoute of
        Just Home →
          HH.slot_
            (Proxy ∷ Proxy "home")
            unit
            HomeComponent.component
            { basePath }
        Just (Sandbox { s: mbSource }) →
          HH.slot
            (Proxy ∷ Proxy "sandbox")
            unit
            SandboxComponent.component
            { initialSource: fromMaybe "" mbSource, randomNumberSeed }
            handleSandboxOutput
        _ →
          HH.text "Page Not Found"

      renderNavigationBar =
        let
          logoImage = HH.img
            [ HP.src $ printRoute $ StaticAsset "pwa.svg"
            , classes [ "logo", "navbar-item" ]
            ]
        in
          HH.nav
            [ HPA.label "main navigation"
            , HPA.role "navigation"
            , classes [ "is-flex-grow-0", "is-flex-shrink-0", "navbar" ]
            ]
            [ HH.div
                [ classes [ "navbar-brand" ]
                ]
                [ case mbRoute of
                    Just Home →
                      HH.i
                        [ classes [ "navbar-item" ] ]
                        [ logoImage ]

                    _ →
                      HH.a
                        [ HP.href $ printRoute Home
                        , classes [ "navbar-item" ]
                        ]
                        [ logoImage ]
                , HH.a
                    [ HPA.label "menu"
                    , HPA.role "button"
                    , HE.onClick \_ → toggleBurgerMenuOpen
                    , classes $ [ "navbar-burger" ]
                        <>
                          if isBurgerMenuOpen then [ "is-active" ]
                          else []
                    ]
                    [ HH.span [ HPA.hidden "true" ] []
                    , HH.span [ HPA.hidden "true" ] []
                    , HH.span [ HPA.hidden "true" ] []
                    ]
                ]
            , renderNavigationMenu
            ]

      renderNavigationMenu = HH.div
        [ classes $ [ "navbar-menu" ]
            <> if isBurgerMenuOpen then [ "is-active" ] else []
        ]
        [ HH.div
            [ classes [ "navbar-start" ] ]
            [ HH.div
                [ classes
                    [ "is-fullwidth"
                    , "is-small"
                    , "is-toggle"
                    , "tabs"
                    ]
                ]
                [ HH.ul_
                    [ HH.li
                        [ classes case mbRoute of
                            Just (Sandbox _) →
                              [ "is-active" ]

                            _ →
                              []
                        ]
                        [ HH.a
                            [ HP.href $ printRoute
                                (Sandbox { s: Nothing })
                            ]
                            [ Parts.icon
                                "fa-solid"
                                "fa-bucket"
                                Medium
                            , HH.span
                                [ classes [ "ml-1" ] ]
                                [ HH.text "Sandbox" ]
                            ]
                        ]

                    ]
                ]
            ]
        , HH.div
            [ classes [ "navbar-end" ] ]
            [ HH.a
                [ HP.href "https://github.com/mstream/mlogo" ]
                [ Parts.icon "fab" "fa-github-square" Large ]
            ]
        ]

    Hooks.useLifecycleEffect do
      s ← liftEffect do
        location ← Window.location =<< HTML.window
        path ← Location.pathname location
        search ← Location.search location
        pure $ path <> search

      putMbRoute $ hush $ Route.parse basePath s
      pure Nothing

    Hooks.pure do
      HH.div
        [ HP.id "container"
        , classes
            [ "is-flex", "is-flex-direction-column" ]
        ]
        [ renderNavigationBar
        , HH.div
            [ HP.id "router-content"
            , classes [ "is-flex-grow-1", "is-flex-shrink-1" ]
            ]
            [ renderCurrentComponent ]
        ]

