module MLogo.WebApp.Components.App (component) where

import Prelude

import Data.Either (Either(..), hush, note)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks as ExtraHooks
import MLogo.Program as Program
import MLogo.WebApp.Components.Canvas as CanvasComponent
import MLogo.WebApp.Components.Editor (Output(..), Query(..))
import MLogo.WebApp.Components.Editor as EditorComponent
import MLogo.WebApp.Components.Examples (Output(..))
import MLogo.WebApp.Components.Examples as ExamplesComponent
import MLogo.WebApp.Components.SideBar as SideBarComponent
import MLogo.WebApp.Parts as Parts
import MLogo.WebApp.Utils (classes)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as R
import Type.Proxy (Proxy(..))
import Utils as Utils
import Web.HTML as HTML
import Web.HTML.History (DocumentTitle(..), URL(..))
import Web.HTML.History as History
import Web.HTML.Location as Location
import Web.HTML.Window as Window

component ∷ ∀ i m o q. MonadAff m ⇒ Component q i o m
component = Hooks.component \{ slotToken } _ → Hooks.do
  mbAst /\ putMbAst ← ExtraHooks.usePutState $ Just Nil

  let
    handleEditorOutput ∷ EditorComponent.Output → HookM m Unit
    handleEditorOutput = case _ of
      AstChanged { ast, source } → do
        liftEffect $ setSourceInUrl source
        putMbAst $ Just ast
      SyntaxErrorDetected →
        putMbAst Nothing

    handleSideBarOutput ∷ ExamplesComponent.Output → HookM m Unit
    handleSideBarOutput (SourceTryRequested exampleAst) = do
      putMbAst $ Just exampleAst

      Hooks.tell
        slotToken
        (Proxy ∷ Proxy "editor")
        unit
        (SetAst exampleAst)

    renderUserInputColumn = HH.div
      [ classes [ "column", "is-6" ] ]
      [ HH.div
          [ HP.id "editor" ]
          [ HH.slot
              (Proxy ∷ Proxy "editor")
              unit
              EditorComponent.component
              unit
              handleEditorOutput
          ]
      , HH.div
          [ HP.id "side-bar"
          , classes [ "if-full-height" ]
          ]
          [ HH.slot
              (Proxy ∷ Proxy "sideBar")
              unit
              SideBarComponent.component
              unit
              handleSideBarOutput
          ]
      ]

    renderDisplayColumn = HH.div
      [ classes [ "column", "is-6" ] ]
      [ case (note "Syntax Error" mbAst) >>= Program.interpretAst of
          Left errorMessage →
            HH.div
              [ classes [ "error" ] ]
              [ Parts.icon "mdi-close-box"
              , HH.text $ "Runtime Error: " <> errorMessage
              ]
          Right visibleState →
            HH.div
              [ HP.id "canvas" ]
              [ HH.slot_
                  (Proxy ∷ Proxy "canvas")
                  unit
                  CanvasComponent.component
                  visibleState
              ]
      ]

  Hooks.useLifecycleEffect do
    mbSource ← liftEffect $ getSourceFromUrl
    case mbSource of
      Just source →
        Hooks.tell
          slotToken
          (Proxy ∷ Proxy "editor")
          unit
          (SetSource source)
      Nothing →
        pure unit

    pure Nothing

  Hooks.pure do
    HH.div
      [ classes [ "columns", "is-gapless" ]
      , HP.id "container"
      ]
      [ renderDisplayColumn
      , renderUserInputColumn
      ]

getSourceFromUrl ∷ Effect (Maybe String)
getSourceFromUrl = do
  window ← HTML.window
  location ← Window.location window
  search ← Location.search location
  pure do
    { s } ← parseSearch search
    encoded ← s
    Utils.decodeFromString encoded

setSourceInUrl ∷ String → Effect Unit
setSourceInUrl source = do
  window ← HTML.window
  history ← Window.history window
  historyState ← History.state history
  location ← Window.location window
  origin ← Location.origin location
  pathname ← Location.pathname location

  let
    path = pathname <> printSearch
      { s: Utils.uriEncodedStringToString
          <$> Utils.encodeToUriComponent source
      }

  History.replaceState
    historyState
    (DocumentTitle "MLogo")
    (URL $ origin <> path)
    history

type Search = { s ∷ Maybe String }

parseSearch ∷ String → Maybe Search
parseSearch = hush <<< R.parse route

printSearch ∷ Search → String
printSearch = R.print route

type Route = { s ∷ Maybe String }

route ∷ RouteDuplex' { s ∷ Maybe String }
route = R.params { s: R.optional <<< R.string }
