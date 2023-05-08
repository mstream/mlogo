module Main.WebApp (main) where

import Prelude

import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen (ClassName(..), Component)
import Halogen.Aff (selectElement)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Halogen.VDom.Driver (runUI)
import MLogo.Program as Program
import MLogo.WebApp.CanvasComponent as CanvasComponent
import MLogo.WebApp.EditorComponent (Output(..), Query(..))
import MLogo.WebApp.EditorComponent as EditorComponent
import MLogo.WebApp.ExamplesComponent (Output(..))
import MLogo.WebApp.ExamplesComponent as ExamplesComponent
import MLogo.WebApp.SideBarComponent as SideBarComponent
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as R
import Type.Proxy (Proxy(..))
import Utils as Utils
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML as HTML
import Web.HTML.History (DocumentTitle(..), URL(..))
import Web.HTML.History as History
import Web.HTML.Location as Location
import Web.HTML.Window as Window

main ∷ Effect Unit
main = launchAff_ do
  mbDivElem ← selectElement $ QuerySelector "#halogen"
  for_ mbDivElem \divElem → do
    void $ runUI rootComp 0 divElem

rootComp ∷ ∀ i m o q. MonadAff m ⇒ Component q i o m
rootComp = Hooks.component \{ slotToken } _ → Hooks.do
  astResult /\ astResultId ← Hooks.useState $ Right Nil

  let
    handleEditorOutput ∷ EditorComponent.Output → HookM m Unit
    handleEditorOutput = case _ of
      AstChanged { ast, source } → do
        liftEffect $ setSourceInUrl source
        Hooks.put astResultId (Right ast)
      SyntaxErrorDetected errorMessage →
        Hooks.put astResultId (Left errorMessage)

    handleSideBarOutput ∷ ExamplesComponent.Output → HookM m Unit
    handleSideBarOutput (SourceTryRequested exampleAst) = do
      Hooks.put astResultId (Right exampleAst)

      Hooks.tell
        slotToken
        (Proxy ∷ Proxy "editor")
        unit
        (SetAst exampleAst)

    renderLeftColumn = HH.div
      [ HP.classes
          [ ClassName "column"
          , ClassName "is-6"
          ]
      ]
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
          ]
          [ HH.slot
              (Proxy ∷ Proxy "sideBar")
              unit
              SideBarComponent.component
              unit
              handleSideBarOutput
          ]
      ]

    renderRightColumn = HH.div
      [ HP.classes
          [ ClassName "column"
          , ClassName "is-6"
          ]
      ]
      [ case astResult >>= Program.interpretAst of
          Left errorMessage →
            HH.div
              [ HP.classes [ ClassName "error" ] ]
              [ HH.text errorMessage ]
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
      [ HP.classes
          [ ClassName "columns"
          , ClassName "is-gapless"
          ]
      , HP.id "container"
      ]
      [ renderLeftColumn
      , renderRightColumn
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

  let
    path = printSearch
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

route ∷ RouteDuplex' { s ∷ Maybe String }
route = R.params { s: R.optional <<< R.string }
