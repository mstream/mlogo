module Main.WebApp (main) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), Component)
import Halogen.Aff (selectElement)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Svg.Attributes (Color(..), Transform(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Halogen.VDom.Driver (runUI)
import MLogo.Interpretation.State
  ( Angle(..)
  , PointerState
  , Position(..)
  , ScreenState
  , VisibleState
  )
import MLogo.Interpretation.Statement as Statement
import MLogo.Program as Program
import MLogo.WebApp.AceComponent (Output(..))
import MLogo.WebApp.AceComponent as AceComponent
import Type.Proxy (Proxy(..))
import Web.DOM.ParentNode (QuerySelector(..))

type State = { text ∷ String }

type ChildSlots =
  ( ace ∷ AceComponent.Slot Unit
  )

_ace = Proxy ∷ Proxy "ace"

main ∷ Effect Unit
main = launchAff_ do
  mbDivElem ← selectElement $ QuerySelector "#halogen"
  for_ mbDivElem \divElem → do
    void $ runUI rootComp 0 divElem

canvasSize ∷ Number
canvasSize = 100.0

transforms ∷ Array Transform
transforms =
  [ Translate (canvasSize / 2.0) (canvasSize / 2.0)
  , Scale one (-one)
  ]

pointerSize ∷ Number
pointerSize = 8.0

halfOfPointerSize ∷ Number
halfOfPointerSize = pointerSize / 2.0

rootComp ∷ ∀ i m o q. MonadAff m ⇒ Component q i o m
rootComp = Hooks.component \_ _ → Hooks.do
  source /\ sourceId ← Hooks.useState ""
  let
    handleAceOutput = case _ of
      TextChanged s →
        Hooks.put sourceId s
  Hooks.pure do
    HH.div
      [ HP.id "container" ]
      [ HH.div
          [ HP.classes [ ClassName "left-panel" ] ]
          [ HH.slot _ace
              unit
              AceComponent.component
              unit
              handleAceOutput
          , renderLegend $ List.fromFoldable $ Map.keys
              Statement.commands
          ]
      , case Program.run source of
          Left errorMessage →
            HH.div
              [ HP.classes [ ClassName "error" ] ]
              [ HH.text errorMessage ]
          Right state →
            renderSvg state
      ]

renderSvg ∷ ∀ i w. VisibleState → HTML w i
renderSvg state = SE.svg
  [ SA.classes
      [ ClassName "canvas" ]
  , SA.viewBox 0.0 0.0 canvasSize canvasSize
  ]
  (Array.fromFoldable $ renderState state)

renderLegend ∷ ∀ i w. List String → HTML w i
renderLegend commandNames = HH.div
  [ HP.classes [ ClassName "legend" ] ]
  (Array.fromFoldable $ renderEntry <$> commandNames)
  where
  renderEntry s =
    HH.div_ [ HH.text s ]

renderState ∷ ∀ i w. VisibleState → List (HTML w i)
renderState state =
  renderScreenState
    state.screen <> renderPointerState state.pointer

renderPointerState ∷ ∀ i w. PointerState → List (HTML w i)
renderPointerState pointer =
  let
    (Position p1) = pointer.position + Position
      { x: -halfOfPointerSize, y: -halfOfPointerSize }
    (Position p2) = pointer.position + Position
      { x: halfOfPointerSize, y: -halfOfPointerSize }
    (Position p3) = pointer.position + Position
      { x: zero, y: halfOfPointerSize }
    (Position p) = pointer.position
    (Angle a) = pointer.angle
    stroke = SA.stroke $ Named "green"
    transform = SA.transform $ transforms <> [ Rotate (-a) p.x p.y ]
  in
    List.fromFoldable
      [ SE.line
          [ SA.x1 p1.x
          , SA.y1 p1.y
          , SA.x2 p2.x
          , SA.y2 p2.y
          , stroke
          , transform
          ]
      , SE.line
          [ SA.x1 p2.x
          , SA.y1 p2.y
          , SA.x2 p3.x
          , SA.y2 p3.y
          , stroke
          , transform
          ]
      , SE.line
          [ SA.x1 p3.x
          , SA.y1 p3.y
          , SA.x2 p1.x
          , SA.y2 p1.y
          , stroke
          , transform
          ]
      ]

renderScreenState ∷ ∀ i w. ScreenState → List (HTML w i)
renderScreenState = map \{ p1: (Position start), p2: (Position end) } →
  SE.line
    [ SA.x1 start.x
    , SA.y1 start.y
    , SA.x2 end.x
    , SA.y2 end.y
    , SA.stroke $ Named "black"
    , SA.transform transforms
    ]

