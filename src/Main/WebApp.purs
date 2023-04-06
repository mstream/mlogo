module Main.WebApp (main) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List)
import Data.List as List
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen (ClassName(..), Component)
import Halogen.Aff (selectElement)
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Svg.Attributes (Color(..), Transform(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Halogen.VDom.Driver (runUI)
import MLogo.Interpretation (PointerState, Position(..), ScreenState)
import MLogo.Program as Program
import Web.DOM.ParentNode (QuerySelector(..))

type RenderableState = { pointer :: PointerState, screen :: ScreenState }

main :: Effect Unit
main = launchAff_ do
  mbDivElem <- selectElement $ QuerySelector "#halogen"
  for_ mbDivElem \divElem -> do
    void $ runUI rootComp 0 divElem

type Input = Int
type Output = Int
data Query a = Query a

canvasSize :: Number
canvasSize = 100.0

transform :: forall i r. IProp (transform :: String | r) i
transform = SA.transform
  [ Scale one (-one)
  , Translate (canvasSize / 2.0) (-canvasSize / 2.0)
  ]

pointerSize :: Number
pointerSize = 10.0

halfOfPointerSize :: Number
halfOfPointerSize = pointerSize / 2.0

rootComp :: Component Query Input Output Aff
rootComp = Hooks.component \_ _ -> Hooks.do
  Hooks.pure do
    HH.div
      [ HP.id "canvas-container" ]
      [ case Program.run "forward 10" of
          Left errorMessage ->
            HH.text errorMessage
          Right state ->
            renderSvg state
      ]

renderSvg :: forall i w. RenderableState -> HTML w i
renderSvg state = SE.svg
  [ SA.classes
      [ ClassName "canvas" ]
  , SA.viewBox 0.0 0.0 canvasSize canvasSize
  ]
  (Array.fromFoldable $ renderState state)

renderState :: forall i w. RenderableState -> List (HTML w i)
renderState state =
  renderScreenState
    state.screen <> renderPointerState state.pointer

renderPointerState :: forall i w. PointerState -> List (HTML w i)
renderPointerState pointer =
  let
    (Position p1) = pointer.position + Position { x: -halfOfPointerSize, y: -halfOfPointerSize }
    (Position p2) = pointer.position + Position { x: halfOfPointerSize, y: -halfOfPointerSize }
    (Position p3) = pointer.position + Position { x: zero, y: halfOfPointerSize }
    stroke = SA.stroke $ Named "green"
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

renderScreenState :: forall i w. ScreenState -> List (HTML w i)
renderScreenState = map \{ p1: (Position start), p2: (Position end) } ->
  SE.line
    [ SA.x1 start.x
    , SA.y1 start.y
    , SA.x2 end.x
    , SA.y2 end.y
    , SA.stroke $ Named "black"
    , transform
    ]

