module MLogo.WebApp.CanvasComponent where

import Prelude

import Control.Monad.State (put)
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.Svg.Attributes (Color(..), Transform(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import MLogo.Interpretation.State
  ( Angle(..)
  , PointerState
  , Position(..)
  , ScreenState
  , VisibleState
  )

data Action = Receive Input

type Input = VisibleState

type State = Input

component ∷ ∀ m o q. MonadAff m ⇒ Component q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

initialState ∷ Input → State
initialState = identity

render ∷ ∀ m. State → ComponentHTML Action () m
render state =
  SE.svg
    [ SA.id "canvas"
    , SA.viewBox 0.0 0.0 canvasSize canvasSize
    ]
    (Array.fromFoldable $ renderVisibleState state)

renderVisibleState ∷ ∀ i w. VisibleState → List (HTML w i)
renderVisibleState state =
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

handleAction
  ∷ ∀ m o. MonadAff m ⇒ Action → HalogenM State Action () o m Unit
handleAction = case _ of
  Receive visibleState →
    put visibleState

canvasSize ∷ Number
canvasSize = 400.0

transforms ∷ Array Transform
transforms =
  [ Translate (canvasSize / 2.0) (canvasSize / 2.0)
  , Scale one (-one)
  ]

pointerSize ∷ Number
pointerSize = 8.0

halfOfPointerSize ∷ Number
halfOfPointerSize = pointerSize / 2.0

