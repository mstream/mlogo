module MLogo.WebApp.CanvasComponent where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Number as Number
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Svg.Attributes (Color(..), Transform(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import MLogo.Interpretation.State
  ( Angle(..)
  , Position(..)
  , VisibleState
  )
import MLogo.Webapp.Utils (classes)

type Input = VisibleState

component ∷ ∀ m o q. MonadAff m ⇒ Component q Input o m
component = Hooks.component \_ { pointer, screen } → Hooks.do
  zoom /\ zoomId ← Hooks.useState $ maxZoom / 2

  let
    handleZoomIn = when
      (zoom < maxZoom)
      (Hooks.modify_ zoomId (_ + 1))

    handleZoomOut = when
      (zoom > 1)
      (Hooks.modify_ zoomId (_ - 1))

    canvasSize = Int.toNumber canvasBaseSize
      * (Number.pow 2.0 (Int.toNumber $ maxZoom - zoom))

    transforms =
      [ Translate (canvasSize / 2.0) (canvasSize / 2.0)
      , Scale one (-one)
      ]

    renderZoomScale acc n =
      if n <= maxZoom then
        renderZoomScale (acc <> if n == zoom then "┼" else "─") (n + 1)
      else acc

    renderPointer { angle: Angle a, position } =
      let
        halfOfPointerSize = Int.toNumber pointerBaseSize / 2.0
        (Position p1) = position + Position
          { x: -halfOfPointerSize, y: -halfOfPointerSize }
        (Position p2) = position + Position
          { x: halfOfPointerSize, y: -halfOfPointerSize }
        (Position p3) = position + Position
          { x: zero, y: halfOfPointerSize }
        (Position p) = position
        stroke = SA.stroke $ Named "green"
        transform = SA.transform $ transforms <> [ Rotate (-a) p.x p.y ]
      in
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

    renderScreen = Array.fromFoldable <<< map
      \{ p1: (Position start), p2: (Position end) } →
        SE.line
          [ SA.x1 start.x
          , SA.y1 start.y
          , SA.x2 end.x
          , SA.y2 end.y
          , SA.stroke $ Named "black"
          , SA.transform transforms
          ]

    renderZoomPanel = HH.div
      [ classes
          [ "is-flex"
          , "is-flex-direction-row"
          , "is-justify-content-space-between"
          ]
      , HP.id "zoom-panel"
      ]
      [ renderZoomButton "mdi-minus" handleZoomOut
      , HH.div_ [ HH.text $ renderZoomScale "" 1 ]
      , renderZoomButton "mdi-plus" handleZoomIn
      ]

    renderZoomButton iconName clickHandler = HH.button
      [ HE.onClick \_ → clickHandler
      , classes [ "button", "square" ]
      ]
      [ HH.span
          [ classes [ "icon", "is-small" ] ]
          [ HH.i
              [ classes [ "mdi", iconName ] ]
              []
          ]
      ]

  Hooks.pure do
    HH.div
      [ classes
          [ "body"
          , "has-background-grey-lighter"
          , "is-align-items-center"
          , "is-justify-content-center"
          , "is-flex"
          , "is-relative"
          , "square"
          ]
      ]
      [ HH.div
          [ classes [ "has-background-white", "wrapper", "square" ] ]
          [ SE.svg
              [ SA.viewBox zero zero canvasSize canvasSize ]
              (renderScreen screen <> renderPointer pointer)
          ]
      , renderZoomPanel
      ]

pointerBaseSize ∷ Int
pointerBaseSize = 16

canvasBaseSize ∷ Int
canvasBaseSize = 128

maxZoom ∷ Int
maxZoom = 4

