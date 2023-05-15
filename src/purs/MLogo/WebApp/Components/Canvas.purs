module MLogo.WebApp.Components.Canvas where

import Prelude

import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Array as Array
import Data.Int as Int
import Data.Number as Number
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
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
import MLogo.WebApp.Parts as Parts
import MLogo.WebApp.Utils (classes)

type Input = VisibleState

component ∷ ∀ m o q. MonadAff m ⇒ Component q Input o m
component = Hooks.component \_ { pointer, screen } → Hooks.do
  zoom /\ zoomId ← Hooks.useState $ maxZoom / 2

  let
    handleZoomIn = when
      (zoom < maxZoom)
      (Hooks.modify_ zoomId (_ + zoomStep))

    handleZoomOut = when
      (zoom > minZoom)
      (Hooks.modify_ zoomId (_ - zoomStep))

    canvasSize = Int.toNumber canvasBaseSize
      * (Number.pow 2.0 (Int.toNumber $ maxZoom - zoom))

    transforms =
      [ Translate (canvasSize / 2.0) (canvasSize / 2.0)
      , Scale one (-one)
      ]

    renderZoomScale = HH.input
      [ HP.disabled true
      , HP.max $ Int.toNumber maxZoom
      , HP.min $ Int.toNumber minZoom
      , HP.step $ Step $ Int.toNumber zoomStep
      , HP.type_ InputRange
      , HP.value $ show zoom
      ]

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
      , renderZoomScale
      , renderZoomButton "mdi-plus" handleZoomIn
      ]

    renderZoomButton iconName clickHandler = HH.button
      [ HE.onClick \_ → clickHandler
      , classes [ "button", "square" ]
      ]
      [ Parts.icon iconName ]

  Hooks.pure do
    HH.div
      [ classes
          [ "has-background-grey-lighter"
          , "is-align-items-center"
          , "is-justify-content-center"
          , "is-flex"
          , "is-full-height"
          , "is-full-width"
          , "is-relative"
          ]
      ]
      [ SE.svg
          [ SA.viewBox zero zero canvasSize canvasSize
          , SA.classes
              [ ClassName "has-background-white"
              , ClassName "is-full-width"
              , ClassName "is-square"
              ]
          ]
          (renderScreen screen <> renderPointer pointer)
      , renderZoomPanel
      ]

pointerBaseSize ∷ Int
pointerBaseSize = 16

canvasBaseSize ∷ Int
canvasBaseSize = 128

maxZoom ∷ Int
maxZoom = 4

minZoom ∷ Int
minZoom = 1

zoomStep ∷ Int
zoomStep = 1
