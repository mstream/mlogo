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
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(..))
import Halogen.Svg.Elements as SE
import MLogo.Interpretation.State (Angle(..), VisibleState)
import MLogo.Interpretation.State as State
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

    renderPointer { angle: Angle a, color, position } =
      let
        halfOfPointerSize = Int.toNumber pointerBaseSize / 2.0
        pl =
          { x: position.x - halfOfPointerSize
          , y: position.y - halfOfPointerSize
          }
        pr =
          { x: position.x + halfOfPointerSize
          , y: position.y - halfOfPointerSize
          }
        pt =
          { x: position.x
          , y: position.y + halfOfPointerSize
          }
        stroke = SA.stroke $ colorAttribute color
        strokeLineCap = SA.strokeLineCap LineCapRound
        strokeOpacity = SA.strokeOpacity 0.5
        transform = SA.transform
          $ transforms <> [ Rotate (-a) position.x position.y ]
        defaultAttrs =
          [ stroke, strokeLineCap, strokeOpacity, transform ]
      in
        [ SE.line $ defaultAttrs
            <>
              [ SA.x1 pt.x
              , SA.y1 pt.y
              , SA.x2 pl.x
              , SA.y2 pl.y
              ]
        , SE.line $ defaultAttrs
            <>
              [ SA.x1 pt.x
              , SA.y1 pt.y
              , SA.x2 pr.x
              , SA.y2 pr.y
              ]
        , SE.line $ defaultAttrs
            <>
              [ SA.x1 position.x
              , SA.y1 position.y
              , SA.x2 pl.x
              , SA.y2 pl.y
              ]
        , SE.line $ defaultAttrs
            <>
              [ SA.x1 position.x
              , SA.y1 position.y
              , SA.x2 pr.x
              , SA.y2 pr.y
              ]
        ]

    renderScreen = Array.fromFoldable <<< map \{ color, p1, p2 } →
      SE.line
        [ SA.x1 p1.x
        , SA.y1 p1.y
        , SA.x2 p2.x
        , SA.y2 p2.y
        , SA.stroke $ colorAttribute color
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

colorAttribute ∷ State.Color → Color
colorAttribute color =
  let
    scale = Int.round <<< (_ * 2.55) <<< Int.toNumber
    { blue, green, red } = State.toRGB color
  in
    RGB (scale red) (scale green) (scale blue)

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
