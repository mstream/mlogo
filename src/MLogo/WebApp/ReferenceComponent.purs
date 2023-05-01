module MLogo.WebApp.ReferenceComponent (component) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen (Component)
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import MLogo.Interpretation.Command (Command(..))
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.Types (Parameters(..), ValueType)
import MLogo.Interpretation.Types as Types
import Test.QuickCheck.Gen as Gen

type Input = Map String (Map String Command)

type Entry =
  { description ∷ String
  , exampleArgs ∷ Array Value
  , outputValueType ∷ Maybe ValueType
  , parameters ∷ Parameters
  }

component ∷ ∀ m o q. MonadAff m ⇒ Component q Input o m
component = Hooks.component \_ commandsByAliasByCategory → Hooks.do
  entriesByAliasByCategory /\ entriesByAliasByCategoryId ←
    Hooks.useState Map.empty

  Hooks.useLifecycleEffect do
    entries ← liftEffect $ generateEntries commandsByAliasByCategory
    Hooks.put entriesByAliasByCategoryId entries
    pure Nothing

  let
    renderCategory (name /\ entries) =
      HH.div
        [ HP.classes
            [ ClassName "is-flex"
            , ClassName "is-flex-direction-column"
            , ClassName "mb-6"
            ]
        ]
        [ HH.h3
            [ HP.classes [ ClassName "is-3", ClassName "title" ] ]
            [ HH.text name ]
        , HH.div_ (renderEntry <$> Map.toUnfoldable entries)
        ]

    renderEntry
      ( name /\
          { description, exampleArgs, outputValueType, parameters }
      ) =
      HH.div
        [ HP.classes
            [ ClassName "block"
            , ClassName "is-flex"
            , ClassName "is-flex-direction-column"
            ]
        ]
        [ HH.hr_
        , HH.code
            [ HP.classes
                [ ClassName "is-family-code"
                , ClassName "is-flex"
                , ClassName "is-flex-direction-row"
                , ClassName "is-flex-wrap-wrap"
                , ClassName "is-size-5"
                ]
            ]
            ( [ HH.span
                  [ HP.classes [ ClassName "has-text-weight-bold" ] ]
                  [ HH.text name ]
              ] <> renderParameters <> renderOutputType
            )
        , HH.div_ [ HH.text description ]
        , HH.div_
            [ HH.hr_
            , HH.h4
                [ HP.classes [ ClassName "is-4", ClassName "title" ] ]
                [ HH.text "Examples" ]
            , HH.code_ $ [ HH.text name, HH.text " " ] <>
                renderExampleArgs
            , HH.hr_
            ]
        ]
      where
      renderOutputType = case outputValueType of
        Just ovt →
          [ HH.span
              [ HP.classes
                  [ ClassName "has-text-weight-semibold"
                  , ClassName "mx-4"
                  ]
              ]
              [ HH.text "→" ]
          , renderValueType ovt
          ]
        Nothing →
          []

      renderParameters = case parameters of
        FixedParameters ps →
          renderParameter <$> ps
        VariableParameters p →
          [ renderParameter p, HH.text "..." ]

      renderExampleArgs = Array.intersperse
        (HH.span_ [ HH.text " " ])
        (renderValue <$> exampleArgs)

    renderParameter { name, valueType } =
      HH.span_
        [ HH.span
            [ HP.classes
                [ ClassName "has-text-weight-semibold"
                , ClassName "ml-4"
                ]
            ]
            [ HH.text $ ":" <> name ]
        , renderValueType valueType
        ]

    renderValueType valueType = HH.span
      [ HP.classes
          [ ClassName "has-text-grey"
          , ClassName "has-text-weight-light"
          , ClassName "is-italic"
          , ClassName "is-size-7"
          ]
      ]
      [ HH.text $ "(" <> show valueType <> ")"
      ]

    renderValue = HH.text <<< case _ of
      BooleanValue b →
        show b
      FloatValue x →
        show x
      IntegerValue n →
        show n
      WordValue s →
        "\"" <> s

  Hooks.pure do
    HH.div
      [ HP.classes [ ClassName "body" ] ]
      (renderCategory <$> Map.toUnfoldable entriesByAliasByCategory)

generateEntries
  ∷ Map String (Map String Command)
  → Effect (Map String (Map String Entry))
generateEntries = traverse categoriesToEntries
  where
  categoriesToEntries ∷ Map String Command → Effect (Map String Entry)
  categoriesToEntries = traverse commandToEntry

  commandToEntry ∷ Command → Effect Entry
  commandToEntry (Command { description, outputValueType, parameters }) =
    do
      exampleArgs ← liftEffect $ Gen.randomSampleOne $
        Types.generateValuesFromParameters parameters
      pure { description, exampleArgs, outputValueType, parameters }

