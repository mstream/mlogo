module MLogo.WebApp.Components.Reference (component) where

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
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks as ExtraHooks
import MLogo.Interpretation.Command (Command(..))
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.Types (Parameters(..), ValueType)
import MLogo.Interpretation.Types as Types
import MLogo.WebApp.Utils (classes)
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
  entriesByAliasByCategory /\ putEntriesByAliasByCategory ←
    ExtraHooks.usePutState Map.empty

  Hooks.useLifecycleEffect do
    entries ← liftEffect $ generateEntries commandsByAliasByCategory
    putEntriesByAliasByCategory entries
    pure Nothing

  let
    renderCategory (name /\ entries) =
      HH.div
        [ classes [ "is-flex", "is-flex-direction-column", "mb-6" ] ]
        [ HH.h3
            [ classes [ "is-3", "title" ] ]
            [ HH.text name ]
        , HH.div_ (renderEntry <$> Map.toUnfoldable entries)
        ]

    renderEntry
      ( name /\
          { description, exampleArgs, outputValueType, parameters }
      ) =
      HH.div
        [ classes [ "block", "is-flex", "is-flex-direction-column" ] ]
        [ HH.hr_
        , HH.code
            [ classes
                [ "is-family-code"
                , "is-flex"
                , "is-flex-direction-row"
                , "is-flex-wrap-wrap"
                , "is-size-5"
                ]
            ]
            ( [ HH.span
                  [ classes [ "has-text-weight-bold" ] ]
                  [ HH.text name ]
              ] <> renderParameters <> renderOutputType
            )
        , HH.div_ [ HH.text description ]
        , HH.div_
            [ HH.hr_
            , HH.h4
                [ classes [ "is-4", "title" ] ]
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
              [ classes [ "has-text-weight-semibold", "mx-4" ] ]
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
            [ classes [ "has-text-weight-semibold", "ml-4" ] ]
            [ HH.text $ ":" <> name ]
        , renderValueType valueType
        ]

    renderValueType valueType = HH.span
      [ classes
          [ "has-text-grey"
          , "has-text-weight-light"
          , "is-italic"
          , "is-size-7"
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
      [ classes [ "is-full-width" ] ]
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

