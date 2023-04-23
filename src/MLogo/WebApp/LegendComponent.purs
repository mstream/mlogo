module MLogo.WebApp.LegendComponent where

import Prelude

import Control.Monad.State (get, modify_)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen as H
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import MLogo.Interpretation.Command (Command(..))
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.Types
  ( Parameter
  , Parameters(..)
  , ValueType
  )
import MLogo.Interpretation.Types as Types
import Test.QuickCheck.Gen as Gen

data Action = Initialize | Receive Input

type Input = Map String Command

type State =
  { commandsByAlias ∷ Map String Command
  , entries ∷ Map String Entry
  }

type Entry =
  { description ∷ String
  , exampleArgs ∷ Array Value
  , outputValueType ∷ Maybe ValueType
  , parameters ∷ Parameters
  }

component ∷ ∀ m o q. MonadAff m ⇒ Component q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

initialState ∷ Input → State
initialState commandsByAlias = { commandsByAlias, entries: Map.empty }

render ∷ ∀ m. State → ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ ClassName "legend" ] ]
    (renderEntry <$> Map.toUnfoldable state.entries)

renderEntry ∷ ∀ i w. String /\ Entry → HTML w i
renderEntry
  (name /\ { description, exampleArgs, outputValueType, parameters }) =
  HH.div
    [ HP.classes [ ClassName "legend-entry" ] ]
    [ HH.div
        [ HP.classes [ ClassName "command-header" ] ]
        ( [ HH.text name
          , HH.text " "
          ] <> renderParameters <> renderOutputType
        )
    , HH.div_ [ HH.text description ]
    , HH.div_
        [ HH.hr_
        , HH.header_ [ HH.text "Examples" ]
        , HH.p_ $ [ HH.text name, HH.text " " ] <> renderExampleArgs
        , HH.hr_
        ]
    ]
  where
  renderOutputType = case outputValueType of
    Just ovt →
      [ HH.span_ [ HH.text " -> " ], renderValueType ovt ]
    Nothing →
      []

  renderParameters = case parameters of
    FixedParameters ps →
      Array.intersperse
        (HH.span_ [ HH.text " " ])
        (renderParameter <$> ps)
    VariableParameters p →
      [ renderParameter p, HH.text "..." ]

  renderExampleArgs = Array.intersperse
    (HH.span_ [ HH.text " " ])
    (renderValue <$> exampleArgs)

renderParameter ∷ ∀ i w. Parameter → HTML w i
renderParameter { name, valueType } =
  HH.span_
    [ HH.text ":"
    , HH.span
        [ HP.classes [ ClassName "command-header" ] ]
        [ HH.text name ]
    , renderValueType valueType
    ]

renderValueType ∷ ∀ i w. ValueType → HTML w i
renderValueType valueType = HH.span
  [ HP.classes [ ClassName "parameter-value-type" ] ]
  [ HH.text $ "(" <> show valueType <> ")"
  ]

renderValue ∷ ∀ i w. Value → HTML w i
renderValue = HH.text <<< case _ of
  BooleanValue b →
    show b
  FloatValue x →
    show x
  IntegerValue n →
    show n
  WordValue s →
    "\"" <> s

handleAction
  ∷ ∀ m o. MonadAff m ⇒ Action → HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize → do
    let
      f (Command { description, outputValueType, parameters }) = do
        exampleArgs ← liftEffect
          $ Gen.randomSampleOne
          $ Types.generateValuesFromParameters parameters
        pure { description, exampleArgs, outputValueType, parameters }

    state ← get
    entries ← traverse f state.commandsByAlias
    modify_ _ { entries = entries }

  Receive commandsByAlias → do
    modify_ _ { commandsByAlias = commandsByAlias }
    handleAction Initialize
