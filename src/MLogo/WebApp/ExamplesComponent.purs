module MLogo.WebApp.ExamplesComponent (component) where

import Prelude

import Control.Monad.State (modify_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Examples (Example(..))
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen as H
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import MLogo.Parsing (Expression)
import MLogo.Printing as Printing

data Action = Receive Input

type Input = Map String Example

type State = { examplesByTitle ∷ Map String Example }

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
initialState examplesByTitle = { examplesByTitle }

render ∷ ∀ m. State → ComponentHTML Action () m
render state =
  HH.div_
    (renderExample <$> Map.toUnfoldable state.examplesByTitle)

renderExample ∷ ∀ i w. String /\ Example → HTML w i
renderExample (title /\ Example { ast }) =
  HH.div
    [ HP.classes [ ClassName "example-entry" ] ]
    [ HH.h3_ [ HH.text title ]
    , HH.div_ [ renderCode ast ]
    ]

renderCode ∷ ∀ i w. Array Expression → HTML w i
renderCode ast = HH.code_ [ HH.text $ Printing.printExpressions ast ]

handleAction
  ∷ ∀ m o. MonadAff m ⇒ Action → HalogenM State Action () o m Unit
handleAction = case _ of
  Receive examplesByTitle →
    modify_ _ { examplesByTitle = examplesByTitle }
