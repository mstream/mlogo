module MLogo.WebApp.ExamplesComponent (Output(..), component) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Examples (Example(..))
import Halogen (Component)
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import MLogo.Printing as Printing

type Input = Map String Example
data Output = SourceTryRequested String

component ∷ ∀ m q. MonadAff m ⇒ Component q Input Output m
component = Hooks.component \{ outputToken } examplesByTitle → Hooks.do
  let
    handleTryButtonClick = Hooks.raise outputToken
      <<< SourceTryRequested

    renderExample (title /\ Example { ast }) =
      let
        source = Printing.printExpressions ast
      in
        HH.div
          [ HP.classes [ ClassName "example-entry" ] ]
          [ HH.div
              [ HP.classes [ ClassName "example-header" ] ]
              [ HH.button
                  [ HE.onClick \_ → handleTryButtonClick source ]
                  [ HH.text "try" ]
              , HH.h3_
                  [ HH.text title ]
              ]
          , HH.div_
              [ HH.code_
                  [ HH.text source ]
              ]
          ]

  Hooks.pure do
    HH.div_ (renderExample <$> Map.toUnfoldable examplesByTitle)

