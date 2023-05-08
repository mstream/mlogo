module MLogo.WebApp.ExamplesComponent (Output(..), component) where

import Prelude

import Data.List (List)
import Data.List as List
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
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import MLogo.Parsing.Expression (Expression)
import MLogo.Printing as Printing

type Input = Map String Example

data Output = SourceTryRequested (List Expression)

component ∷ ∀ m q. MonadAff m ⇒ Component q Input Output m
component = Hooks.component \{ outputToken } examplesByTitle → Hooks.do
  let
    handleTryButtonClick ∷ List Expression → HookM m Unit
    handleTryButtonClick =
      Hooks.raise outputToken <<< SourceTryRequested

    renderExample (title /\ Example { ast }) =
      let
        source = Printing.codeToString
          $ Printing.printExpressions ast 50
      in
        HH.div
          [ HP.classes
              [ ClassName "block"
              , ClassName "is-flex"
              , ClassName "is-flex-direction-column"
              ]
          ]
          [ HH.div
              [ HP.classes
                  [ ClassName "is-flex"
                  , ClassName "is-flex-direction-row"
                  ]
              ]
              [ HH.button
                  [ HE.onClick \_ →
                      handleTryButtonClick $ List.fromFoldable ast
                  , HP.classes [ ClassName "button" ]
                  ]
                  [ HH.span
                      [ HP.classes
                          [ ClassName "icon", ClassName "is-small" ]
                      ]
                      [ HH.i
                          [ HP.classes
                              [ ClassName "aria-hidden"
                              , ClassName "mdi"
                              , ClassName "mdi-play-circle"
                              , ClassName "mr-1"
                              ]
                          ]
                          []
                      ]
                  , HH.span_ [ HH.text "try" ]
                  ]
              , HH.h3
                  [ HP.classes [ ClassName "is-3", ClassName "title" ] ]
                  [ HH.text title ]
              ]
          , HH.div_
              [ HH.pre_
                  [ HH.code_
                      [ HH.text source ]
                  ]
              ]
          ]

  Hooks.pure do
    HH.div
      [ HP.classes
          [ ClassName "is-flex", ClassName "is-flex-direction-column" ]
      ]
      (renderExample <$> Map.toUnfoldable examplesByTitle)

