module MLogo.WebApp.ExamplesComponent (Output(..), component) where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Examples (Example(..))
import Halogen (Component)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import MLogo.Parsing.Expression (Expression)
import MLogo.Printing as Printing
import MLogo.Printing.Code as Code
import MLogo.Webapp.Utils (classes)

type Input = Map String Example

data Output = SourceTryRequested (List Expression)

component ∷ ∀ m q. MonadAff m ⇒ Component q Input Output m
component = Hooks.component \{ outputToken } examplesByTitle → Hooks.do
  let
    handleTryButtonClick ∷ List Expression → HookM m Unit
    handleTryButtonClick = Hooks.raise outputToken
      <<< SourceTryRequested

    renderExample ∷ ∀ w. String /\ Example → HTML w (HookM m Unit)
    renderExample (title /\ Example { ast }) =
      let
        source ∷ String
        source = Code.codeToString $ Printing.printExpressions ast 50
      in
        HH.div
          [ classes [ "block", "is-flex", "is-flex-direction-column" ] ]
          [ HH.div
              [ classes [ "is-flex", "is-flex-direction-row" ] ]
              [ HH.button
                  [ HE.onClick \_ →
                      handleTryButtonClick $ List.fromFoldable ast
                  , classes [ "button" ]
                  ]
                  [ HH.span
                      [ classes [ "icon", "is-small" ] ]
                      [ HH.i
                          [ classes
                              [ "aria-hidden"
                              , "mdi"
                              , "mdi-play-circle"
                              , "mr-1"
                              ]
                          ]
                          []
                      ]
                  , HH.span_ [ HH.text "try" ]
                  ]
              , HH.h3
                  [ classes [ "is-3", "title" ] ]
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
      [ classes [ "is-flex", "is-flex-direction-column" ] ]
      (renderExample <$> Map.toUnfoldable examplesByTitle)

