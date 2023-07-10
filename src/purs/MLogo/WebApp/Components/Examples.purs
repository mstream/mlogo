module MLogo.WebApp.Components.Examples (Output(..), component) where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import MLogo.Program.Example (Example(..))
import Halogen (Component)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import MLogo.Parsing.Expression (Expression)
import MLogo.Printing as Printing
import MLogo.Printing.Code as Code
import MLogo.WebApp.Parts (IconSize(..))
import MLogo.WebApp.Parts as Parts
import MLogo.WebApp.Utils (classes)

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
        source = Code.codeToString
          $ Printing.printExpressions
              ast
              { pageWidth: 50, simplifyBinaryOperations: false }
      in
        HH.div
          [ HPA.role "figure"
          , classes [ "block", "is-flex", "is-flex-direction-column" ]
          ]
          [ HH.div
              [ classes [ "is-flex", "is-flex-direction-row" ] ]
              [ HH.button
                  [ HE.onClick \_ →
                      handleTryButtonClick $ List.fromFoldable ast
                  , classes [ "button" ]
                  ]
                  [ Parts.icon "fas" "fa-play-circle" Small
                  , HH.span
                      [ classes [ "mr-1" ] ]
                      [ HH.text "try" ]
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
      [ HP.id "examples"
      , classes [ "is-flex", "is-flex-direction-column" ]
      ]
      (renderExample <$> Map.toUnfoldable examplesByTitle)

