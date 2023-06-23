module MLogo.WebApp.Components.Sandbox (Output(..), component) where

import Prelude

import Data.Either (Either(..), note)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks as ExtraHooks
import MLogo.Program as Program
import MLogo.WebApp.Components.Canvas as CanvasComponent
import MLogo.WebApp.Components.Editor as EditorComponent
import MLogo.WebApp.Components.Examples as ExamplesComponent
import MLogo.WebApp.Components.SideBar as SideBarComponent
import MLogo.WebApp.Parts (IconSize(..))
import MLogo.WebApp.Parts as Parts
import MLogo.WebApp.Utils (classes)
import Type.Proxy (Proxy(..))

type Input = { initialSource ∷ String, randomNumberSeed ∷ Int }

data Output = SourceChanged String

component ∷ ∀ m q. MonadAff m ⇒ Component q Input Output m
component = Hooks.component
  \{ outputToken, slotToken } { initialSource, randomNumberSeed } →
    Hooks.do
      mbAst /\ putMbAst ← ExtraHooks.usePutState $ Just Nil

      let
        handleEditorOutput ∷ EditorComponent.Output → HookM m Unit
        handleEditorOutput = case _ of
          EditorComponent.AstChanged { ast, source } → do
            putMbAst $ Just ast
            Hooks.raise outputToken (SourceChanged source)
          EditorComponent.SyntaxErrorDetected →
            putMbAst Nothing

        handleSideBarOutput ∷ ExamplesComponent.Output → HookM m Unit
        handleSideBarOutput = case _ of
          ExamplesComponent.SourceTryRequested exampleAst → do
            putMbAst $ Just exampleAst

            Hooks.tell
              slotToken
              (Proxy ∷ Proxy "editor")
              unit
              (EditorComponent.SetAst exampleAst)

        renderPrimaryColumn =
          let
            interpretationResult = note "Syntax Error" mbAst
              >>= Program.interpretAst randomNumberSeed

          in
            HH.div
              [ classes
                  [ "column"
                  , "is-7-desktop"
                  , "is-flex"
                  , "is-flex-direction-column"
                  ]
              ]
              [ case interpretationResult of
                  Left errorMessage →
                    HH.div
                      [ HPA.label "error message"
                      , classes [ "block", "error" ]
                      ]
                      [ Parts.icon "fas" "fa-times-circle" Small
                      , HH.text $ "Runtime Error: " <> errorMessage
                      ]
                  Right visibleState →
                    HH.div
                      [ HP.id "canvas"
                      , HPA.label "canvas"
                      , classes [ "block", "has-bezels" ]
                      ]
                      [ HH.slot_
                          (Proxy ∷ Proxy "canvas")
                          unit
                          CanvasComponent.component
                          visibleState
                      ]
              , HH.div
                  [ HP.id "editor"
                  , HPA.label "editor"
                  , classes [ "block", "has-bezels", "is-full-height" ]
                  ]
                  [ HH.slot
                      (Proxy ∷ Proxy "editor")
                      unit
                      EditorComponent.component
                      unit
                      handleEditorOutput
                  ]
              ]

        renderSecondaryColumn = HH.div
          [ classes [ "column", "is-5-desktop" ] ]
          [ HH.div
              [ HP.id "side-bar"
              , HPA.label "help"
              , classes [ "is-full-height" ]
              ]
              [ HH.slot
                  (Proxy ∷ Proxy "sideBar")
                  unit
                  SideBarComponent.component
                  unit
                  handleSideBarOutput
              ]
          ]

      Hooks.useLifecycleEffect do
        Hooks.tell
          slotToken
          (Proxy ∷ Proxy "editor")
          unit
          (EditorComponent.SetSource initialSource)

        pure Nothing

      Hooks.pure do
        HH.div
          [ classes
              [ "columns"
              , "is-1"
              , "is-full-height"
              , "is-mobile"
              , "is-overflow-y-scroll"
              , "p-4"
              ]
          ]
          [ HH.div
              [ classes
                  [ "column"
                  , "columns"
                  , "is-desktop"
                  ]
              ]
              [ renderPrimaryColumn
              , renderSecondaryColumn
              ]
          , HH.aside
              [ classes
                  [ "column"
                  , "is-hidden-desktop"
                  , "is-narrow"
                  , "is-sticky"
                  , "menu"
                  ]
              ]
              [ HH.p
                  [ classes [ "menu-label" ] ]
                  [ HH.text "Sandbox" ]
              , HH.ul
                  [ classes [ "menu-list" ] ]
                  [ HH.li_
                      [ HH.a
                          [ HP.href "#canvas" ]
                          [ HH.text "Canvas" ]
                      ]
                  , HH.li_
                      [ HH.a
                          [ HP.href "#editor" ]
                          [ HH.text "Editor" ]
                      ]
                  , HH.li_
                      [ HH.a
                          [ HP.href "#side-bar" ]
                          [ HH.text "Help" ]
                      ]
                  ]
              ]
          ]

