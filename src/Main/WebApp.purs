module Main.WebApp (main) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), Component)
import Halogen.Aff (selectElement)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Halogen.VDom.Driver (runUI)
import MLogo.Program as Program
import MLogo.WebApp.CanvasComponent as CanvasComponent
import MLogo.WebApp.EditorComponent (Output(..), Query(..))
import MLogo.WebApp.EditorComponent as EditorComponent
import MLogo.WebApp.ExamplesComponent (Output(..))
import MLogo.WebApp.ExamplesComponent as ExamplesComponent
import MLogo.WebApp.SideBarComponent as SideBarComponent
import Type.Proxy (Proxy(..))
import Web.DOM.ParentNode (QuerySelector(..))

main ∷ Effect Unit
main = launchAff_ do
  mbDivElem ← selectElement $ QuerySelector "#halogen"
  for_ mbDivElem \divElem → do
    void $ runUI rootComp 0 divElem

rootComp ∷ ∀ i m o q. MonadAff m ⇒ Component q i o m
rootComp = Hooks.component \{ slotToken } _ → Hooks.do
  astResult /\ astResultId ← Hooks.useState $ Right Nil
  astToggle /\ astToggleId ← Hooks.useState false

  let
    handleEditorOutput ∷ EditorComponent.Output → HookM m Unit
    handleEditorOutput = case _ of
      AstChanged newAst →
        Hooks.put astResultId (Right newAst)
      SyntaxErrorDetected errorMessage →
        Hooks.put astResultId (Left errorMessage)

    handleSideBarOutput ∷ ExamplesComponent.Output → HookM m Unit
    handleSideBarOutput (SourceTryRequested exampleAst) = do
      Hooks.put astResultId (Right exampleAst)

      Hooks.tell
        slotToken
        (Proxy ∷ Proxy "editor")
        unit
        (SetAst exampleAst)

    handleAstToggleClick ∷ HookM m Unit
    handleAstToggleClick = Hooks.modify_ astToggleId not

    renderRightColumn = HH.div
      [ HP.classes
          [ ClassName "column"
          , ClassName "is-6"
          ]
      ]
      [ HH.div
          [ HP.classes
              [ ClassName "is-flex"
              , ClassName "is-flex-direction-column"
              ]
          ]
          [ HH.button
              [ HE.onClick \_ → handleAstToggleClick
              , HP.classes [ ClassName "button" ]
              ]
              [ HH.text
                  if astToggle then "View drawing" else "View AST"
              ]
          , if astToggle then HH.text $ show astResult
            else case astResult >>= Program.interpretAst of
              Left errorMessage →
                HH.div
                  [ HP.classes [ ClassName "error" ] ]
                  [ HH.text errorMessage ]
              Right visibleState →
                HH.div
                  [ HP.id "canvas" ]
                  [ HH.slot_
                      (Proxy ∷ Proxy "canvas")
                      unit
                      CanvasComponent.component
                      visibleState
                  ]
          ]
      ]

  Hooks.pure do
    HH.div
      [ HP.classes
          [ ClassName "columns"
          , ClassName "is-gapless"
          ]
      , HP.id "container"
      ]
      [ HH.div
          [ HP.classes
              [ ClassName "column"
              , ClassName "is-6"
              ]
          ]
          [ HH.div
              [ HP.id "editor" ]
              [ HH.slot
                  (Proxy ∷ Proxy "editor")
                  unit
                  EditorComponent.component
                  unit
                  handleEditorOutput
              ]
          , HH.div
              [ HP.id "side-bar"
              ]
              [ HH.slot
                  (Proxy ∷ Proxy "sideBar")
                  unit
                  SideBarComponent.component
                  unit
                  handleSideBarOutput
              ]
          ]
      , renderRightColumn
      ]

