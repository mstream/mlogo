module MLogo.WebApp.EditorComponent (Output(..), Query(..), component) where

import Prelude

import Ace (Editor)
import Ace as Ace
import Ace.EditSession as Session
import Ace.Editor as Editor
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (info)
import Effect.Exception (throw)
import Halogen (RefLabel(..), SubscriptionId)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks as ExtraHooks
import Halogen.Subscription as HS
import MLogo.Parsing.Expression (Expression)
import MLogo.Printing as Printing
import MLogo.Printing.Code as Code
import MLogo.Program as Program
import MLogo.Webapp.Utils (classes)
import Parsing (ParseError(..), Position(..))

data Output
  = AstChanged { ast ∷ List Expression, source ∷ String }
  | SyntaxErrorDetected

data Query a
  = SetAst (List Expression) a
  | SetSource String a

data SourceInfo
  = Parsable ParsableSourceInfo
  | Unparsable UnparsableSourceInfo

type ParsableSourceInfo =
  { ast ∷ List Expression
  , formatted ∷ String
  , unformatted ∷ String
  }

type UnparsableSourceInfo = { line ∷ Int, reason ∷ String }

component ∷ ∀ i m. MonadAff m ⇒ H.Component Query i Output m
component = Hooks.component \{ outputToken, queryToken } _ → Hooks.do
  _mbEditor /\ mbEditorId ← Hooks.useState Nothing
  _sourceInfo /\ sourceInfoId ← Hooks.useState
    $ Parsable { ast: Nil, formatted: "", unformatted: "" }

  getMbEditor ← ExtraHooks.useGet _mbEditor
  getSourceInfo ← ExtraHooks.useGet _sourceInfo

  let
    putMbEditor = Hooks.put mbEditorId
    putSourceInfo = Hooks.put sourceInfoId

    inEditor ∷ ∀ a. (Editor → Effect a) → HookM m a
    inEditor f = getMbEditor >>= liftEffect <<< case _ of
      Nothing →
        throw "editor not initialized"
      Just editor →
        f editor

    getSource ∷ HookM m String
    getSource = inEditor Editor.getValue

    updateSyntaxError ∷ Maybe UnparsableSourceInfo → HookM m Unit
    updateSyntaxError mbUnparsableSourceInfo = inEditor \editor → do
      info $ "updating syntax error: " <> show mbUnparsableSourceInfo
      session ← Editor.getSession editor
      case mbUnparsableSourceInfo of
        Just { line, reason } →
          Session.setAnnotations
            [ { column: 0
              , row: line - 1
              , text: reason
              , type: "error"
              }
            ]
            session
        Nothing →
          Session.clearAnnotations session

    updateSource ∷ String → HookM m Unit
    updateSource newSource = do
      currentSource ← getSource
      inEditor \editor → do
        when
          (newSource /= currentSource)
          (void $ Editor.setValue newSource Nothing editor)

    handleChange ∷ HookM m Unit
    handleChange = do
      source ← getSource
      case Program.parseExpressions source of
        Left (ParseError reason (Position { line })) → do
          let
            unparsableSourceInfo = { line, reason }
          updateSyntaxError (Just unparsableSourceInfo)
          putSourceInfo $ Unparsable unparsableSourceInfo
          Hooks.raise outputToken SyntaxErrorDetected

        Right ast → do
          let
            formattedSource = formatSource ast

          sourceInfo ← getSourceInfo
          case sourceInfo of
            Parsable info →
              when
                (ast /= info.ast)
                (Hooks.raise outputToken (AstChanged { ast, source }))
            Unparsable _ → do
              updateSyntaxError Nothing
              Hooks.raise outputToken (AstChanged { ast, source })

          putSourceInfo $ Parsable
            { ast, formatted: formattedSource, unformatted: source }

    handleFormatButtonClick ∷ HookM m Unit
    handleFormatButtonClick = do
      sourceInfo ← getSourceInfo
      case sourceInfo of
        Parsable { formatted } → do
          info $ "updating source with formatted version: " <> formatted
          updateSource formatted
        Unparsable _ →
          liftEffect $ throw "source cannot be formatted"

    subscribeToSessionChanges ∷ HookM m Unit → HookM m SubscriptionId
    subscribeToSessionChanges cb = do
      mbAceElement ← Hooks.getHTMLElementRef aceRefLabel

      case mbAceElement of
        Just aceElement → do
          editor ← liftEffect $ Ace.editNode aceElement Ace.ace
          session ← liftEffect $ Editor.getSession editor
          putMbEditor $ Just editor
          { emitter, listener } ← liftEffect HS.create
          subscriptionId ← Hooks.subscribe emitter
          liftEffect $ Session.onChange
            session
            (\_ → HS.notify listener cb)

          pure subscriptionId

        Nothing →
          liftEffect $ throw "ace element not found"

    renderToolPanel = HH.div
      [ classes [ "is-flex", "is-flex-direction-row" ] ]
      [ HH.button
          [ HE.onClick \_ → handleFormatButtonClick
          , classes [ "button" ]
          , HP.disabled case _sourceInfo of
              Parsable { formatted, unformatted } →
                formatted == unformatted
              Unparsable _ →
                true
          ]
          [ HH.span
              [ classes [ "icon", "is-small" ] ]
              [ HH.i
                  [ classes
                      [ "aria-hidden"
                      , "mdi"
                      , "mdi-format-align-left"
                      , "mr-1"
                      ]
                  ]
                  []
              ]
          , HH.span_ [ HH.text "format" ]
          ]
      ]

  Hooks.useLifecycleEffect do
    subscriptionId ← subscribeToSessionChanges handleChange

    pure $ Just do
      putMbEditor Nothing
      Hooks.unsubscribe subscriptionId

  Hooks.useQuery queryToken case _ of
    SetAst ast next → do
      updateSource $ formatSource ast
      pure $ Just next
    SetSource source next → do
      updateSource source
      pure $ Just next

  Hooks.pure do
    HH.div
      [ classes
          [ "is-flex"
          , "is-flex-direction-column"
          , "is-full-height"
          , "is-full-width"
          ]
      ]
      [ renderToolPanel
      , HH.div
          [ HP.ref aceRefLabel
          , classes [ "is-full-height", "is-full-width" ]
          ]
          []
      ]

aceRefLabel ∷ RefLabel
aceRefLabel = RefLabel "ace"

formatSource ∷ List Expression → String
formatSource ast = Code.codeToString $ Printing.printExpressions ast 50

