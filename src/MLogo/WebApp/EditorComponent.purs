module MLogo.WebApp.EditorComponent (Output(..), Query(..), component) where

import Prelude

import Ace as Ace
import Ace.EditSession as Session
import Ace.Editor as Editor
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
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
import Halogen.Subscription as HS
import MLogo.Parsing.Expression (Expression)
import MLogo.Printing as Printing
import MLogo.Printing.Code as Code
import MLogo.Program as Program
import MLogo.Webapp.Utils (classes)
import Parsing as P

data Output
  = AstChanged ({ ast ∷ List Expression, source ∷ String })
  | SyntaxErrorDetected String

data Query a = SetAst (List Expression) a | SetSource String a

data SourceInfo
  = Parsable
      { ast ∷ List Expression
      , formatted ∷ String
      , unformatted ∷ String
      }
  | Unparsable String

component ∷ ∀ i m. MonadAff m ⇒ H.Component Query i Output m
component = Hooks.component \{ outputToken, queryToken } _ → Hooks.do
  mbEditor /\ mbEditorId ← Hooks.useState Nothing
  sourceInfo /\ sourceInfoId ← Hooks.useState $ Unparsable ""

  let
    getSource ∷ HookM m String
    getSource = Hooks.get mbEditorId >>= liftEffect <<< case _ of
      Nothing →
        throw "editor not initialized"
      Just editor →
        Editor.getValue editor

    updateSource ∷ String → HookM m Unit
    updateSource newSource = case mbEditor of
      Nothing →
        pure unit
      Just editor → do
        currentSource ← getSource
        when
          (newSource /= currentSource)
          (void $ liftEffect $ Editor.setValue newSource Nothing editor)

    handleChange ∷ HookM m Unit
    handleChange = do
      source ← getSource
      case Program.parseExpressions source of
        Left parseError → do
          updateSource source
          Hooks.put sourceInfoId (Unparsable source)

          Hooks.raise
            outputToken
            (SyntaxErrorDetected $ P.parseErrorMessage parseError)

        Right ast → do
          let
            formattedSource = formatSource ast

          case sourceInfo of
            Parsable info →
              when
                (ast /= info.ast)
                (Hooks.raise outputToken (AstChanged { ast, source }))
            Unparsable _ →
              Hooks.raise outputToken (AstChanged { ast, source })

          Hooks.put
            sourceInfoId
            ( Parsable
                { ast, formatted: formattedSource, unformatted: source }
            )

    handleFormatButtonClick ∷ HookM m Unit
    handleFormatButtonClick = do
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
          ed ← liftEffect $ Ace.editNode aceElement Ace.ace
          session ← liftEffect $ Editor.getSession ed
          Hooks.put mbEditorId (Just ed)
          { emitter, listener } ← liftEffect HS.create
          subscriptionId ← Hooks.subscribe emitter
          liftEffect $ Session.onChange
            session
            (\_ → HS.notify listener cb)

          pure subscriptionId

        Nothing →
          liftEffect $ throw "ace element not found"

  Hooks.useLifecycleEffect do
    subscriptionId ← subscribeToSessionChanges handleChange

    pure $ Just do
      Hooks.put mbEditorId Nothing
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
      [ classes [ "body", "is-flex", "is-flex-direction-column" ] ]
      [ HH.div
          [ classes [ "is-flex", "is-flex-direction-row" ] ]
          [ HH.button
              [ HE.onClick \_ → handleFormatButtonClick
              , classes [ "button" ]
              , HP.disabled case sourceInfo of
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
      , HH.div
          [ HP.ref aceRefLabel ]
          []
      ]

aceRefLabel ∷ RefLabel
aceRefLabel = RefLabel "ace"

formatSource ∷ List Expression → String
formatSource ast = Code.codeToString $ Printing.printExpressions ast 50

