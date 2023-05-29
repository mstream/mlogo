module MLogo.WebApp.Components.Editor (Output(..), Query(..), component) where

import Prelude

import Ace (Editor)
import Ace as Ace
import Ace.EditSession as Session
import Ace.Editor as Editor
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
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
import MLogo.WebApp.Parts (IconSize(..))
import MLogo.WebApp.Parts as Parts
import MLogo.WebApp.Utils (classes)
import Parsing (ParseError(..), Position(..))

data Output
  = AstChanged { ast ∷ List Expression, source ∷ String }
  | SyntaxErrorDetected

data Query a
  = SetAst (List Expression) a
  | SetSource String a

type State = { editor ∷ Editor, sourceInfo ∷ SourceInfo }

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
  _mbState /\ _mbStateId ← Hooks.useState Nothing
  getMbState ← ExtraHooks.useGet _mbState

  let
    modifyMbState = Hooks.modify_ _mbStateId

    whenInitialized ∷ ∀ a. (State → HookM m a) → HookM m a
    whenInitialized f = getMbState >>= case _ of
      Nothing →
        liftEffect $ throw "state not initialized"
      Just state →
        f state

    getSource ∷ HookM m String
    getSource = whenInitialized \{ editor } →
      liftEffect $ Editor.getValue editor

    updateSyntaxError ∷ Maybe UnparsableSourceInfo → HookM m Unit
    updateSyntaxError mbUnparsableSourceInfo =
      whenInitialized \{ editor } → liftEffect do
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
      whenInitialized \{ editor } → liftEffect $ when
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
          modifyMbState $ map _
            { sourceInfo = Unparsable unparsableSourceInfo }
          Hooks.raise outputToken SyntaxErrorDetected

        Right ast → do
          let
            formattedSource = formatSource ast

          whenInitialized \{ sourceInfo } → case sourceInfo of
            Parsable { ast: currentAst } →
              when
                {- TODO: fix
                       this causes strange behaviour when the format
                       button is clicked
                -}
                (ast /= currentAst || true)
                (Hooks.raise outputToken (AstChanged { ast, source }))
            Unparsable _ → do
              updateSyntaxError Nothing
              Hooks.raise outputToken (AstChanged { ast, source })

          modifyMbState $ map _
            { sourceInfo = Parsable
                { ast
                , formatted: formattedSource
                , unformatted: source
                }
            }

    handleFormatButtonClick ∷ HookM m Unit
    handleFormatButtonClick = whenInitialized \{ sourceInfo } →
      case sourceInfo of
        Parsable { formatted } →
          updateSource formatted
        Unparsable _ →
          liftEffect $ throw "source cannot be formatted"

    subscribeToSessionChanges ∷ HookM m Unit → HookM m SubscriptionId
    subscribeToSessionChanges cb = do
      mbAceElement ← Hooks.getHTMLElementRef aceRefLabel

      case mbAceElement of
        Just aceElement → do
          editor ← liftEffect $ Ace.editNode aceElement Ace.ace
          source ← liftEffect $ Editor.getValue editor
          sourceInfo ← case Program.parseExpressions source of
            Left (ParseError reason (Position { line })) → do
              let
                unparsableSourceInfo = { line, reason }
              updateSyntaxError (Just unparsableSourceInfo)
              pure $ Unparsable unparsableSourceInfo
            Right ast →
              pure $ Parsable
                { ast
                , formatted: formatSource ast
                , unformatted: source
                }

          modifyMbState $ const $ Just { editor, sourceInfo }

          { emitter, listener } ← liftEffect HS.create
          subscriptionId ← Hooks.subscribe emitter
          session ← liftEffect $ Editor.getSession editor
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
          , HP.disabled case _.sourceInfo <$> _mbState of
              Just (Parsable { formatted, unformatted }) →
                formatted == unformatted
              Just (Unparsable _) →
                true
              Nothing →
                false
          ]
          [ Parts.icon "fas" "fa-align-left" Small
          , HH.span
              [ classes [ "ml-1" ] ]
              [ HH.text "format" ]
          ]
      ]

  Hooks.useLifecycleEffect do
    subscriptionId ← subscribeToSessionChanges handleChange

    pure $ Just do
      modifyMbState $ const Nothing
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
          [ "is-flex", "is-flex-direction-column", "is-full-height" ]
      ]
      [ renderToolPanel
      , HH.div
          [ HP.ref aceRefLabel
          , classes [ "is-full-height" ]
          ]
          []
      ]

aceRefLabel ∷ RefLabel
aceRefLabel = RefLabel "ace"

formatSource ∷ List Expression → String
formatSource ast = Code.codeToString
  $ Printing.printExpressions
      ast
      { pageWidth: 50, simplifyBinaryOperations: true }

