module MLogo.WebApp.EditorComponent (Output(..), Query(..), component) where

import Prelude

import Ace as Ace
import Ace.EditSession as Session
import Ace.Editor as Editor
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (info)
import Effect.Exception (throw)
import Halogen (RefLabel(..), SubscriptionId)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Halogen.Subscription as HS

data Output = TextChanged String

data Query a = ChangeText String a

component ∷ ∀ i m. MonadAff m ⇒ H.Component Query i Output m
component = Hooks.component \{ outputToken, queryToken } _ → Hooks.do
  editor /\ editorId ← Hooks.useState Nothing

  let
    handleChange = do
      mbEd ← Hooks.get editorId
      case mbEd of
        Just ed → do
          text ← liftEffect (Editor.getValue ed)
          Hooks.raise outputToken (TextChanged text)
        Nothing →
          liftEffect $ throw "editor has not been initialized yet"

    subscribeToSessionChanges ∷ HookM m Unit → HookM m SubscriptionId
    subscribeToSessionChanges cb = do
      mbAceElement ← Hooks.getHTMLElementRef aceRefLabel

      case mbAceElement of
        Just aceElement → do
          ed ← liftEffect $ Ace.editNode aceElement Ace.ace
          session ← liftEffect $ Editor.getSession ed
          Hooks.put editorId (Just ed)
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
      Hooks.put editorId Nothing
      Hooks.unsubscribe subscriptionId

  Hooks.useQuery queryToken case _ of
    ChangeText text next → do
      case editor of
        Nothing →
          pure unit
        Just ed → do
          current ← liftEffect $ Editor.getValue ed
          when (text /= current) do
            void $ liftEffect $ Editor.setValue text Nothing ed
      Hooks.raise outputToken (TextChanged text)
      pure (Just next)

  Hooks.pure do
    HH.div
      [ HP.id "editor"
      , HP.ref aceRefLabel
      ]
      []

aceRefLabel ∷ RefLabel
aceRefLabel = RefLabel "ace"
