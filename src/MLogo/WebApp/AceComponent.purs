module MLogo.WebApp.AceComponent (Output(..), Query(..), component) where

import Prelude

import Ace as Ace
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Types (Editor)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

type Slot = H.Slot Query Output

data Query a = ChangeText String a

data Output = TextChanged String

data Action
  = Initialize
  | Finalize
  | HandleChange

type State = { editor ∷ Maybe Editor }

component ∷ ∀ i m. MonadAff m ⇒ H.Component Query i Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Initialize
        , finalize = Just Finalize
        }
    }

initialState ∷ ∀ i. i → State
initialState _ = { editor: Nothing }

render ∷ ∀ m. State → H.ComponentHTML Action () m
render = const $ HH.div
  [ HP.id "editor"
  , HP.ref (H.RefLabel "ace")
  ]
  []

handleAction
  ∷ ∀ m. MonadAff m ⇒ Action → H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize → do
    H.getHTMLElementRef (H.RefLabel "ace") >>= traverse_ \element → do
      editor ← H.liftEffect $ Ace.editNode element Ace.ace
      session ← H.liftEffect $ Editor.getSession editor
      H.modify_ (_ { editor = Just editor })
      { emitter, listener } ← H.liftEffect HS.create
      void $ H.subscribe emitter
      H.liftEffect $ Session.onChange session
        (\_ → HS.notify listener HandleChange)
  Finalize → do
    H.modify_ (_ { editor = Nothing })
  HandleChange → do
    H.gets _.editor >>= traverse_ \editor → do
      text ← H.liftEffect (Editor.getValue editor)
      H.raise $ TextChanged text

handleQuery
  ∷ ∀ m a
  . MonadAff m
  ⇒ Query a
  → H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  ChangeText text next → do
    maybeEditor ← H.gets _.editor
    case maybeEditor of
      Nothing → pure unit
      Just editor → do
        current ← H.liftEffect $ Editor.getValue editor
        when (text /= current) do
          void $ H.liftEffect $ Editor.setValue text Nothing editor
    H.raise $ TextChanged text
    pure (Just next)

