module Test.Spec.MLogo.Interpretation.Command
  ( commandTestCase
  , interpretCommand
  ) where

import Prelude

import Control.Monad.Except (class MonadError)
import Control.Monad.State (class MonadState)
import Data.Either.Nested (type (\/))
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import MLogo.Interpretation.Command (Command(..))
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State (ExecutionState, Value)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

commandTestCase
  ∷ Command
  → String
  → ExecutionState
  → Array Value
  → String \/ (Maybe Value /\ ExecutionState)
  → Spec Unit
commandTestCase
  command@(Command { name })
  title
  state
  arguments
  expected =
  it
    ("interprets \"" <> name <> "\" command: " <> title)
    ( ( Interpret.runInterpret
          interpretCommand
          state
          { arguments: List.fromFoldable arguments, command }
      )
        `shouldEqual` expected
    )

interpretCommand
  ∷ ∀ m
  . MonadError String m
  ⇒ MonadState ExecutionState m
  ⇒ Interpret m { arguments ∷ List Value, command ∷ Command }
interpretCommand { arguments, command: (Command { interpret }) } =
  interpret arguments
