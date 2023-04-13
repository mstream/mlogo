module MLogo.Interpretation.Interpret (Interpret, runInterpret) where

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.State.Class (class MonadState)
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import MLogo.Interpretation.State (ExecutionState, Value)

type Interpret m i =
  MonadError String m
  ⇒ MonadState ExecutionState m
  ⇒ i
  → m (Maybe Value)

runInterpret
  ∷ ∀ i
  . (i → StateT ExecutionState (Except String) (Maybe Value))
  → ExecutionState
  → i
  → String \/ (Maybe Value /\ ExecutionState)
runInterpret computation initialState input =
  runExcept (runStateT (computation input) initialState)

