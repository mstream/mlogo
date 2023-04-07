module MLogo.Interpretation (run) where

import Data.Either.Nested (type (\/))
import Data.Foldable (foldM)
import Data.List (List)
import MLogo.Interpretation.State (ExecutionState)
import MLogo.Interpretation.State as State
import MLogo.Interpretation.Statement as Statement
import MLogo.Parsing (Statement)

run ∷ List Statement → String \/ ExecutionState
run = foldM Statement.interpret State.initialExecutionState

