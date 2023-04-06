module MLogo.Interpretation (run) where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Either.Nested (type (\/))
import Data.Foldable (foldM, foldl)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import MLogo.Interpretation.State (ExecutionState)
import MLogo.Interpretation.State as State
import MLogo.Interpretation.Statement as Statement
import MLogo.Parsing
  ( Expression(..)
  , Parameter(..)
  , ProcedureCall(..)
  , Statement(..)
  )

run ∷ List Statement → String \/ ExecutionState
run = foldM Statement.interpret State.initialExecutionState

