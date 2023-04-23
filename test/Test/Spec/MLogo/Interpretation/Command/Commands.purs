module Test.Spec.MLogo.Interpretation.Command.Commands (spec) where

import Prelude

import Control.Monad.Except (class MonadError)
import Control.Monad.State (class MonadState)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Data.Tuple.Nested (type (/\), (/\))
import MLogo.Interpretation.Command (Command(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.Interpret as Interpret
import MLogo.Interpretation.State
  ( ExecutionState(..)
  , Line
  , Position(..)
  , Value(..)
  )
import MLogo.Interpretation.State as State
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.MLogo.Interpretation.Command.Commands.Arithmetic as Arithmetic
import Test.Spec.MLogo.Interpretation.Command.Commands.Graphics as Graphics
import Test.Spec.MLogo.Interpretation.Command.Commands.WorkspaceManagement as WorkspaceManagement
import Test.Spec.QuickCheck (quickCheck)

spec ∷ Spec Unit
spec = describe "Commands" do
  Arithmetic.spec
  Graphics.spec
  WorkspaceManagement.spec
