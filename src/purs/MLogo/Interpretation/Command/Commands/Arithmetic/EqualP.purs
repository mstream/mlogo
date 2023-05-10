module MLogo.Interpretation.Command.Commands.Arithmetic.EqualP
  ( command
  , commandsByAlias
  , interpret
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Heterogeneous.Folding as Heterogeneous
import MLogo.Interpretation.Command (Command(..), ToMap(..))
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.Types (ValueType(..))
import MLogo.Interpretation.Types as Types

commandsByAlias ∷ Map String Command
commandsByAlias = Heterogeneous.hfoldlWithIndex
  ToMap
  (Map.empty ∷ Map String Command)
  { equalp: command }

command ∷ Command
command =
  let
    inputParser = Types.variableAnyInputParser "value"
  in
    Command
      { description: "Tells if values are equal."
      , interpret: \values →
          case Types.runVariableInputParser inputParser values of
            Left errorMessage →
              throwError errorMessage
            Right input →
              interpret input
      , name: "equalp"
      , outputValueType: Just BooleanType
      , parameters: Types.parametersFromVariableInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m (List Value)
interpret = pure <<< Just <<< BooleanValue <<< go true Nothing
  where
  go ∷ Boolean → (Maybe Value) → (List Value) → Boolean
  go acc mbLast = case _ of
    Nil →
      acc
    v : vs →
      let
        continue = go acc (Just v) vs
      in
        case mbLast of
          Nothing →
            continue
          Just last →
            if v == last then
              continue
            else
              false

