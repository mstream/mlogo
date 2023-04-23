module MLogo.Interpretation.Command.Commands.Arithmetic.Sum
  ( command
  , commandsByAlias
  , interpret
  ) where

import Prelude

import Data.Foldable (foldl)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Heterogeneous.Folding as Heterogeneous
import MLogo.Interpretation.Command (Command(..), ToMap(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.Types (ValueType(..))
import MLogo.Interpretation.Types as Types

commandsByAlias ∷ Map String Command
commandsByAlias = Heterogeneous.hfoldlWithIndex
  ToMap
  (Map.empty ∷ Map String Command)
  { sum: command }

command ∷ Command
command =
  let
    inputParser = Types.variableNumberInputParser "addend"
  in
    Command
      { description: "Sums up given numbers."
      , interpret: Command.parseAndInterpretInput
          (Types.runVariableInputParser inputParser)
          interpret
      , name: "sum"
      , outputValueType: Just NumberType
      , parameters: Types.parametersFromVariableInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m (List Number)
interpret = pure <<< Just <<< FloatValue <<< foldl (+) zero
