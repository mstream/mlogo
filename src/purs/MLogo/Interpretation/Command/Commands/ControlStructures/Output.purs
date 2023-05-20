module MLogo.Interpretation.Command.Commands.ControlStructures.Output
  ( command
  , commandsByAlias
  , interpret
  ) where

import Prelude

import Control.Monad.State (modify_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Heterogeneous.Folding as Heterogeneous
import MLogo.Interpretation.Command (Command(..), ToMap(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.State (Value)
import MLogo.Interpretation.Types as Types

commandsByAlias ∷ Map String Command
commandsByAlias = Heterogeneous.hfoldlWithIndex
  ToMap
  (Map.empty ∷ Map String Command)
  { output: command }

command ∷ Command
command =
  let
    inputParser = Types.fixedAnyInputParser "value"
  in
    Command
      { description:
          "Outputs a given value to the context in which it was invoked in."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpret
      , name: "output"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m Value
interpret value = pure Nothing <* modify_ \st → st
  { outputtedValue = Just value }
