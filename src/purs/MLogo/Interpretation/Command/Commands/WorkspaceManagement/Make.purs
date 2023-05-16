module MLogo.Interpretation.Command.Commands.WorkspaceManagement.Make
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
  { make: command }

command ∷ Command
command =
  let
    inputParser = ado
      name ← Types.fixedWordInputParser "name"
      value ← Types.fixedAnyInputParser "value"
      in { name, value }
  in
    Command
      { description: "Set a global variable value."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpret
      , name: "make"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m { name ∷ String, value ∷ Value }
interpret { name, value } = pure Nothing <* modify_ \st → st
  { globalVariables = Map.insert
      name
      value
      st.globalVariables
  }
