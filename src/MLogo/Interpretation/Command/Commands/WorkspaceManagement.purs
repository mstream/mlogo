module MLogo.Interpretation.Command.Commands.WorkspaceManagement
  ( commandsByAlias
  , repCount
  , variableAssignment
  ) where

import Prelude

import Control.Monad.State (get, modify_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Heterogeneous.Folding as Heterogeneous
import MLogo.Interpretation.Command (Command(..), ToMap(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.State (ExecutionState(..), Value(..))
import MLogo.Interpretation.Types (ValueType(..))
import MLogo.Interpretation.Types as Types

commandsByAlias ∷ Map String Command
commandsByAlias = Heterogeneous.hfoldlWithIndex
  ToMap
  (Map.empty ∷ Map String Command)
  { make: variableAssignment
  , repcount: repCount
  }

variableAssignment ∷ Command
variableAssignment =
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
          interpretVariableAssignment
      , name: "make"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

repCount ∷ Command
repCount =
  let
    inputParser = Types.fixedNoInputParser
  in
    Command
      { description:
          "outputs the repetition count of the innermost current REPEAT or FOREVER, starting from 1"
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpretRepCount
      , name: "repcount"
      , outputValueType: Just IntegerType
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpretVariableAssignment
  ∷ ∀ m. Interpret m { name ∷ String, value ∷ Value }
interpretVariableAssignment { name, value } = do
  modify_ \(ExecutionState state) →
    ExecutionState $ state
      { globalVariables = Map.insert name value state.globalVariables }
  pure Nothing

interpretRepCount ∷ ∀ m. Interpret m Unit
interpretRepCount _ = do
  (ExecutionState state) ← get
  pure $ Just $ IntegerValue state.repCount

