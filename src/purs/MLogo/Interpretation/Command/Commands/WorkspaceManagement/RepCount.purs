module MLogo.Interpretation.Command.Commands.WorkspaceManagement.RepCount
  ( command
  , commandsByAlias
  , interpret
  ) where

import Prelude

import Control.Monad.State (get)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
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
  { repcount: command }

command ∷ Command
command =
  let
    inputParser = Types.fixedNoInputParser
  in
    Command
      { description:
          "outputs the repetition count of the innermost current REPEAT or FOREVER, starting from 1"
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          ( const $ (Just <<< IntegerValue <<< _.repCount)
              <$> unwrap
              <$> get
          )
      , name: "repcount"
      , outputValueType: Just IntegerType
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m Unit
interpret _ = (Just <<< IntegerValue <<< _.repCount)
  <$> unwrap
  <$> get
