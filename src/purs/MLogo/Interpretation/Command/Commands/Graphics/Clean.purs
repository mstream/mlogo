module MLogo.Interpretation.Command.Commands.Graphics.Clean
  ( command
  , commandsByAlias
  , interpret
  ) where

import Prelude

import Control.Monad.State (modify_)
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Heterogeneous.Folding as Heterogeneous
import MLogo.Interpretation.Command (Command(..), ToMap(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.State (ExecutionState(..))
import MLogo.Interpretation.Types as Types

commandsByAlias ∷ Map String Command
commandsByAlias = Heterogeneous.hfoldlWithIndex
  ToMap
  (Map.empty ∷ Map String Command)
  { clean: command }

command ∷ Command
command =
  let
    inputParser = Types.fixedNoInputParser
  in
    Command
      { description: "Clear the drawing area."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpret
      , name: "clean"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m Unit
interpret _ = pure Nothing <* do
  modify_ $ over ExecutionState _ { screen = Nil }
