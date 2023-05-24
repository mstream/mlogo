module MLogo.Interpretation.Command.Commands.Graphics.ShowTurtle
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
import MLogo.Interpretation.Types as Types

commandsByAlias ∷ Map String Command
commandsByAlias = Heterogeneous.hfoldlWithIndex
  ToMap
  (Map.empty ∷ Map String Command)
  { showturtle: command, st: command }

command ∷ Command
command =
  let
    inputParser = Types.fixedNoInputParser
  in
    Command
      { description: "Make the cursor visible."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpret
      , name: "showturtle"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m Unit
interpret _ = pure Nothing <* modify_ \st → st
  { pointer = st.pointer { isVisible = true } }
