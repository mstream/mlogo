module MLogo.Interpretation.Command.Commands.Graphics.SetHeading
  ( command
  , commandsByAlias
  , interpret
  ) where

import Prelude

import Control.Monad.RWS (modify_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Heterogeneous.Folding as Heterogeneous
import MLogo.Interpretation.Command (Command(..), ToMap(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.State (Angle(..), ExecutionState(..))
import MLogo.Interpretation.Types as Types

commandsByAlias ∷ Map String Command
commandsByAlias = Heterogeneous.hfoldlWithIndex
  ToMap
  (Map.empty ∷ Map String Command)
  { seth: command, setheading: command }

command ∷ Command
command =
  let
    inputParser = Types.fixedNumberInputParser "angle"
  in
    Command
      { description: "Rotate the cursor to the specified heading"
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpret
      , name: "setheading"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m Number
interpret angle = pure Nothing <* do
  modify_ $ over ExecutionState
    ( \st → st
        { pointer = st.pointer
            { angle = Angle angle }
        }
    )

