module MLogo.Interpretation.Command.Commands.Graphics.Back
  ( command
  , commandsByAlias
  , interpret
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Heterogeneous.Folding as Heterogeneous
import MLogo.Interpretation.Command (Command(..), ToMap(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Command.Commands.Graphics.Forward as Forward
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.Types as Types

commandsByAlias ∷ Map String Command
commandsByAlias = Heterogeneous.hfoldlWithIndex
  ToMap
  (Map.empty ∷ Map String Command)
  { back: command, bk: command }

command ∷ Command
command =
  let
    inputParser = Types.fixedNumberInputParser "steps"
  in
    Command
      { description:
          "Move the cursor backward by the given amount of steps."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpret
      , name: "back"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m Number
interpret steps = Forward.interpret (-steps)

