module MLogo.Interpretation.Command.Commands.Graphics.Right
  ( command
  , commandsByAlias
  , interpret
  ) where

import Prelude

import Control.Monad.State (gets)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Heterogeneous.Folding as Heterogeneous
import MLogo.Interpretation.Command (Command(..), ToMap(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Command.Commands.Graphics.SetHeading as SetHeading
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.Types as Types

commandsByAlias ∷ Map String Command
commandsByAlias = Heterogeneous.hfoldlWithIndex
  ToMap
  (Map.empty ∷ Map String Command)
  { right: command, rt: command }

command ∷ Command
command =
  let
    inputParser = Types.fixedNumberInputParser "steps"
  in
    Command
      { description:
          "Rotate the cursor by the given angle clockwise."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpret
      , name: "right"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m Number
interpret angle = gets (unwrap <<< _.pointer.angle)
  >>= (SetHeading.interpret <<< add angle)
