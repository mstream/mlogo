module MLogo.Interpretation.Command.Commands.Graphics.Forward
  ( command
  , commandsByAlias
  , interpret
  ) where

import Prelude

import Control.Monad.State (get)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Heterogeneous.Folding as Heterogeneous
import MLogo.Interpretation.Command (Command(..), ToMap(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Command.Commands.Graphics.SetXY as SetXY
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.State as State
import MLogo.Interpretation.Types as Types

commandsByAlias ∷ Map String Command
commandsByAlias = Heterogeneous.hfoldlWithIndex
  ToMap
  (Map.empty ∷ Map String Command)
  { fd: command, forward: command }

command ∷ Command
command =
  let
    inputParser = Types.fixedNumberInputParser "steps"
  in
    Command
      { description:
          "Move the cursor forward by the given amount of steps."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpret
      , name: "forward"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m Number
interpret steps = do
  state ← get

  let
    radians = State.toRadians state.pointer.angle
    target =
      { x: state.pointer.position.x + steps * Number.sin radians
      , y: state.pointer.position.y + steps * Number.cos radians
      }

  SetXY.interpret target

