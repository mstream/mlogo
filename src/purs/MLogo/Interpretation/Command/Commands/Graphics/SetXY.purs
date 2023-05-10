module MLogo.Interpretation.Command.Commands.Graphics.SetXY
  ( command
  , commandsByAlias
  , interpret
  ) where

import Prelude

import Control.Monad.State (modify_)
import Data.List ((:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Heterogeneous.Folding as Heterogeneous
import MLogo.Interpretation.Command (Command(..), ToMap(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.State (ExecutionState(..), Position(..))
import MLogo.Interpretation.Types as Types

commandsByAlias ∷ Map String Command
commandsByAlias = Heterogeneous.hfoldlWithIndex
  ToMap
  (Map.empty ∷ Map String Command)
  { setxy: command }

command ∷ Command
command =
  let
    inputParser = ado
      x ← Types.fixedNumberInputParser "x-coordinate"
      y ← Types.fixedNumberInputParser "y-coordinate"
      in Position { x, y }
  in
    Command
      { description:
          "Moves the cursor to an absolute position in the graphics window. The two inputs are numbers, the X and Y coordinates."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpret
      , name: "setxy"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m Position
interpret target = pure Nothing <* do
  modify_ $ over ExecutionState
    ( \st → st
        { pointer = st.pointer { position = target }
        , screen =
            if st.pointer.isDown then
              { p1: st.pointer.position
              , p2: target
              } : st.screen
            else st.screen
        }
    )

