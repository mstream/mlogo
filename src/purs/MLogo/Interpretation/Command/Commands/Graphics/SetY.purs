module MLogo.Interpretation.Command.Commands.Graphics.SetY
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
import Heterogeneous.Folding as Heterogeneous
import MLogo.Interpretation.Command (Command(..), ToMap(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.Types as Types

commandsByAlias ∷ Map String Command
commandsByAlias = Heterogeneous.hfoldlWithIndex
  ToMap
  (Map.empty ∷ Map String Command)
  { sety: command }

command ∷ Command
command =
  let
    inputParser = Types.fixedNumberInputParser "y-coordinate"
  in
    Command
      { description: "Sets cursor's y coordinate."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpret
      , name: "sety"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m Number
interpret y = pure Nothing <* modify_ \st → st
  { pointer = st.pointer { position = st.pointer.position { y = y } }
  , screen =
      if st.pointer.isDown then
        { p1: st.pointer.position
        , p2: st.pointer.position { y = y }
        } : st.screen
      else st.screen
  }

